# Server-selected direct upload of a WSI file.

#' Upload a Whole Slide Image to the Strand AI Platform
#'
#' Follows the explicit server-selected upload transport:
#' \enumerate{
#'   \item `POST /api/v1/uploads` — create a session with `uploadTransport`
#'   \item Transfer GCS resumable chunks, a small S3 single PUT, or bounded SHA256 S3 multipart parts
#'   \item S3 automatically calls owned completion to start durable ingestion; GCS uses object-finalize
#' }
#' Both backends support the 50 GiB application cap. S3 multipart automatically
#' renews owned part URLs and completes the verified manifest. Permanent single
#' PUT also uses owned completion; no Content-Range
#' or API credentials are sent to S3.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param path Path to a local file (SVS / TIFF / NDPI / ...).
#' @param content_type Override the auto-detected MIME type.
#' @param chunk_size Bytes per chunk. Must be a positive multiple of 256 KiB
#'   (the GCS resumable upload requirement). Defaults to 8 MiB.
#' @param progress If `TRUE`, prints upload progress to the console.
#' @param auto_segment Opt out of automatic cell segmentation for this upload.
#'   `NULL` (default) uses the org's default; `FALSE` skips segmentation (the
#'   slide is still ingested and rendered); `TRUE` forces it on even when the
#'   org default is off.
#' @param mpp Optional user-reported microns per pixel, persisted when the upload
#'   is created. Must be greater than 0 and at most 100. This creation-time value
#'   takes precedence over embedded slide calibration.
#'
#' @return A list with class `strand_upload` containing `id`, `gcs_path`,
#'   `upload_url`, `upload_transport`, `width_px`, `height_px`, `status`.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' upload <- strand_upload_file(client, "slide.svs", progress = TRUE, mpp = 0.26)
#' }
#' @export
strand_upload_file <- function(client, path,
                               content_type = NULL,
                               chunk_size = 8L * 1024L * 1024L,
                               progress = FALSE,
                               auto_segment = NULL,
                               mpp = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("No such file: ", path, call. = FALSE)
  }
  if (!is.numeric(chunk_size) || chunk_size <= 0 ||
      chunk_size %% (256L * 1024L) != 0) {
    stop("chunk_size must be a positive multiple of 256 KiB.", call. = FALSE)
  }
  if (!is.null(auto_segment) &&
      (!is.logical(auto_segment) || length(auto_segment) != 1L || is.na(auto_segment))) {
    stop("auto_segment must be TRUE, FALSE, or NULL.", call. = FALSE)
  }

  size <- file.info(path)$size
  filename <- basename(path)
  ct <- content_type %||% strand_guess_content_type(path)

  init_body <- list(filename = filename, fileSize = size, contentType = ct)
  if (!is.null(auto_segment)) init_body$autoSegment <- auto_segment
  if (!is.null(mpp)) init_body$mpp <- strand_validate_mpp(mpp, "mpp")

  session <- strand_perform_json(
    client, "uploads", method = "POST",
    body = init_body
  )

  if (identical(session$uploadTransport, "gcs_resumable")) {
    strand_stream_to_gcs(session$uploadUrl, path, size, ct,
                         chunk_size = chunk_size, progress = progress)
  } else if (identical(session$uploadTransport, "s3_presigned_put")) {
    strand_stream_to_s3(session$uploadUrl, path, size, ct, progress = FALSE)
    completed <- strand_perform_json(client, sprintf("uploads/%s/complete", session$uploadId), method = "POST")
    if (!identical(completed$uploadId, session$uploadId) || !strand_valid_completion_generation(completed$generation)) stop("Owned upload completion is unverified", call. = FALSE)
    if (isTRUE(progress)) cat(sprintf("Uploaded %s / %s bytes\\n", strand_content_length(size), strand_content_length(size)))
  } else if (identical(session$uploadTransport, "s3_multipart")) {
    strand_stream_multipart(client, session, path, size, progress)
  } else {
    stop("Upload session requires an explicit supported uploadTransport.", call. = FALSE)
  }

  upload <- strand_wait_for_ingest_start(client, session$uploadId)
  upload$upload_url <- session$uploadUrl
  upload$upload_transport <- session$uploadTransport
  upload$gcs_path <- session$gcsPath
  upload
}

# Wait for object-finalize delivery to move the row out of client-owned
# `uploading` after the storage PUT completes. This bounded poll prevents
# immediate predict calls from racing the ingest transition.
strand_wait_for_ingest_start <- function(client, upload_id,
                                         timeout_sec = 120,
                                         poll_interval_sec = 0.5) {
  deadline <- Sys.time() + timeout_sec
  repeat {
    upload <- strand_parse_upload_row(
      strand_perform_json(client, sprintf("uploads/%s", upload_id))
    )
    if (identical(upload$status, "upload_failed")) {
      stop(
        "Storage finalized an object whose size did not match the declared upload; ",
        "create a new upload session and send the complete file.",
        call. = FALSE
      )
    }
    if (!identical(upload$status, "uploading")) return(upload)
    if (Sys.time() >= deadline) {
      stop(
        "The object upload completed, but automatic ingest did not start within 120 seconds.",
        call. = FALSE
      )
    }
    Sys.sleep(poll_interval_sec)
  }
}

#' @export
print.strand_upload <- function(x, ...) {
  cat("<strand_upload>\n")
  cat("  id:     ", x$id, "\n", sep = "")
  if (!is.null(x$filename)) cat("  file:   ", x$filename, "\n", sep = "")
  if (!is.null(x$status))   cat("  status: ", x$status, "\n", sep = "")
  if (!is.null(x$width_px) && !is.null(x$height_px)) {
    cat("  size:   ", x$width_px, "x", x$height_px, "\n", sep = "")
  }
  invisible(x)
}

# HTTP Content-Length is decimal digits, never R's scientific notation.
strand_content_length <- function(size) sprintf("%.0f", size)
strand_valid_completion_generation <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value) && value != "null"
}

strand_stream_multipart <- function(client, session, path, size, progress) {
  geometry <- session$multipart
  part_size <- geometry$partSizeBytes
  count <- geometry$partCount
  if (is.null(part_size) || !(part_size %in% c(8 * 1024^2, 64 * 1024^2)) ||
      is.null(count) || count != ceiling(size / part_size) || count < 1 || count > 800 ||
      !identical(geometry$checksumAlgorithm, "SHA256")) {
    stop("Invalid server multipart geometry", call. = FALSE)
  }
  before <- file.info(path)
  input <- file(path, "rb")
  on.exit(close(input), add = TRUE)
  manifest <- vector("list", count)
  sent <- 0
  for (number in seq_len(count)) {
    buf <- readBin(input, "raw", n = min(part_size, size - sent))
    if (length(buf) != min(part_size, size - sent)) stop("Local multipart source changed", call. = FALSE)
    checksum <- openssl::base64_encode(openssl::sha256(buf))
    renewed <- strand_perform_json(client, sprintf("uploads/%s", session$uploadId), method = "POST",
      body = list(parts = list(list(partNumber = number, checksumSha256 = checksum))))
    if (!identical(renewed$uploadTransport, "s3_multipart") ||
        !identical(renewed$multipart, geometry)) stop("Multipart authority changed", call. = FALSE)
    existing <- Filter(function(p) p$partNumber == number, renewed$uploadedParts)
    if (length(existing)) {
      if (existing[[1]]$checksumSha256 != checksum || existing[[1]]$size != length(buf))
        stop("Existing multipart part differs from local source", call. = FALSE)
      etag <- existing[[1]]$etag
    } else {
      part <- renewed$parts[[1]]
      if (length(renewed$parts) != 1L || part$partNumber != number ||
          part$headers$`Content-Length` != strand_content_length(length(buf)) ||
          part$headers$`x-amz-checksum-sha256` != checksum) stop("Invalid multipart part receipt", call. = FALSE)
      req <- httr2::req_method(httr2::request(part$uploadUrl), "PUT")
      req <- httr2::req_body_raw(req, buf)
      req <- httr2::req_headers(req, `Content-Length` = strand_content_length(length(buf)),
                                `x-amz-checksum-sha256` = checksum)
      req <- httr2::req_error(req, is_error = function(resp) FALSE)
      response <- httr2::req_perform(req)
      etag <- httr2::resp_header(response, "etag")
      if (httr2::resp_status(response) != 200L || is.null(etag)) stop("S3 multipart part rejected", call. = FALSE)
    }
    manifest[[number]] <- list(partNumber = number, etag = etag, checksumSha256 = checksum)
    sent <- sent + length(buf)
    after <- file.info(path)
    if (before$size != after$size || before$mtime != after$mtime) stop("Local multipart source changed", call. = FALSE)
    if (isTRUE(progress) && sent < size) cat(sprintf("\\rUploaded %s / %s bytes", strand_content_length(sent), strand_content_length(size)))
  }
  completed <- strand_perform_json(client, sprintf("uploads/%s/complete", session$uploadId),
                                  method = "POST", body = list(parts = manifest))
  if (!identical(completed$uploadId, session$uploadId) || !strand_valid_completion_generation(completed$generation))
    stop("Multipart completion is unverified", call. = FALSE)
  if (isTRUE(progress)) cat(sprintf("\\rUploaded %s / %s bytes\\n", strand_content_length(size), strand_content_length(size)))
}

# S3 PutObject is a single streamed request: no GCS Content-Range or API auth.
strand_stream_to_s3 <- function(upload_url, path, size, content_type, progress) {
  req <- httr2::request(upload_url)
  req <- httr2::req_method(req, "PUT")
  req <- httr2::req_body_file(req, path, type = content_type)
  req <- httr2::req_headers(req, `Content-Length` = strand_content_length(size))
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (!(status %in% c(200L, 201L))) stop("S3 rejected object upload: HTTP ", status, call. = FALSE)
  if (isTRUE(progress)) message(sprintf("  uploaded %.1f / %.1f MB (100%%)", size / 1e6, size / 1e6))
}

# Chunked PUT to the GCS resumable session URL.
strand_stream_to_gcs <- function(upload_url, path, size, content_type,
                                  chunk_size, progress) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  pos <- 0
  while (pos < size) {
    take <- min(chunk_size, size - pos)
    buf <- readBin(con, what = "raw", n = take)
    if (length(buf) == 0L) break
    end_byte <- pos + length(buf) - 1L
    range_hdr <- sprintf("bytes %d-%d/%d", pos, end_byte, size)

    req <- httr2::request(upload_url)
    req <- httr2::req_method(req, "PUT")
    req <- httr2::req_headers(req,
                              `Content-Length` = strand_content_length(length(buf)),
                              `Content-Range` = range_hdr,
                              `Content-Type` = content_type)
    req <- httr2::req_body_raw(req, buf, type = content_type)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- httr2::req_perform(req)

    is_final <- (pos + length(buf)) >= size
    status <- httr2::resp_status(resp)
    if (is_final) {
      if (!(status %in% c(200L, 201L))) {
        stop("GCS rejected final chunk: HTTP ", status, call. = FALSE)
      }
    } else {
      if (status != 308L) {
        stop("GCS rejected chunk: HTTP ", status, call. = FALSE)
      }
    }
    pos <- pos + length(buf)
    if (isTRUE(progress)) {
      message(sprintf("  uploaded %.1f / %.1f MB (%.0f%%)",
                      pos / 1e6, size / 1e6, 100 * pos / size))
    }
  }
}

#' List uploads for the calling organization
#'
#' Fetches one page of uploads, newest-first. Filters out archived uploads;
#' use [strand_restore()] to bring archived rows back into the active set.
#'
#' @param client A `strand_client`.
#' @param limit Page size (1-200). Defaults to 100.
#' @param cursor Opaque cursor from a prior response's `next_cursor`. `NULL`
#'   (default) fetches the first page.
#'
#' @return A list with class `strand_upload_list` containing:
#'   * `uploads`: list of `strand_upload` entries (each carries `id`,
#'     `filename`, `file_size`, `status`, `gcs_path`, `width_px`, `height_px`,
#'     `created_at`).
#'   * `next_cursor`: cursor for the next page, or `NULL` on the last page.
#'
#' @examples
#' \dontrun{
#' page <- strand_uploads_list(client, limit = 20)
#' for (u in page$uploads) message(u$id, " ", u$filename)
#' }
#' @export
strand_uploads_list <- function(client, limit = 100L, cursor = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  if (!is.numeric(limit) || length(limit) != 1L || limit <= 0) {
    stop("limit must be a positive integer", call. = FALSE)
  }
  query <- list(limit = as.integer(limit))
  if (!is.null(cursor)) {
    if (!is.character(cursor) || length(cursor) != 1L) {
      stop("cursor must be a single string", call. = FALSE)
    }
    query$cursor <- cursor
  }
  raw <- strand_perform_json(client, "uploads", query = query)
  items <- raw$uploads %||% list()
  structure(
    list(
      uploads = lapply(items, strand_parse_upload_row),
      next_cursor = raw$nextCursor
    ),
    class = "strand_upload_list"
  )
}

#' Fetch a single upload by id
#'
#' @param client A `strand_client`.
#' @param upload_id Upload identifier (UUID).
#'
#' @return A `strand_upload` list as documented in [strand_uploads_list()].
#'   Raises `strand_not_found_error` if no upload with that id exists for the
#'   calling org.
#'
#' @examples
#' \dontrun{
#' u <- strand_uploads_get(client, "11111111-1111-1111-1111-111111111111")
#' }
#' @export
strand_uploads_get <- function(client, upload_id) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  if (!is.character(upload_id) || length(upload_id) != 1L || !nzchar(upload_id)) {
    stop("upload_id must be a non-empty string", call. = FALSE)
  }
  raw <- strand_perform_json(client, sprintf("uploads/%s", upload_id))
  strand_parse_upload_row(raw)
}

#' @export
print.strand_upload_list <- function(x, ...) {
  n <- length(x$uploads)
  cat("<strand_upload_list> ", n, " upload", if (n == 1L) "" else "s",
      if (!is.null(x$next_cursor)) " (more pages available)" else "",
      "\n", sep = "")
  for (u in x$uploads) {
    cat("  ", u$id, "  ", u$status %||% "?", "  ",
        u$filename %||% "<no name>", "\n", sep = "")
  }
  invisible(x)
}

# Parse a row from GET /uploads or GET /uploads/{id} into a strand_upload.
# fileSize is sent as a string (postgres bigint → JSON string); coerce to numeric.
# widthPx/heightPx may be missing (null) on uploads that haven't finished
# the dimensions probe yet — normalize to NULL rather than NA / empty list.
strand_parse_upload_row <- function(raw) {
  file_size <- raw$fileSize
  if (!is.null(file_size)) {
    coerced <- suppressWarnings(as.numeric(file_size))
    file_size <- if (is.na(coerced)) NULL else coerced
  }
  structure(
    list(
      id = strand_as_scalar_chr(raw$id),
      filename = strand_as_scalar_chr(raw$filename),
      file_size = file_size,
      status = strand_as_scalar_chr(raw$status),
      gcs_path = strand_as_scalar_chr(raw$gcsPath),
      width_px = strand_as_scalar_int(raw$widthPx),
      height_px = strand_as_scalar_int(raw$heightPx),
      auto_segment = strand_as_scalar_lgl(raw$autoSegment),
      created_at = strand_as_scalar_chr(raw$createdAt)
    ),
    class = "strand_upload"
  )
}

strand_as_scalar_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (length(x) > 1L) return(NULL)
  as.character(x)
}

strand_as_scalar_int <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (!is.numeric(x[[1L]])) return(NULL)
  as.integer(x[[1L]])
}

strand_as_scalar_lgl <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NULL)
  if (!is.logical(x[[1L]])) return(NULL)
  as.logical(x[[1L]])
}

strand_guess_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
         svs  = "image/aperio-svs",
         tiff = "image/tiff",
         tif  = "image/tiff",
         ndpi = "image/ndpi",
         scn  = "image/scn",
         mrxs = "image/mrxs",
         vsi  = "image/vsi",
         bif  = "image/bif",
         "application/octet-stream")
}

# Resumable upload of a WSI file.

#' Upload a Whole Slide Image to the Strand Platform
#'
#' Performs the three-step resumable upload flow:
#' \enumerate{
#'   \item `POST /api/v1/uploads` — create a resumable session
#'   \item `PUT <uploadUrl>` — stream the file to GCS in 8 MiB chunks
#'   \item `POST /api/v1/uploads/{id}/complete` — finalize, read slide dims
#' }
#'
#' @param client A `strand_client` from [strand_client()].
#' @param path Path to a local file (SVS / TIFF / NDPI / ...).
#' @param content_type Override the auto-detected MIME type.
#' @param chunk_size Bytes per chunk. Must be a positive multiple of 256 KiB
#'   (the GCS resumable upload requirement). Defaults to 8 MiB.
#' @param progress If `TRUE`, prints upload progress to the console.
#'
#' @return A list with class `strand_upload` containing `id`, `gcs_path`,
#'   `upload_url`, `width_px`, `height_px`, `status`.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' upload <- strand_upload_file(client, "slide.svs", progress = TRUE)
#' }
#' @export
strand_upload_file <- function(client, path,
                               content_type = NULL,
                               chunk_size = 8L * 1024L * 1024L,
                               progress = FALSE) {
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

  size <- file.info(path)$size
  filename <- basename(path)
  ct <- content_type %||% strand_guess_content_type(path)

  session <- strand_perform_json(
    client, "uploads", method = "POST",
    body = list(filename = filename, fileSize = size, contentType = ct)
  )

  strand_stream_to_gcs(session$uploadUrl, path, size, ct,
                        chunk_size = chunk_size, progress = progress)

  completion <- strand_perform_json(
    client, sprintf("uploads/%s/complete", session$uploadId),
    method = "POST"
  )

  structure(
    list(
      id = session$uploadId,
      gcs_path = session$gcsPath,
      upload_url = session$uploadUrl,
      width_px = completion$widthPx,
      height_px = completion$heightPx,
      status = completion$status
    ),
    class = "strand_upload"
  )
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
                              `Content-Length` = as.character(length(buf)),
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

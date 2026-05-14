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
  cat("  status: ", x$status, "\n", sep = "")
  cat("  size:   ", x$width_px, "x", x$height_px, "\n", sep = "")
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

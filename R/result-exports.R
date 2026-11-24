# Unified, format-driven result exports.

strand_check_job <- function(job) {
  if (!inherits(job, "strand_job")) {
    stop("job must be a strand_job (from strand_predict())", call. = FALSE)
  }
}

#' Request a format-driven result export
#'
#' Native OME-Zarr is returned without conversion. OME-Zarr ZIP and OME-TIFF
#' are asynchronous, cached exports. The response manifest explicitly records
#' H&E and segmentation/cell-expression contents.
#'
#' @param job A `strand_job`.
#' @param format One of `"ome-zarr"`, `"ome-zarr-zip"`, or `"ome-tiff"`.
#' @param include_he Include processed/registered H&E. Defaults to `FALSE` for every format.
#' @param include_segmentation Attach the latest segmentation artifact manifest.
#' @param include_original_upload Request the separate, potentially multi-GB source file stored by
#'   Strand, not processed/registered H&E. If ingest de-identification ran, this is the stored
#'   de-identified copy. Defaults to `FALSE`.
#' @return A result export manifest.
#' @export
strand_export_request <- function(job, format,
                                  include_he = FALSE,
                                  include_segmentation = FALSE,
                                  include_original_upload = FALSE) {
  strand_check_job(job)
  formats <- c("ome-zarr", "ome-zarr-zip", "ome-tiff")
  if (!is.character(format) || length(format) != 1L || !(format %in% formats)) {
    stop("format must be one of: ome-zarr, ome-zarr-zip, ome-tiff", call. = FALSE)
  }
  body <- list(
    format = format,
    includeHe = include_he,
    includeSegmentation = include_segmentation,
    includeOriginalUpload = include_original_upload
  )
  strand_perform_json(
    job$client,
    sprintf("jobs/%s/exports", job$id),
    method = "POST",
    body = body,
    expected = c(200L, 202L)
  )
}

#' Get format-driven result export status
#'
#' @inheritParams strand_export_request
#' @return A refreshed result export manifest.
#' @export
strand_export_get <- function(job, format,
                              include_he = FALSE,
                              include_segmentation = FALSE,
                              include_original_upload = FALSE) {
  strand_check_job(job)
  query <- list(
    format = format,
    includeHe = tolower(as.character(include_he)),
    includeSegmentation = tolower(as.character(include_segmentation)),
    includeOriginalUpload = tolower(as.character(include_original_upload))
  )
  strand_perform_json(
    job$client,
    sprintf("jobs/%s/exports", job$id),
    query = query,
    expected = c(200L, 202L)
  )
}

#' Download a generated result export
#'
#' @param job A `strand_job`.
#' @param format One of `"ome-zarr-zip"` or `"ome-tiff"`.
#' @param include_he Include processed/registered H&E.
#' @param include_segmentation Attach the latest segmentation artifact manifest.
#' @param path Destination file.
#' @param timeout Maximum seconds to wait; `Inf` waits indefinitely.
#' @param poll_interval Seconds between status requests.
#' @return Normalized destination path, invisibly.
#' @export
strand_export_download <- function(job, format, path,
                                   include_he = FALSE,
                                   include_segmentation = FALSE,
                                   timeout = Inf, poll_interval = 2) {
  if (!(format %in% c("ome-zarr-zip", "ome-tiff"))) {
    stop("download format must be ome-zarr-zip or ome-tiff", call. = FALSE)
  }
  export <- strand_export_request(
    job, format,
    include_he = include_he,
    include_segmentation = include_segmentation
  )
  deadline <- if (is.finite(timeout)) Sys.time() + timeout else NA
  while (!identical(export$status, "ready")) {
    if (identical(export$status, "failed")) {
      stop(export$error %||% sprintf("%s export failed", format), call. = FALSE)
    }
    if (!is.na(deadline) && Sys.time() >= deadline) {
      stop(sprintf("%s export was not ready within %s seconds", format, timeout), call. = FALSE)
    }
    Sys.sleep(poll_interval)
    export <- strand_export_get(
      job, format,
      include_he = include_he,
      include_segmentation = include_segmentation
    )
  }
  url <- export$artifacts$prediction$downloadUrl
  if (is.null(url)) stop("Ready export did not include a download URL", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  resp <- httr2::req_perform(httr2::req_error(httr2::request(url), is_error = function(resp) FALSE), path = path)
  if (httr2::resp_status(resp) >= 400L) stop("Result download failed", call. = FALSE)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

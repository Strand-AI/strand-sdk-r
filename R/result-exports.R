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
#' @param include_he Include source H&E. OME-TIFF always includes H&E.
#' @param include_segmentation Attach the latest segmentation artifact manifest.
#' @return A result export manifest.
#' @export
strand_export_request <- function(job, format,
                                  include_he = NULL,
                                  include_segmentation = FALSE) {
  strand_check_job(job)
  formats <- c("ome-zarr", "ome-zarr-zip", "ome-tiff")
  if (!is.character(format) || length(format) != 1L || !(format %in% formats)) {
    stop("format must be one of: ome-zarr, ome-zarr-zip, ome-tiff", call. = FALSE)
  }
  body <- list(format = format, includeSegmentation = include_segmentation)
  if (!is.null(include_he)) body$includeHe <- include_he
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
                              include_he = NULL,
                              include_segmentation = FALSE) {
  strand_check_job(job)
  query <- list(
    format = format,
    includeSegmentation = tolower(as.character(include_segmentation))
  )
  if (!is.null(include_he)) query$includeHe <- tolower(as.character(include_he))
  strand_perform_json(
    job$client,
    sprintf("jobs/%s/exports", job$id),
    query = query,
    expected = c(200L, 202L)
  )
}

#' Download a generated result export
#'
#' @inheritParams strand_export_request
#' @param path Destination file.
#' @param timeout Maximum seconds to wait; `Inf` waits indefinitely.
#' @param poll_interval Seconds between status requests.
#' @return Normalized destination path, invisibly.
#' @export
strand_export_download <- function(job, format, path,
                                   include_segmentation = FALSE,
                                   timeout = Inf, poll_interval = 2) {
  if (!(format %in% c("ome-zarr-zip", "ome-tiff"))) {
    stop("download format must be ome-zarr-zip or ome-tiff", call. = FALSE)
  }
  export <- strand_export_request(
    job, format,
    include_he = if (identical(format, "ome-tiff")) TRUE else NULL,
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
      include_he = if (identical(format, "ome-tiff")) TRUE else NULL,
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

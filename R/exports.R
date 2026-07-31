# Alternate-format exports for completed jobs.

strand_check_job <- function(job) {
  if (!inherits(job, "strand_job")) {
    stop("job must be a strand_job (from strand_predict())", call. = FALSE)
  }
}

strand_parse_ome_tiff_export <- function(raw) {
  structure(
    list(
      status = strand_as_scalar_chr(raw$status),
      format = strand_as_scalar_chr(raw$format),
      size_bytes = if (is.null(raw$sizeBytes)) NULL else as.numeric(raw$sizeBytes),
      download_url = strand_as_scalar_chr(raw$downloadUrl),
      download_url_expires_at = strand_as_scalar_chr(raw$downloadUrlExpiresAt),
      error = strand_as_scalar_chr(raw$error),
      updated_at = strand_as_scalar_chr(raw$updatedAt)
    ),
    class = "strand_ome_tiff_export"
  )
}

#' Request an OME-TIFF export
#'
#' Starts an asynchronous OME-TIFF export for a completed job. The request is
#' idempotent: repeated calls return the in-progress or cached export.
#'
#' @param job A `strand_job` from [strand_predict()].
#'
#' @return A `strand_ome_tiff_export` status list. `status` is one of
#'   `pending`, `running`, or `ready`; a ready response includes a signed
#'   `download_url`.
#'
#' @examples
#' \dontrun{
#' export <- strand_ome_tiff_request(job)
#' }
#' @export
strand_ome_tiff_request <- function(job) {
  strand_check_job(job)
  raw <- strand_perform_json(
    job$client,
    sprintf("jobs/%s/exports/ome-tiff", job$id),
    method = "POST",
    expected = c(200L, 202L)
  )
  strand_parse_ome_tiff_export(raw)
}

#' Get OME-TIFF export status
#'
#' @inheritParams strand_ome_tiff_request
#'
#' @return A `strand_ome_tiff_export` status list. When ready, the signed
#'   `download_url` is valid for one hour.
#'
#' @export
strand_ome_tiff_get <- function(job) {
  strand_check_job(job)
  raw <- strand_perform_json(
    job$client,
    sprintf("jobs/%s/exports/ome-tiff", job$id),
    expected = c(200L, 202L)
  )
  strand_parse_ome_tiff_export(raw)
}

#' Download a job result as OME-TIFF
#'
#' Requests an OME-TIFF export, polls until it is ready, then downloads the
#' file from the short-lived signed URL returned by the platform.
#'
#' @inheritParams strand_ome_tiff_request
#' @param path Destination file path. Parent directories are created.
#' @param timeout Max seconds to wait. `Inf` waits forever.
#' @param poll_interval Seconds between status requests.
#' @param progress If `TRUE`, reports status changes while waiting.
#'
#' @return The normalized destination path, invisibly.
#'
#' @examples
#' \dontrun{
#' strand_download_ome_tiff(job, "results.ome.tiff", progress = TRUE)
#' }
#' @export
strand_download_ome_tiff <- function(job, path, timeout = Inf,
                                     poll_interval = 2, progress = FALSE) {
  strand_check_job(job)
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("path must be a non-empty string", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || timeout < 0) {
    stop("timeout must be a non-negative number", call. = FALSE)
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1L ||
      poll_interval < 0) {
    stop("poll_interval must be a non-negative number", call. = FALSE)
  }

  deadline <- if (is.finite(timeout)) Sys.time() + timeout else NA
  export <- strand_ome_tiff_request(job)
  last_status <- NA_character_
  while (!identical(export$status, "ready")) {
    if (isTRUE(progress) && !identical(export$status, last_status)) {
      message("  OME-TIFF export: ", export$status)
      last_status <- export$status
    }
    if (!is.na(deadline) && Sys.time() >= deadline) {
      stop(sprintf("OME-TIFF export for job %s was not ready within %s seconds",
                   job$id, timeout), call. = FALSE)
    }
    Sys.sleep(poll_interval)
    export <- strand_ome_tiff_get(job)
  }

  if (is.null(export$download_url)) {
    stop("Ready OME-TIFF export did not include a download URL", call. = FALSE)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  req <- httr2::request(export$download_url)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req, path = path)
  status <- httr2::resp_status(resp)
  if (status >= 400L) {
    stop(sprintf("OME-TIFF download failed with HTTP %d", status), call. = FALSE)
  }
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

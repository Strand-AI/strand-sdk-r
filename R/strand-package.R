#' strandai: Client for the Strand Platform API
#'
#' R client for the Strand Platform REST API. Provides functions to upload
#' Whole Slide Images via GCS resumable upload, submit Lattice H&E to
#' multiplex protein inference jobs, and download OME-Zarr predictions as
#' SpatialExperiment objects.
#'
#' @section Quickstart:
#' ```
#' library(strandai)
#' client <- strand_client()  # reads STRAND_API_KEY
#' upload <- strand_upload_file(client, "slide.svs")
#' est <- strand_estimate(client, upload$id, c("CD3", "CD8"))
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"))
#' strand_job_wait(job)
#' spe <- strand_download_results(job)
#' ```
#'
#' @section Authentication:
#' Mint API keys at `https://app.strandai.com/settings/api-keys`. Pass with
#' `strand_client(api_key=...)` or set `STRAND_API_KEY` in the environment.
#'
#' @keywords internal
"_PACKAGE"

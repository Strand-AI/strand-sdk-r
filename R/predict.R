# Predict surface — estimate + submit.

#' Estimate credit cost for a prediction
#'
#' Calls `POST /api/v1/predict/estimate` to compute the credit cost for a
#' given upload + marker selection. Does not reserve credits.
#'
#' @param client A `strand_client`.
#' @param upload_id Upload identifier (from [strand_upload_file()]).
#' @param markers Character vector of marker names (e.g. `c("CD3", "CD8")`).
#'
#' @return A list with `patch_count`, `marker_count`, `estimated_credits`,
#'   `org_balance`, `org_pending`.
#'
#' @examples
#' \dontrun{
#' est <- strand_estimate(client, upload$id, c("CD3", "CD8", "Ki67"))
#' message("≈ ", est$estimated_credits, " credits")
#' }
#' @export
strand_estimate <- function(client, upload_id, markers) {
  markers <- strand_coerce_markers(markers)
  raw <- strand_perform_json(
    client, "predict/estimate", method = "POST",
    body = list(uploadId = upload_id, markers = markers)
  )
  list(
    patch_count = raw$patchCount,
    marker_count = raw$markerCount,
    estimated_credits = raw$estimatedCredits,
    org_balance = raw$orgBalance %||% 0L,
    org_pending = raw$orgPending %||% 0L
  )
}

#' Submit a prediction job
#'
#' Calls `POST /api/v1/predict`. Atomically reserves credits in the same
#' transaction; on insufficient balance raises a `strand_insufficient_credits_error`
#' condition with a `required` field.
#'
#' @inheritParams strand_estimate
#'
#' @return A `strand_job` list with `id`, `reserved_credits`, `client`.
#'
#' @examples
#' \dontrun{
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"))
#' strand_job_wait(job)
#' }
#' @export
strand_predict <- function(client, upload_id, markers) {
  markers <- strand_coerce_markers(markers)
  raw <- strand_perform_json(
    client, "predict", method = "POST",
    body = list(uploadId = upload_id, markers = markers),
    expected = 202L
  )
  structure(
    list(
      id = raw$jobId,
      reserved_credits = raw$reservedCredits %||% 0L,
      client = client
    ),
    class = "strand_job"
  )
}

strand_coerce_markers <- function(markers) {
  if (!is.character(markers)) {
    stop("markers must be a character vector", call. = FALSE)
  }
  trimmed <- trimws(markers)
  trimmed <- trimmed[nzchar(trimmed)]
  if (length(trimmed) == 0L) {
    stop("markers must contain at least one non-empty entry", call. = FALSE)
  }
  trimmed
}

#' Run the full prediction pipeline in one blocking call
#'
#' Orchestrates upload → submit → wait → (optional) download, returning when
#' the job reaches a terminal state. All sub-operations use the same exported
#' primitives ([strand_upload_file()], [strand_predict()], [strand_job_wait()],
#' [strand_download_results()]) so callers can drop down a level whenever they
#' need finer control.
#'
#' @param client A `strand_client`.
#' @param image_path Path to a local WSI file (SVS / TIFF / NDPI / ...).
#' @param markers Character vector of markers to predict (e.g.
#'   `c("HER2", "CD8", "PD1")`).
#' @param timeout_sec Max seconds to wait for the job to finish; defaults to 30
#'   minutes.
#' @param output_dir Optional output directory. When provided, the entire
#'   OME-Zarr result store is mirrored under this directory.
#' @param poll_interval_sec Seconds between status polls while waiting.
#' @param on_progress Optional callback `function(stage, fraction)` where
#'   `stage` is one of `"upload"`, `"submit"`, `"wait"`, `"download"`.
#'   `fraction` is always a numeric in `[0, 1]` — `0` at the start of each
#'   stage and `1` at its end, with intermediate values where the underlying
#'   step exposes progress.
#'
#' @return A list with class `strand_predict_result` containing:
#'   `job_id`, `status`, `credits_used`, `marker_outputs` (named list mapping
#'   marker name → on-disk directory when `output_dir` is provided, else an
#'   empty list), `output_dir`, and `job` (the underlying `strand_job` handle).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' result <- strand_run(
#'   client, "biopsy.ome.tiff",
#'   markers = c("HER2", "CD8", "PD1"),
#'   output_dir = "./outputs/"
#' )
#' cat("used", result$credits_used, "credits\n")
#'
#' # The S3 generic also works: predict(client, ...)
#' result <- predict(client, "biopsy.ome.tiff", markers = c("HER2"))
#' }
#' @export
strand_run <- function(client, image_path, markers,
                       timeout_sec = 1800,
                       output_dir = NULL,
                       poll_interval_sec = 5,
                       on_progress = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  validated_markers <- strand_coerce_markers(markers)
  if (!file.exists(image_path)) {
    stop("No such file: ", image_path, call. = FALSE)
  }

  report <- if (is.function(on_progress)) on_progress else function(stage, fraction) invisible(NULL)

  report("upload", 0)
  upload <- strand_upload_file(client, image_path)
  report("upload", 1)

  # From here on the upload is durable on the platform; if anything downstream
  # fails, attach upload_id to the condition so callers can resume via
  # strand_predict(client, upload_id, markers) without paying for re-upload.
  marker_outputs <- list()
  out_dir <- NULL
  tryCatch(
    {
      report("submit", 0)
      job <- strand_predict(client, upload$id, validated_markers)
      report("submit", 1)

      report("wait", 0)
      status <- strand_job_wait(job,
                                timeout = timeout_sec,
                                poll_interval = poll_interval_sec)
      report("wait", 1)

      if (!is.null(output_dir)) {
        report("download", 0)
        out_dir <- output_dir
        strand_download_results(job, path = out_dir)
        for (m in validated_markers) {
          marker_outputs[[m]] <- file.path(out_dir, "markers", m)
        }
        report("download", 1)
      }
    },
    error = function(e) {
      if (is.null(e$upload_id)) e$upload_id <- upload$id
      stop(e)
    }
  )

  structure(
    list(
      job_id = job$id,
      status = status$status,
      credits_used = job$reserved_credits %||% 0L,
      marker_outputs = marker_outputs,
      output_dir = out_dir,
      job = job
    ),
    class = "strand_predict_result"
  )
}

#' @export
print.strand_predict_result <- function(x, ...) {
  cat("<strand_predict_result>\n")
  cat("  job_id:       ", x$job_id, "\n", sep = "")
  cat("  status:       ", x$status, "\n", sep = "")
  cat("  credits_used: ", x$credits_used, "\n", sep = "")
  if (!is.null(x$output_dir)) {
    cat("  output_dir:   ", x$output_dir, "\n", sep = "")
  }
  if (length(x$marker_outputs) > 0L) {
    cat("  markers:      ", paste(names(x$marker_outputs), collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' Run a prediction via the `stats::predict` generic
#'
#' Dispatches `predict(client, image_path, markers, ...)` to [strand_run()],
#' so the standard R generic works on a `strand_client`. See [strand_run()]
#' for the full argument list and return shape.
#'
#' @param object A `strand_client`.
#' @param image_path Path to a local WSI file.
#' @param markers Character vector of markers to predict.
#' @param ... Additional arguments passed to [strand_run()] (e.g.
#'   `timeout_sec`, `output_dir`, `poll_interval_sec`, `on_progress`).
#'
#' @return A `strand_predict_result` list; see [strand_run()].
#' @exportS3Method stats::predict
predict.strand_client <- function(object, image_path, markers, ...) {
  strand_run(object, image_path, markers, ...)
}

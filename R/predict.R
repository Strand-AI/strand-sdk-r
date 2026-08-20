# Prediction pricing and submission.

# Canonical SDK-routable Lattice versions. Mirrors `POSTMAN_VERSIONS` in
# `platform/src/lib/inference/postman-versions.ts` — the SDK can't import
# from the platform's TS source, so this list has to be kept in lockstep
# with each new version cut. See `infra/notes/postman-versioning-2026-06.md`
# §4 (rewritten 2026-06-03): legacy alias rewriting was dropped from both
# the SDK and the server. Unknown / legacy strings now flow straight
# through to the server, which returns 400 `unknown_model`. This list
# stays purely as caller-facing documentation — the SDK doesn't reject
# anything client-side, the server is the authority.
STRAND_SUPPORTED_MODELS <- c("v0.4", "v0.5", "v0.7")

strand_validate_model <- function(model) {
  if (is.null(model)) return(NULL)
  if (!is.character(model) || length(model) != 1L || is.na(model) || !nzchar(model)) {
    stop("model must be NULL or a single non-empty string", call. = FALSE)
  }
  # No client-side rewriting or warning. Pass the caller's string through
  # to the server, which is the authority on which ids are live. Legacy
  # `"v10*"` aliases were dropped on 2026-06-03 (design note §4,
  # rewritten); they now 400 with `unknown_model` server-side.
  model
}

strand_parse_estimate <- function(raw) {
  list(
    dry_run = TRUE,
    patch_count = raw$patchCount,
    marker_count = raw$markerCount,
    estimated_credits = raw$estimatedCredits,
    org_balance = raw$orgBalance %||% 0L,
    org_pending = raw$orgPending %||% 0L
  )
}

#' Price or submit a prediction
#'
#' Calls `POST /api/v1/predict`. The default mode atomically reserves credits
#' and creates a job. With `dry_run = TRUE`, the same marker, model, and sample
#' validation runs without reserving credits or creating a job.
#'
#' @param client A `strand_client`.
#' @param upload_id Upload identifier (from [strand_upload_file()]).
#' @param markers Character vector of marker names (e.g. `c("CD3", "CD8")`).
#' @param model Optional explicit Lattice version. `"v0.7"` is the current
#'   dispatchable version and default. `"v0.4"` and `"v0.5"` remain recognized
#'   identifiers for historical records, but requesting either for a new job
#'   returns 400 `model_sunset`. When `NULL` (default), the platform picks
#'   `"v0.7"`.
#' @param dry_run When `TRUE`, price the request without reserving credits or
#'   creating a job. Must be one non-missing logical value.
#'
#' @return With `dry_run = FALSE`, a `strand_job` list with `id`,
#'   `reserved_credits`, and `client`. With `dry_run = TRUE`, a list with
#'   `dry_run = TRUE`, `patch_count`, `marker_count`, `estimated_credits`,
#'   `org_balance`, and `org_pending`.
#'
#' @examples
#' \dontrun{
#' estimate <- strand_predict(
#'   client, upload$id, c("CD3", "CD8"), dry_run = TRUE
#' )
#' message("approximately ", estimate$estimated_credits, " credits")
#'
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"))
#' strand_job_wait(job)
#'
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"),
#'                       model = "v0.7")
#' }
#' @export
strand_predict <- function(client, upload_id, markers, model = NULL,
                           dry_run = FALSE) {
  markers <- strand_coerce_markers(markers)
  model <- strand_validate_model(model)
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("dry_run must be TRUE or FALSE", call. = FALSE)
  }

  body <- list(uploadId = upload_id, markers = I(markers))
  if (!is.null(model)) body$model <- model
  if (isTRUE(dry_run)) body$dryRun <- TRUE

  raw <- strand_perform_json(
    client, "predict", method = "POST",
    body = body,
    expected = if (isTRUE(dry_run)) 200L else 202L
  )
  if (isTRUE(dry_run)) return(strand_parse_estimate(raw))

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

#' Run the full prediction pipeline in one call
#'
#' Orchestrates upload → submit → (optional) wait → (optional) download. With
#' `wait = TRUE` (default) returns when the job reaches a terminal state. With
#' `wait = FALSE`, returns the `strand_job` handle as soon as upload + submit
#' complete so the caller can drive `strand_job_wait()` /
#' `strand_download_results()` later. All sub-operations use the same exported
#' primitives ([strand_upload_file()], [strand_predict()], [strand_job_wait()],
#' [strand_download_results()]) so callers can drop down a level whenever they
#' need finer control.
#'
#' @param client A `strand_client`.
#' @param image_path Path to a local WSI file (SVS / TIFF / NDPI / ...).
#' @param markers Character vector of markers to predict (e.g.
#'   `c("HER2", "CD8", "PD1")`).
#' @param model Optional explicit model id; see [strand_predict()]. Defaults
#'   to `NULL` (platform picks).
#' @param wait When `TRUE` (default), block through upload → submit → wait →
#'   download and return a `strand_predict_result`. When `FALSE`, return the
#'   `strand_job` handle once upload + submit complete. `timeout_sec`,
#'   `poll_interval_sec`, `output_dir`, and the `"wait"` / `"download"`
#'   progress stages are ignored when `wait = FALSE`.
#' @param timeout_sec Max seconds to wait for the job to finish; defaults to 30
#'   minutes.
#' @param output_dir Optional output directory. When provided, the entire
#'   OME-Zarr result store is mirrored under this directory.
#' @param poll_interval_sec Seconds between status polls while waiting.
#' @param on_progress Optional callback `function(stage, fraction)` where
#'   `stage` is one of `"upload"`, `"submit"`, `"wait"`, `"download"` (only
#'   `"upload"` and `"submit"` fire when `wait = FALSE`). `fraction` is always
#'   a numeric in `[0, 1]` — `0` at the start of each stage and `1` at its end,
#'   with intermediate values where the underlying step exposes progress.
#' @param auto_segment Opt out of automatic cell segmentation for the uploaded
#'   slide. `NULL` (default) uses the org default; `FALSE` skips segmentation;
#'   `TRUE` forces it on. Passed through to [strand_upload_file()].
#'
#' @return When `wait = TRUE`, a list with class `strand_predict_result`
#'   containing: `job_id`, `status`, `credits_used`, `marker_outputs` (named
#'   list mapping marker name → on-disk directory when `output_dir` is
#'   provided, else an empty list), `output_dir`, and `job` (the underlying
#'   `strand_job` handle). When `wait = FALSE`, the `strand_job` handle
#'   directly (same shape as [strand_predict()]).
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
#' # Choose a version explicitly:
#' result <- strand_run(client, "biopsy.ome.tiff", c("CD8", "Ki67"),
#'                      model = "v0.7")
#'
#' # Fire-and-forget — upload + submit only, then drive the job later:
#' job <- strand_run(client, "biopsy.ome.tiff", c("CD8"), wait = FALSE)
#' status <- strand_job_wait(job)
#' spe <- strand_download_results(job)
#'
#' # The S3 generic also works: predict(client, ...)
#' result <- predict(client, "biopsy.ome.tiff", markers = c("HER2"))
#' }
#' @export
strand_run <- function(client, image_path, markers,
                       model = NULL,
                       wait = TRUE,
                       timeout_sec = 1800,
                       output_dir = NULL,
                       poll_interval_sec = 5,
                       on_progress = NULL,
                       auto_segment = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  validated_markers <- strand_coerce_markers(markers)
  model <- strand_validate_model(model)
  if (!file.exists(image_path)) {
    stop("No such file: ", image_path, call. = FALSE)
  }

  report <- if (is.function(on_progress)) on_progress else function(stage, fraction) invisible(NULL)

  report("upload", 0)
  upload <- strand_upload_file(client, image_path, auto_segment = auto_segment)
  report("upload", 1)

  # From here on the upload is durable on the platform; if anything downstream
  # fails, attach upload_id to the condition so callers can resume via
  # strand_predict(client, upload_id, markers) without paying for re-upload.
  marker_outputs <- list()
  out_dir <- NULL
  status <- NULL
  tryCatch(
    {
      report("submit", 0)
      job <- strand_predict(client, upload$id, validated_markers, model = model)
      report("submit", 1)

      if (!isTRUE(wait)) {
        return(job)
      }

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
      # Canonical v0.X label the platform actually ran. Reading off
      # `status$model` (rather than the user-supplied `model` arg) so we
      # surface what dispatched even when the caller omitted `model =`
      # and the platform picked its default. `NULL` against older
      # servers that didn't return the field on the job payload.
      model = status$model,
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
  if (!is.null(x$model)) {
    cat("  model:        ", x$model, "\n", sep = "")
  }
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
#' for the full argument list and return shape — `model =` and `wait =` are
#' both forwarded.
#'
#' @param object A `strand_client`.
#' @param image_path Path to a local WSI file.
#' @param markers Character vector of markers to predict.
#' @param ... Additional arguments passed to [strand_run()] (e.g. `model`,
#'   `wait`, `timeout_sec`, `output_dir`, `poll_interval_sec`, `on_progress`).
#'
#' @return A `strand_predict_result` list when `wait = TRUE`, or a
#'   `strand_job` handle when `wait = FALSE`; see [strand_run()].
#' @exportS3Method stats::predict
predict.strand_client <- function(object, image_path, markers, ...) {
  strand_run(object, image_path, markers, ...)
}

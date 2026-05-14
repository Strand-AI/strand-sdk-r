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

# Job handle — refresh, wait, results.

#' Fetch the latest status snapshot for a job
#'
#' @param client A `strand_client`.
#' @param job_id Job identifier (or a `strand_job` list).
#' @return A list with the job's current state.
#' @export
strand_job_get <- function(client, job_id) {
  if (inherits(client, "strand_job") && missing(job_id)) {
    job <- client
    raw <- strand_perform_json(job$client, sprintf("jobs/%s", job$id))
  } else if (inherits(client, "strand_client")) {
    raw <- strand_perform_json(client, sprintf("jobs/%s", job_id))
  } else {
    stop("First argument must be a strand_client or strand_job", call. = FALSE)
  }
  strand_parse_job(raw)
}

strand_parse_job <- function(raw) {
  list(
    id = raw$id,
    status = raw$status,
    progress = raw$progress,
    reserved_credits = raw$reservedCredits,
    markers = unlist(raw$markers %||% list(), use.names = FALSE),
    # Canonical v0.X Lattice version that ran. The platform normalizes
    # legacy aliases before persisting (design note §0 / §4), so this
    # field is always a v0.X label when the server populated it. `NULL`
    # against older deploys that didn't emit `model` on the job payload.
    model = raw$model,
    created_at = raw$createdAt,
    started_at = raw$startedAt,
    completed_at = raw$completedAt,
    error_message = raw$errorMessage,
    results_available = isTRUE(raw$resultsAvailable)
  )
}

#' Cancel an in-flight job
#'
#' Calls `POST /api/v1/jobs/{id}/cancel`, then fetches and returns the updated
#' job status. Cancellation refunds the reserved credits and releases the
#' organization's concurrent-job slot. The server returns 400 if the job is
#' already terminal.
#'
#' @param job A `strand_job` from [strand_predict()].
#'
#' @return The updated job status list with `status = "cancelled"`.
#'
#' @examples
#' \dontrun{
#' job <- strand_predict(client, upload$id, c("CD3", "CD8"))
#' status <- strand_job_cancel(job)
#' }
#' @export
strand_job_cancel <- function(job) {
  if (!inherits(job, "strand_job")) {
    stop("job must be a strand_job (from strand_predict())", call. = FALSE)
  }
  strand_perform_json(
    job$client,
    sprintf("jobs/%s/cancel", job$id),
    method = "POST"
  )
  strand_job_get(job)
}

#' Block until a job reaches a terminal status
#'
#' Polls `GET /api/v1/jobs/{id}` on a fixed interval and returns the terminal
#' status. (The platform also exposes an SSE stream at `/jobs/{id}/stream`; the
#' R SDK uses polling for compatibility — there is no widely-used SSE client in
#' base R, and httr2 streaming is awkward to integrate with `R CMD check`.)
#'
#' @param job A `strand_job` from [strand_predict()].
#' @param timeout Max seconds to wait. `Inf` to wait forever.
#' @param poll_interval Seconds between status polls.
#' @param progress If `TRUE`, prints a one-line status update per poll.
#'
#' @return The terminal job status list.
#'
#' @export
strand_job_wait <- function(job, timeout = Inf,
                            poll_interval = 2,
                            progress = FALSE) {
  if (!inherits(job, "strand_job")) {
    stop("job must be a strand_job (from strand_predict())", call. = FALSE)
  }
  deadline <- if (is.finite(timeout)) Sys.time() + timeout else NA
  last_status <- NA_character_
  repeat {
    status <- strand_job_get(job)
    if (isTRUE(progress) && !identical(status$status, last_status)) {
      prog <- if (!is.null(status$progress)) sprintf(" (%.0f%%)", 100 * status$progress) else ""
      message(sprintf("  status: %s%s", status$status, prog))
      last_status <- status$status
    }
    if (status$status %in% c("completed", "failed", "cancelled")) {
      if (identical(status$status, "failed")) {
        msg <- status$error_message %||% sprintf("Job %s failed", job$id)
        stop(structure(list(message = msg, job_id = job$id),
                       class = c("strand_job_failed_error", "strand_api_error",
                                  "error", "condition")))
      }
      return(status)
    }
    if (!is.na(deadline) && Sys.time() > deadline) {
      stop(sprintf("Job %s did not reach terminal status within %s seconds",
                   job$id, timeout), call. = FALSE)
    }
    Sys.sleep(poll_interval)
  }
}

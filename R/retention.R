# Per-sample retention controls (Phase 2). Mirrors the REST surface:
#   PATCH /samples/{id}/retention
#   PATCH /samples/retention            (bulk)
#   POST  /samples/{id}/restore
#
# Modes are mutually exclusive — exactly one of `expires_at`, `pin = TRUE`,
# or `use_org_default = TRUE`. We validate client-side so misuse fails
# before the round-trip.

# Build the JSON body shared between single + bulk endpoints. Returns a
# named list ready for jsonlite::toJSON.
strand_retention_body <- function(expires_at, pin, use_org_default, reason) {
  picked <- sum(!is.null(expires_at), isTRUE(pin), isTRUE(use_org_default))
  if (picked != 1L) {
    stop(
      "Provide exactly one of: expires_at, pin = TRUE, or use_org_default = TRUE.",
      call. = FALSE
    )
  }
  body <- list()
  if (isTRUE(pin)) {
    body$pin <- TRUE
  } else if (isTRUE(use_org_default)) {
    body$useOrgDefault <- TRUE
  } else {
    if (inherits(expires_at, "POSIXt")) {
      # ISO 8601 with timezone. format.POSIXct emits "+0000" — change to
      # "+00:00" so the server's Zod date-time validator accepts it.
      iso <- format(expires_at, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      iso <- sub("(\\+|-)([0-9]{2})([0-9]{2})$", "\\1\\2:\\3", iso)
      body$expiresAt <- iso
    } else {
      body$expiresAt <- as.character(expires_at)
    }
  }
  if (!is.null(reason)) {
    body$reason <- reason
  }
  body
}

#' Set retention on a single sample
#'
#' Pin a sample to a specific expiry, pin indefinitely, or revert to the
#' org's default retention policy.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id UUID of the sample.
#' @param expires_at A `POSIXct` (or ISO 8601 character) expiry. Pass `NULL`
#'   together with `pin = TRUE` to pin indefinitely.
#' @param pin If `TRUE`, pin the sample indefinitely (overrides org policy).
#' @param use_org_default If `TRUE`, clear any override and revert to the
#'   org's current retention policy.
#' @param reason Optional governance reason (10-500 chars).
#'
#' @return The updated sample payload (`id`, `expiresAt`, `expiresAtSource`, ...).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' strand_set_retention(client, sample_id, pin = TRUE,
#'                      reason = "Active research project — keep until publication")
#' }
#' @export
strand_set_retention <- function(client, sample_id,
                                  expires_at = NULL,
                                  pin = FALSE,
                                  use_org_default = FALSE,
                                  reason = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  body <- strand_retention_body(expires_at, pin, use_org_default, reason)
  strand_perform_json(
    client, sprintf("samples/%s/retention", sample_id),
    method = "PATCH", body = body
  )
}

#' Set retention on a batch of samples (max 500)
#'
#' All-or-nothing: if any sample fails the permission gate (caller is not
#' the sample creator, an org owner/admin, or a Strand admin), the whole
#' call returns a 403 with no rows touched.
#'
#' @inheritParams strand_set_retention
#' @param sample_ids Character vector of sample UUIDs (length 1-500).
#'
#' @return A list with `updated` (integer count) and `batchId` (UUID).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' strand_set_retention_bulk(client, c(uuid1, uuid2, uuid3), pin = TRUE)
#' }
#' @export
strand_set_retention_bulk <- function(client, sample_ids,
                                       expires_at = NULL,
                                       pin = FALSE,
                                       use_org_default = FALSE,
                                       reason = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  if (!is.character(sample_ids) || length(sample_ids) == 0L || length(sample_ids) > 500L) {
    stop("sample_ids must be a character vector of length 1-500.", call. = FALSE)
  }
  body <- strand_retention_body(expires_at, pin, use_org_default, reason)
  body$sampleIds <- as.list(sample_ids)
  strand_perform_json(client, "samples/retention", method = "PATCH", body = body)
}

#' Restore an archived sample
#'
#' Sets `archived_at` to `NULL` and bumps `expires_at` to at least 30 days
#' from now so the nightly reaper does not immediately re-archive it. Caller
#' must have the same permissions required for [strand_set_retention()].
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id UUID of the sample.
#'
#' @return The restored sample payload (`id`, `archivedAt`, `expiresAt`).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' strand_restore(client, sample_id)
#' }
#' @export
strand_restore <- function(client, sample_id) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  strand_perform_json(
    client, sprintf("samples/%s/restore", sample_id),
    method = "POST"
  )
}

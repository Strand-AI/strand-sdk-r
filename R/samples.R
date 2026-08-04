# Sample resource reads. Mirrors GET /api/v1/samples/{id}.

#' Get a sample
#'
#' Fetch a sample's curated resource: identity, status, physical scale, tags,
#' and its current expiration. This is the read-only way to check when a
#' sample expires (via `will_expire` / `expires_in_days` / `expires_at`)
#' before it moves to Trash, without the mutation `strand_set_expiration()`
#' requires. Any API key whose org owns the sample can read it.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id UUID of the sample.
#'
#' @return A named list describing the sample:
#'   \describe{
#'     \item{id}{Sample UUID.}
#'     \item{name}{Display name, or `NULL` if unset.}
#'     \item{filename}{Original uploaded filename.}
#'     \item{status}{Lifecycle status (`"uploading"`, `"preprocessing"`,
#'       `"ready"`, `"preprocess_failed"`).}
#'     \item{file_size}{Uploaded file size in bytes (numeric), or `NULL`.}
#'     \item{width_px, height_px}{Level-0 dimensions in pixels, or `NULL`
#'       before the dimensions probe completes.}
#'     \item{mpp}{Effective microns per pixel as a named numeric vector
#'       `c(x, y)`, or `NULL` when the sample has no usable scale yet.}
#'     \item{tags}{Character vector of canonical tags (possibly empty).}
#'     \item{created_at}{ISO 8601 creation timestamp.}
#'     \item{expires_at}{ISO 8601 timestamp the sample moves to Trash, or
#'       `NULL` if it never expires.}
#'     \item{expires_at_source}{`"org_default"`, `"custom"`, or `NULL`.}
#'     \item{expires_in_days}{Whole days until expiry (clamped at 0 once
#'       reached), or `NULL` if it never expires.}
#'     \item{will_expire}{`TRUE` when an expiration date is set.}
#'     \item{trashed_at}{ISO 8601 timestamp the sample entered Trash, or
#'       `NULL` if it is still active.}
#'   }
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' s <- strand_samples_get(client, sample_id)
#' if (s$will_expire) message("expires in ", s$expires_in_days, " days")
#' }
#' @export
strand_samples_get <- function(client, sample_id) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  raw <- strand_perform_json(client, sprintf("samples/%s", sample_id))

  file_size <- raw$fileSize
  if (!is.null(file_size)) {
    coerced <- suppressWarnings(as.numeric(file_size))
    file_size <- if (is.na(coerced)) NULL else coerced
  }

  mpp <- NULL
  if (!is.null(raw$mpp) && !is.null(raw$mpp$x) && !is.null(raw$mpp$y)) {
    mpp <- c(x = as.numeric(raw$mpp$x), y = as.numeric(raw$mpp$y))
  }

  list(
    id = strand_as_scalar_chr(raw$id),
    name = strand_as_scalar_chr(raw$name),
    filename = strand_as_scalar_chr(raw$filename),
    status = strand_as_scalar_chr(raw$status),
    file_size = file_size,
    width_px = strand_as_scalar_int(raw$widthPx),
    height_px = strand_as_scalar_int(raw$heightPx),
    mpp = mpp,
    tags = vapply(raw$tags %||% list(), as.character, character(1)),
    created_at = strand_as_scalar_chr(raw$createdAt),
    expires_at = strand_as_scalar_chr(raw$expiresAt),
    expires_at_source = strand_as_scalar_chr(raw$expiresAtSource),
    expires_in_days = strand_as_scalar_int(raw$expiresInDays),
    will_expire = isTRUE(raw$willExpire),
    trashed_at = strand_as_scalar_chr(raw$trashedAt)
  )
}

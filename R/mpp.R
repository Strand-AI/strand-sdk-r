# Physical pixel-size overrides.

#' Set sample microns per pixel
#'
#' Sets the user-reported physical pixel size at the slide's base level. This
#' value takes precedence over embedded slide metadata for subsequent inference
#' jobs. Slides are isotropic: a single value governs both axes.
#'
#' @param client A `strand_client`.
#' @param sample_id Sample or completed-upload identifier.
#' @param mpp Microns per pixel, greater than 0 and at most 100.
#'
#' @return A list with `id` and the persisted scalar `mpp`.
#'
#' @examples
#' \dontrun{
#' strand_set_mpp(client, upload$id, 0.26)
#' }
#' @export
strand_set_mpp <- function(client, sample_id, mpp) {
  value <- strand_validate_mpp(mpp, "mpp")
  raw <- strand_perform_json(
    client,
    sprintf("samples/%s/mpp", sample_id),
    method = "PATCH",
    body = list(mpp = value)
  )
  list(
    id = raw$id,
    mpp = as.numeric(raw$mpp)
  )
}

strand_validate_mpp <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value <= 0 || value > 100) {
    stop(name, " must be a number greater than 0 and at most 100", call. = FALSE)
  }
  as.numeric(value)
}

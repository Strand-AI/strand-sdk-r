# Physical pixel-size overrides.

#' Set sample microns per pixel
#'
#' Sets the user-reported physical pixel size at the slide's base level. This
#' value takes precedence over embedded slide metadata for subsequent inference
#' jobs.
#'
#' @param client A `strand_client`.
#' @param sample_id Sample or completed-upload identifier.
#' @param mpp_x Horizontal microns per pixel, greater than 0 and at most 100.
#' @param mpp_y Optional vertical microns per pixel. When `NULL`, the platform
#'   uses `mpp_x` for both axes.
#'
#' @return A list with `id` and `user_mpp`, a named numeric vector containing
#'   the normalized `x` and `y` values.
#'
#' @examples
#' \dontrun{
#' strand_set_mpp(client, upload$id, 0.26)
#' strand_set_mpp(client, upload$id, 0.26, 0.25)
#' }
#' @export
strand_set_mpp <- function(client, sample_id, mpp_x, mpp_y = NULL) {
  x <- strand_validate_mpp(mpp_x, "mpp_x")
  mpp <- if (is.null(mpp_y)) {
    x
  } else {
    list(x = x, y = strand_validate_mpp(mpp_y, "mpp_y"))
  }
  raw <- strand_perform_json(
    client,
    sprintf("samples/%s/mpp", sample_id),
    method = "PATCH",
    body = list(mpp = mpp)
  )
  list(
    id = raw$id,
    user_mpp = c(x = as.numeric(raw$userMpp$x), y = as.numeric(raw$userMpp$y))
  )
}

strand_validate_mpp <- function(value, name) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
      !is.finite(value) || value <= 0 || value > 100) {
    stop(name, " must be a number greater than 0 and at most 100", call. = FALSE)
  }
  as.numeric(value)
}

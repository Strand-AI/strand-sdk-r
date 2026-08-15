# Marker catalog. Mirrors GET /api/v1/markers.
#
# The entitlement-scoped set of markers the account may request. Credit-free.
# The returned names are exactly what strand_predict()/strand_estimate() will
# accept, so use this to discover valid marker names upfront.

#' List available markers
#'
#' List the markers your account can request, scoped to your entitlement: a
#' self-signup account sees the public panel; a full-panel account sees the
#' whole vocabulary. Credit-free. The names returned are exactly what
#' [strand_predict()] and [strand_estimate()] will accept.
#'
#' @param client A `strand_client` from [strand_client()].
#'
#' @return A named list with `full_panel` (logical; whether the account holds
#'   the full-panel entitlement), `markers` (a character vector of marker
#'   names), and `public_markers` (the subset that is part of the public panel).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' catalog <- strand_markers_list(client)
#' catalog$markers
#' }
#' @export
strand_markers_list <- function(client) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  raw <- strand_perform_json(client, "markers")

  entries <- raw$markers %||% list()
  names_vec <- vapply(entries, function(m) strand_as_scalar_chr(m$name), character(1))
  public_flags <- vapply(entries, function(m) isTRUE(m$publicPanel), logical(1))

  list(
    full_panel = isTRUE(raw$fullPanel),
    markers = names_vec,
    public_markers = names_vec[public_flags]
  )
}

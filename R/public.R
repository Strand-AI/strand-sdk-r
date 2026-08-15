# Public-cohort reads. Mirrors GET /api/v1/public/samples[/{publicId}].
#
# Any authenticated client may browse and read Strand AI's curated public cohort
# (the TCGA release) for free — it is org-independent, and no read spends
# credits. Generation stays credit-gated on strand_predict()/strand_upload_file().

.strand_scalar_dbl <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.numeric(x[[1L]])) {
    return(NULL)
  }
  as.numeric(x[[1L]])
}

#' List the public cohort
#'
#' List Strand AI's curated public cohort (paginated, newest-first). Available to
#' any authenticated client for free — the cohort is org-independent, so you
#' read it regardless of your own org, and no read spends credits.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param page 1-based page number. `NULL` (default) reads page 1.
#' @param page_size Items per page (server default 48, max 100). `NULL` uses
#'   the server default.
#' @param tag Optional public display-tag filter. An unknown tag returns an
#'   empty page.
#'
#' @return A named list with `items` (a list of per-sample lists carrying
#'   `public_id`, `title`, `thumbnail_url`, `tags`, and `metadata`) plus the
#'   `page`, `page_size`, `total_count`, and `total_pages` integers.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' page <- strand_public_samples_list(client, tag = "tcga-coad")
#' vapply(page$items, function(s) s$public_id, character(1))
#' }
#' @export
strand_public_samples_list <- function(client, page = NULL, page_size = NULL, tag = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  query <- list()
  if (!is.null(page)) query$page <- page
  if (!is.null(page_size)) query$pageSize <- page_size
  if (!is.null(tag)) query$tag <- tag

  raw <- strand_perform_json(client, "public/samples", query = query)

  items <- lapply(raw$items %||% list(), function(it) {
    list(
      public_id = strand_as_scalar_chr(it$publicId),
      title = strand_as_scalar_chr(it$title),
      thumbnail_url = strand_as_scalar_chr(it$thumbnailUrl),
      tags = vapply(it$tags %||% list(), as.character, character(1)),
      metadata = it$metadata %||% list()
    )
  })

  list(
    items = items,
    page = strand_as_scalar_int(raw$page),
    page_size = strand_as_scalar_int(raw$pageSize),
    total_count = strand_as_scalar_int(raw$totalCount),
    total_pages = strand_as_scalar_int(raw$totalPages)
  )
}

#' Get a public sample
#'
#' Read one public sample's curated detail: title, tags, public metadata,
#' geometry, and its live marker names. Free and credit-less. A `public_id`
#' that is not a currently-public sample raises a not-found error — there is no
#' path to a non-public or org-scoped sample.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param public_id Public id from [strand_public_samples_list()].
#'
#' @return A named list describing the public sample:
#'   \describe{
#'     \item{public_id}{Public id.}
#'     \item{title}{Display title.}
#'     \item{tags}{Character vector of public display tags (possibly empty).}
#'     \item{metadata}{Named list of public-visible metadata.}
#'     \item{geometry}{Named list `width_px`, `height_px`, `mpp_x`, `mpp_y`.}
#'     \item{markers}{Character vector of live marker channel names.}
#'     \item{thumbnail_url}{API-relative path to the JPEG thumbnail.}
#'     \item{pyramid_url}{API-relative base path of the OME-Zarr pyramid.}
#'   }
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' s <- strand_public_sample_get(client, "00000000-0000-4000-8000-000000000000")
#' s$markers
#' }
#' @export
strand_public_sample_get <- function(client, public_id) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  raw <- strand_perform_json(client, sprintf("public/samples/%s", public_id))

  geom <- raw$geometry %||% list()
  viewer <- raw$viewer %||% list()
  markers <- vapply(
    viewer$markers %||% list(),
    function(m) as.character(m$name),
    character(1)
  )

  list(
    public_id = strand_as_scalar_chr(raw$publicId),
    title = strand_as_scalar_chr(raw$title),
    tags = vapply(raw$tags %||% list(), as.character, character(1)),
    metadata = raw$metadata %||% list(),
    geometry = list(
      width_px = strand_as_scalar_int(geom$widthPx),
      height_px = strand_as_scalar_int(geom$heightPx),
      mpp_x = .strand_scalar_dbl(geom$mppX),
      mpp_y = .strand_scalar_dbl(geom$mppY)
    ),
    markers = markers,
    thumbnail_url = strand_as_scalar_chr(raw$thumbnailUrl),
    pyramid_url = strand_as_scalar_chr(viewer$pyramidUrl)
  )
}

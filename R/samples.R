# Sample collection and detail resources. Mirrors GET/PATCH /api/v1/samples.

.strand_scalar_dbl <- function(x) {
  if (is.null(x) || length(x) != 1L || !is.numeric(x[[1L]])) {
    return(NULL)
  }
  as.numeric(x[[1L]])
}

.strand_file_size <- function(x) {
  if (is.null(x)) return(NULL)
  coerced <- suppressWarnings(as.numeric(x))
  if (length(coerced) != 1L || is.na(coerced)) NULL else coerced
}

.strand_tags <- function(x) {
  vapply(x %||% list(), as.character, character(1))
}

.strand_metadata <- function(x) {
  if (is.null(x)) return(list())
  if (!is.list(x) || (length(x) > 0L && is.null(names(x)))) {
    stop("Invalid sample response: metadata must be an object", call. = FALSE)
  }
  x
}

.strand_ownership <- function(raw) {
  ownership <- raw$ownership
  if (!is.character(ownership) || length(ownership) != 1L || is.na(ownership) ||
      !(ownership %in% c("mine", "public"))) {
    stop("Invalid sample response: ownership must be 'mine' or 'public'", call. = FALSE)
  }
  ownership
}

.strand_parse_mine_list_item <- function(raw) {
  list(
    ownership = "mine",
    id = strand_as_scalar_chr(raw$id),
    name = strand_as_scalar_chr(raw$name),
    filename = strand_as_scalar_chr(raw$filename),
    status = strand_as_scalar_chr(raw$status),
    file_size = .strand_file_size(raw$fileSize),
    tags = .strand_tags(raw$tags),
    created_at = strand_as_scalar_chr(raw$createdAt)
  )
}

.strand_parse_public_list_item <- function(raw) {
  list(
    ownership = "public",
    id = strand_as_scalar_chr(raw$id),
    title = strand_as_scalar_chr(raw$title),
    thumbnail_url = strand_as_scalar_chr(raw$thumbnailUrl),
    tags = .strand_tags(raw$tags),
    metadata = .strand_metadata(raw$metadata)
  )
}

.strand_parse_sample_list_item <- function(raw) {
  ownership <- .strand_ownership(raw)
  if (identical(ownership, "mine")) {
    .strand_parse_mine_list_item(raw)
  } else {
    .strand_parse_public_list_item(raw)
  }
}

.strand_validate_scope <- function(scope) {
  if (!is.character(scope) || length(scope) != 1L || is.na(scope) ||
      !(scope %in% c("mine", "public", "all"))) {
    stop("scope must be exactly one of 'mine', 'public', or 'all'", call. = FALSE)
  }
  scope
}

.strand_validate_sample_limit <- function(limit) {
  if (!is.numeric(limit) || length(limit) != 1L || is.na(limit) ||
      !is.finite(limit) || limit != as.integer(limit) || limit < 1L || limit > 100L) {
    stop("limit must be a whole number from 1 to 100", call. = FALSE)
  }
  as.integer(limit)
}

#' List samples
#'
#' Lists owned samples, Strand's public cohort, or both through one
#' cursor-paginated collection. Public and owned records use distinct shapes;
#' inspect each item's mandatory `ownership` field before using branch fields.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param scope Exactly one of `"mine"`, `"public"`, or `"all"`. The default
#'   lists owned samples. Values are case-sensitive and are never partially
#'   matched.
#' @param limit Items per page, from 1 through 100.
#' @param cursor Opaque cursor returned as `next_cursor` by the previous call,
#'   or `NULL` for the first page.
#' @param tag Optional exact tag filter.
#'
#' @return A named list with `items` and `next_cursor`. Every item has
#'   `ownership` and canonical `id`. Owned items also have `name`, `filename`,
#'   `status`, numeric `file_size`, `tags`, and `created_at`. Public items also
#'   have `title`, `thumbnail_url`, `tags`, and public `metadata`.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' page <- strand_samples_list(client, scope = "public", tag = "tcga-coad")
#' ids <- vapply(page$items, function(sample) sample$id, character(1))
#' }
#' @export
strand_samples_list <- function(client, scope = "mine", limit = 48L,
                                cursor = NULL, tag = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  scope <- .strand_validate_scope(scope)
  limit <- .strand_validate_sample_limit(limit)

  query <- list(scope = scope, limit = limit)
  if (!is.null(cursor)) query$cursor <- cursor
  if (!is.null(tag)) query$tag <- tag

  raw <- strand_perform_json(client, "samples", query = query)
  list(
    items = lapply(raw$items %||% list(), .strand_parse_sample_list_item),
    next_cursor = strand_as_scalar_chr(raw$nextCursor)
  )
}

.strand_parse_sample_job <- function(raw) {
  list(
    id = strand_as_scalar_chr(raw$id),
    status = strand_as_scalar_chr(raw$status),
    progress = .strand_scalar_dbl(raw$progress),
    reserved_credits = strand_as_scalar_int(raw$reservedCredits),
    markers = .strand_tags(raw$markers),
    created_at = strand_as_scalar_chr(raw$createdAt),
    started_at = strand_as_scalar_chr(raw$startedAt),
    completed_at = strand_as_scalar_chr(raw$completedAt),
    error_message = strand_as_scalar_chr(raw$errorMessage),
    results_available = isTRUE(raw$resultsAvailable)
  )
}

.strand_parse_owned_detail <- function(raw) {
  mpp <- NULL
  if (!is.null(raw$mpp) && is.numeric(raw$mpp) && length(raw$mpp) == 1L) {
    mpp <- as.numeric(raw$mpp)
  }

  list(
    ownership = "mine",
    id = strand_as_scalar_chr(raw$id),
    name = strand_as_scalar_chr(raw$name),
    filename = strand_as_scalar_chr(raw$filename),
    status = strand_as_scalar_chr(raw$status),
    file_size = .strand_file_size(raw$fileSize),
    width_px = strand_as_scalar_int(raw$widthPx),
    height_px = strand_as_scalar_int(raw$heightPx),
    mpp = mpp,
    tags = .strand_tags(raw$tags),
    created_at = strand_as_scalar_chr(raw$createdAt),
    expires_at = strand_as_scalar_chr(raw$expiresAt),
    expires_at_source = strand_as_scalar_chr(raw$expiresAtSource),
    expires_in_days = strand_as_scalar_int(raw$expiresInDays),
    will_expire = isTRUE(raw$willExpire),
    trashed_at = strand_as_scalar_chr(raw$trashedAt),
    jobs = lapply(raw$jobs %||% list(), .strand_parse_sample_job),
    job_count = strand_as_scalar_int(raw$jobCount)
  )
}

.strand_parse_public_detail <- function(raw) {
  geom <- raw$geometry %||% list()
  viewer <- raw$viewer %||% list()
  markers <- vapply(
    viewer$markers %||% list(),
    function(marker) as.character(marker$name),
    character(1)
  )

  list(
    ownership = "public",
    id = strand_as_scalar_chr(raw$id),
    title = strand_as_scalar_chr(raw$title),
    tags = .strand_tags(raw$tags),
    metadata = .strand_metadata(raw$metadata),
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

.strand_parse_sample_detail <- function(raw) {
  ownership <- .strand_ownership(raw)
  if (identical(ownership, "mine")) {
    .strand_parse_owned_detail(raw)
  } else {
    .strand_parse_public_detail(raw)
  }
}

#' Get a sample
#'
#' Fetches either an owned sample by sample id or a public sample by share id.
#' The mandatory `ownership` field identifies the returned branch. Owned
#' samples include up to the 50 newest inference jobs and the uncapped
#' `job_count`; public samples never expose job history.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id Owned sample UUID or public share UUID.
#'
#' @return For an owned sample, a named list with `ownership = "mine"`, sample
#'   identity and lifecycle fields, `jobs`, and `job_count`. Each job contains
#'   `id`, `status`, `progress`, `reserved_credits`, `markers`, timestamps,
#'   `error_message`, and `results_available`. For a public sample, a named list
#'   with `ownership = "public"`, canonical share `id`, `title`, `tags`, public
#'   `metadata`, `geometry`, `markers`, `thumbnail_url`, and `pyramid_url`.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' sample <- strand_samples_get(client, sample_id)
#' if (sample$ownership == "mine") {
#'   message(sample$job_count, " inference jobs")
#' } else {
#'   print(sample$markers)
#' }
#' }
#' @export
strand_samples_get <- function(client, sample_id) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  raw <- strand_perform_json(client, sprintf("samples/%s", sample_id))
  .strand_parse_sample_detail(raw)
}

#' Patch an owned sample
#'
#' Updates any combination of display name, complete user-editable tag set,
#' and physical pixel size in one request. `NULL` arguments are omitted.
#' Use `clear_name = TRUE` to send JSON `null` and restore the filename as the
#' display name. Passing `tags = character(0)` clears all user-editable tags.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id UUID of an owned sample.
#' @param name New display name, or `NULL` to leave it unchanged.
#' @param tags Character vector containing the complete desired tag set,
#'   `character(0)` to clear it, or `NULL` to leave it unchanged.
#' @param mpp Isotropic microns per pixel, greater than 0 and at most 100, or
#'   `NULL` to leave it unchanged.
#' @param clear_name Send JSON `null` for `name`, restoring the filename as the
#'   display name. Cannot be combined with a non-`NULL` `name`.
#'
#' @return The refreshed owned sample record, including `jobs` and `job_count`;
#'   see [strand_samples_get()].
#'
#' @examples
#' \dontrun{
#' strand_patch_sample(client, sample_id, name = "Baseline biopsy")
#' strand_patch_sample(client, sample_id, tags = c("baseline", "responding"))
#' strand_patch_sample(client, sample_id, tags = character(0), mpp = 0.26)
#' strand_patch_sample(client, sample_id, clear_name = TRUE)
#' }
#' @export
strand_patch_sample <- function(client, sample_id, name = NULL, tags = NULL,
                                mpp = NULL, clear_name = FALSE) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  if (!is.logical(clear_name) || length(clear_name) != 1L || is.na(clear_name)) {
    stop("clear_name must be TRUE or FALSE", call. = FALSE)
  }
  if (isTRUE(clear_name) && !is.null(name)) {
    stop("clear_name = TRUE cannot be combined with name", call. = FALSE)
  }
  if (!is.null(name) &&
      (!is.character(name) || length(name) != 1L || is.na(name))) {
    stop("name must be NULL or a single string", call. = FALSE)
  }
  if (!is.null(tags) && !is.character(tags)) {
    stop("tags must be NULL or a character vector", call. = FALSE)
  }

  body <- list()
  if (isTRUE(clear_name)) {
    body["name"] <- list(NULL)
  } else if (!is.null(name)) {
    body$name <- name
  }
  if (!is.null(tags)) body$tags <- I(tags)
  if (!is.null(mpp)) body$mpp <- strand_validate_mpp(mpp, "mpp")
  if (length(body) == 0L) {
    stop("at least one of name, tags, mpp, or clear_name must change", call. = FALSE)
  }

  raw <- strand_perform_json(
    client,
    sprintf("samples/%s", sample_id),
    method = "PATCH",
    body = body
  )
  parsed <- .strand_parse_sample_detail(raw)
  if (!identical(parsed$ownership, "mine")) {
    stop("Invalid sample response: PATCH must return an owned sample", call. = FALSE)
  }
  parsed
}

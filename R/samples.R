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

.strand_fields <- function(x) {
  if (is.null(x)) return(list())
  if (!is.list(x) || (length(x) > 0L && is.null(names(x)))) {
    stop("Invalid sample response: fields must be an object", call. = FALSE)
  }
  # Tags is the predefined free-form multi-select and is always ergonomic as a
  # character vector; other field values retain their JSON-derived R shape.
  if ("tags" %in% names(x)) x$tags <- .strand_tags(x$tags)
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
    app_url = strand_as_scalar_chr(raw$appUrl),
    name = strand_as_scalar_chr(raw$name),
    filename = strand_as_scalar_chr(raw$filename),
    status = strand_as_scalar_chr(raw$status),
    file_size = .strand_file_size(raw$fileSize),
    fields = .strand_fields(raw$fields),
    created_at = strand_as_scalar_chr(raw$createdAt)
  )
}

.strand_parse_public_list_item <- function(raw) {
  list(
    ownership = "public",
    id = strand_as_scalar_chr(raw$id),
    app_url = strand_as_scalar_chr(raw$appUrl),
    title = strand_as_scalar_chr(raw$title),
    thumbnail_url = strand_as_scalar_chr(raw$thumbnailUrl)
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

#' List custom sample fields
#'
#' Lists the calling organization's custom-field definitions. This is the
#' discovery surface for [strand_field_filter()]: a caller cannot guess a
#' definition id, so read the catalog once and filter by the ids it returns.
#'
#' @param client A `strand_client` from [strand_client()].
#'
#' @return A list of definitions, each with `id`, `key`, `label`, `value_type`,
#'   and `options` (a character vector for the select types, otherwise `NULL`).
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' fields <- strand_sample_fields_list(client)
#' cohort <- Filter(function(f) f$key == "cohort", fields)[[1]]
#' strand_samples_list(
#'   client,
#'   field = strand_field_filter(cohort$id, "eq", "phase-2")
#' )
#' }
#' @export
strand_sample_fields_list <- function(client) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  raw <- strand_perform_json(client, "sample-fields")
  lapply(raw$fields %||% list(), function(field) {
    options <- field$options
    list(
      id = strand_as_scalar_chr(field$id),
      key = strand_as_scalar_chr(field$key),
      label = strand_as_scalar_chr(field$label),
      value_type = strand_as_scalar_chr(field$valueType),
      origin = strand_as_scalar_chr(field$origin),
      storage = strand_as_scalar_chr(field$storage),
      writable = isTRUE(field$writable),
      # Select types always carry at least one option, so an absent, null, or
      # empty list all mean "not a select field".
      options = if (length(options) == 0L) NULL else vapply(options, as.character, character(1))
    )
  })
}

#' Build a marker-state filter clause
#'
#' One clause for [strand_samples_list()]'s `marker` argument: whether a sample
#' has a prediction for `marker`, and in which state. `"requested"` matches a
#' prediction in any state; `"completed"` only a finished one.
#'
#' @param marker Marker name, e.g. `"CD20"`. Cannot contain `:` or `,`.
#' @param state Either `"completed"` (default) or `"requested"`.
#'
#' @return A `strand_marker_filter` object.
#'
#' @examples
#' strand_marker_filter("CD20")
#' strand_marker_filter("CD4", "requested")
#' @export
strand_marker_filter <- function(marker, state = c("completed", "requested")) {
  if (!is.character(marker) || length(marker) != 1L || is.na(marker) ||
      !nzchar(trimws(marker))) {
    stop("marker must be a single non-empty marker name", call. = FALSE)
  }
  if (grepl("[:,]", marker)) {
    stop("marker names cannot contain ':' or ','", call. = FALSE)
  }
  state <- match.arg(state)
  structure(
    list(marker = trimws(marker), state = state),
    class = "strand_marker_filter"
  )
}

.strand_field_operators <- c("eq", "contains", "gte", "lte", "is_empty", "is_not_empty")
.strand_valueless_field_operators <- c("is_empty", "is_not_empty")

#' Build a custom-field filter clause
#'
#' One clause for [strand_samples_list()]'s `field` argument, bound to an
#' organization-defined field's definition id. `"is_empty"` and
#' `"is_not_empty"` take no value; the other operators require one, checked
#' server-side against the field's declared type.
#'
#' @param field_id The custom field's definition id (a UUID).
#' @param operator One of `"eq"`, `"contains"`, `"gte"`, `"lte"`,
#'   `"is_empty"`, `"is_not_empty"`.
#' @param value A single string, number, or logical. Omit for the valueless
#'   operators.
#'
#' @return A `strand_field_filter` object.
#'
#' @examples
#' strand_field_filter("3f1d0c9e-3f8e-4b0b-9f6a-2a1c9f7b4d55", "eq", "phase-2")
#' @export
strand_field_filter <- function(field_id, operator, value = NULL) {
  if (!is.character(field_id) || length(field_id) != 1L || is.na(field_id) ||
      !nzchar(trimws(field_id))) {
    stop("field_id must be a single custom-field definition id", call. = FALSE)
  }
  if (!is.character(operator) || length(operator) != 1L || is.na(operator) ||
      !(operator %in% .strand_field_operators)) {
    stop(
      sprintf("operator must be one of: %s", paste(.strand_field_operators, collapse = ", ")),
      call. = FALSE
    )
  }
  valueless <- operator %in% .strand_valueless_field_operators
  if (valueless && !is.null(value)) {
    stop(sprintf("%s takes no value", operator), call. = FALSE)
  }
  if (!valueless) {
    if (is.null(value) || length(value) != 1L || is.na(value) ||
        !(is.character(value) || is.numeric(value) || is.logical(value))) {
      stop(
        sprintf("%s requires a single string, number, or logical value", operator),
        call. = FALSE
      )
    }
  }
  structure(
    list(field_id = trimws(field_id), operator = operator, value = value),
    class = "strand_field_filter"
  )
}

.strand_marker_param <- function(clause) {
  if (!inherits(clause, "strand_marker_filter")) {
    stop("marker must be built with strand_marker_filter()", call. = FALSE)
  }
  paste0(clause$marker, ":", clause$state)
}

.strand_field_param <- function(clause) {
  if (!inherits(clause, "strand_field_filter")) {
    stop("field must be built with strand_field_filter()", call. = FALSE)
  }
  if (clause$operator %in% .strand_valueless_field_operators) {
    return(paste0(clause$field_id, ":", clause$operator))
  }
  # JSON keeps the value's type; percent-encoding keeps the ':' clause
  # delimiter unambiguous. Same encoding the Strand web app writes into a URL.
  # digits = NA keeps a numeric value exact (the default rounds to 4 decimals
  # and would filter on a different number than the caller passed).
  # repeated = TRUE forces encoding of a value that already looks
  # percent-encoded, which URLencode otherwise passes through untouched.
  encoded <- utils::URLencode(
    as.character(jsonlite::toJSON(clause$value, auto_unbox = TRUE, digits = NA)),
    reserved = TRUE,
    repeated = TRUE
  )
  paste0(clause$field_id, ":", clause$operator, ":", encoded)
}

.strand_clause_params <- function(value, builder, mapper) {
  if (is.null(value)) return(NULL)
  clauses <- if (inherits(value, builder)) list(value) else value
  if (!is.list(clauses)) {
    stop(sprintf("expected %s clauses built with the matching helper", builder), call. = FALSE)
  }
  if (length(clauses) == 0L) return(NULL)
  vapply(clauses, mapper, character(1))
}

.strand_validate_enum_facet <- function(values, name, allowed) {
  if (is.null(values)) return(NULL)
  if (!is.character(values) || anyNA(values)) {
    stop(sprintf("%s must be a character vector", name), call. = FALSE)
  }
  if (length(values) == 0L) return(NULL)
  unknown <- setdiff(values, allowed)
  if (length(unknown) > 0L) {
    stop(
      sprintf("%s values must be among: %s", name, paste(allowed, collapse = ", ")),
      call. = FALSE
    )
  }
  unique(values)
}

.strand_validate_date_facet <- function(value, name) {
  if (is.null(value)) return(NULL)
  if (inherits(value, "Date")) {
    if (length(value) != 1L || is.na(value)) {
      stop(sprintf("%s must be a single date", name), call. = FALSE)
    }
    return(format(value, "%Y-%m-%d"))
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !grepl("^\\d{4}-\\d{2}-\\d{2}$", value) ||
      is.na(as.Date(value, format = "%Y-%m-%d"))) {
    stop(sprintf("%s must be a Date or a YYYY-MM-DD string", name), call. = FALSE)
  }
  value
}

#' List samples
#'
#' Lists owned samples, Strand's public cohort, or both through one
#' cursor-paginated collection. Public and owned records use distinct shapes;
#' inspect each item's mandatory `ownership` field before using branch fields.
#'
#' The filter facets are the same ones the Strand web app puts in a `/samples`
#' URL, so a link and an API call describe the same set. Facets narrow
#' together; `status` and `workflow` values widen (OR) while repeated `marker`
#' and `field` clauses narrow (AND). Tags are a normal field clause bound to
#' the predefined Tags definition id. Public releases support `q` title search
#' only. Under `scope = "all"`, every non-`q` facet suppresses the public block
#' while still filtering owned samples.
#' Under `scope = "public"`, those facets are rejected as unanswerable.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param scope Exactly one of `"mine"`, `"public"`, or `"all"`. The default
#'   lists owned samples. Values are case-sensitive and are never partially
#'   matched.
#' @param limit Items per page, from 1 through 100.
#' @param cursor Opaque cursor returned as `next_cursor` by the previous call,
#'   or `NULL` for the first page.
#' @param q Optional case-insensitive substring match on name or filename.
#' @param status Optional lifecycle statuses: any of `"ready"`,
#'   `"preprocessing"`, `"uploading"`, `"upload_failed"`,
#'   `"preprocess_failed"`.
#' @param uploaded_from,uploaded_to Optional inclusive upload-date bounds, as
#'   `Date` values or `YYYY-MM-DD` strings.
#' @param workflow Optional job states: any of `"ready_no_job"`,
#'   `"in_flight"`, `"has_results"`.
#' @param marker Optional marker-state clauses from [strand_marker_filter()],
#'   singly or as a list.
#' @param field Optional custom-field clauses from [strand_field_filter()],
#'   singly or as a list.
#'
#' @return A named list with `items`, `next_cursor`, and canonical `app_url`.
#'   Every item has `ownership`, canonical `id`, and its own `app_url`. Owned
#'   items also have `name`, `filename`,
#'   `status`, numeric `file_size`, `fields`, and `created_at`. Public items also
#'   have `title` and `thumbnail_url`.
#'
#' @examples
#' \dontrun{
#' client <- strand_client()
#' page <- strand_samples_list(client, scope = "public", q = "TCGA-COAD")
#' ids <- vapply(page$items, function(sample) sample$id, character(1))
#'
#' ready <- strand_samples_list(
#'   client,
#'   status = "ready",
#'   marker = strand_marker_filter("CD20")
#' )
#' }
#' @export
strand_samples_list <- function(client, scope = "mine", limit = 48L,
                                cursor = NULL, q = NULL,
                                status = NULL, uploaded_from = NULL,
                                uploaded_to = NULL, workflow = NULL,
                                marker = NULL, field = NULL) {
  if (!inherits(client, "strand_client")) {
    stop("client must be a strand_client (see strand_client())", call. = FALSE)
  }
  scope <- .strand_validate_scope(scope)
  limit <- .strand_validate_sample_limit(limit)

  query <- list(scope = scope, limit = limit)
  if (!is.null(cursor)) query$cursor <- cursor
  if (!is.null(q)) {
    if (!is.character(q) || length(q) != 1L || is.na(q)) {
      stop("q must be a single search string", call. = FALSE)
    }
    query$q <- q
  }
  query$status <- .strand_validate_enum_facet(
    status, "status",
    c("ready", "preprocessing", "uploading", "upload_failed", "preprocess_failed")
  )
  query$uploaded_from <- .strand_validate_date_facet(uploaded_from, "uploaded_from")
  query$uploaded_to <- .strand_validate_date_facet(uploaded_to, "uploaded_to")
  query$workflow <- .strand_validate_enum_facet(
    workflow, "workflow", c("ready_no_job", "in_flight", "has_results")
  )
  query$marker <- .strand_clause_params(
    marker, "strand_marker_filter", .strand_marker_param
  )
  query$field <- .strand_clause_params(
    field, "strand_field_filter", .strand_field_param
  )
  query <- query[!vapply(query, is.null, logical(1))]

  raw <- strand_perform_json(client, "samples", query = query)
  list(
    items = lapply(raw$items %||% list(), .strand_parse_sample_list_item),
    next_cursor = strand_as_scalar_chr(raw$nextCursor),
    app_url = strand_as_scalar_chr(raw$appUrl)
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
    app_url = strand_as_scalar_chr(raw$appUrl),
    name = strand_as_scalar_chr(raw$name),
    filename = strand_as_scalar_chr(raw$filename),
    status = strand_as_scalar_chr(raw$status),
    file_size = .strand_file_size(raw$fileSize),
    width_px = strand_as_scalar_int(raw$widthPx),
    height_px = strand_as_scalar_int(raw$heightPx),
    mpp = mpp,
    fields = .strand_fields(raw$fields),
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
    app_url = strand_as_scalar_chr(raw$appUrl),
    title = strand_as_scalar_chr(raw$title),
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
#'   with `ownership = "public"`, canonical share `id`, `title`, `geometry`,
#'   `markers`, `thumbnail_url`, and `pyramid_url`.
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
#' Updates any combination of display name, unified field values, and physical
#' pixel size in one request. `NULL` arguments are omitted. Use
#' `clear_name = TRUE` to send JSON `null` and restore the filename. Field
#' values are keyed by stable key; `fields = list(tags = list())` clears Tags.
#'
#' @param client A `strand_client` from [strand_client()].
#' @param sample_id UUID of an owned sample.
#' @param name New display name, or `NULL` to leave it unchanged.
#' @param fields Named list of complete field values keyed by stable key, or
#'   `NULL` to leave every field unchanged.
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
#' strand_patch_sample(client, sample_id, fields = list(tags = c("baseline", "responding")))
#' strand_patch_sample(client, sample_id, fields = list(tags = list()), mpp = 0.26)
#' strand_patch_sample(client, sample_id, clear_name = TRUE)
#' }
#' @export
strand_patch_sample <- function(client, sample_id, name = NULL, fields = NULL,
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
  if (!is.null(fields) &&
      (!is.list(fields) || length(fields) == 0L || is.null(names(fields)) || any(!nzchar(names(fields))))) {
    stop("fields must be NULL or a non-empty named list keyed by field key", call. = FALSE)
  }

  body <- list()
  if (isTRUE(clear_name)) {
    body["name"] <- list(NULL)
  } else if (!is.null(name)) {
    body$name <- name
  }
  if (!is.null(fields)) body$fields <- fields
  if (!is.null(mpp)) body$mpp <- strand_validate_mpp(mpp, "mpp")
  if (length(body) == 0L) {
    stop("at least one of name, fields, mpp, or clear_name must change", call. = FALSE)
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

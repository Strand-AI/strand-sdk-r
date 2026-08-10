# Internal HTTP helpers — request building + typed error mapping.

strand_req <- function(client, path) {
  path <- sub("^/+", "", path)
  req <- httr2::request(paste0(client$api_root, "/", path))
  req <- httr2::req_headers(req,
                            Authorization = paste("Bearer", client$api_key),
                            Accept = "application/json")
  req <- httr2::req_user_agent(req, client$user_agent)
  req <- httr2::req_timeout(req, client$timeout)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req
}

# Perform an authenticated request and return parsed JSON.
strand_perform_json <- function(client, path, method = "GET",
                                body = NULL, query = NULL, expected = 200L) {
  req <- strand_req(client, path)
  req <- httr2::req_method(req, method)
  if (!is.null(query) && length(query) > 0L) {
    req <- do.call(httr2::req_url_query, c(list(.req = req), query))
  }
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  }
  resp <- httr2::req_perform(req)
  strand_raise_for_error(resp)
  status <- httr2::resp_status(resp)
  if (!(status %in% expected)) {
    stop(sprintf("Unexpected HTTP status %d for %s %s", status, method, path),
         call. = FALSE)
  }
  jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
}

strand_perform_raw <- function(client, path) {
  req <- strand_req(client, path)
  resp <- httr2::req_perform(req)
  strand_raise_for_error(resp)
  httr2::resp_body_raw(resp)
}

# Map documented response codes onto the package's typed conditions.
strand_raise_for_error <- function(resp) {
  status <- httr2::resp_status(resp)
  if (status < 400) return(invisible(NULL))

  body <- tryCatch(
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE),
    error = function(e) list()
  )
  message <- body$message %||% body$error %||% sprintf("HTTP %d", status)
  error_code <- if (is.character(body$error)) body$error else NA_character_
  is_unknown_markers <- status == 400L && identical(error_code, "unknown_markers")
  cls <- c(
    if (is_unknown_markers) "strand_unknown_markers_error" else NULL,
    switch(as.character(status),
           "400" = "strand_bad_request_error",
           "401" = "strand_auth_error",
           "402" = "strand_insufficient_credits_error",
           "404" = "strand_not_found_error",
           NULL),
    "strand_api_error", "error", "condition"
  )

  cond <- list(
    message = message,
    status_code = status,
    error_code = error_code,
    body = body
  )

  if (is_unknown_markers) {
    cond$unknown <- vapply(body$unknownMarkers %||% list(), as.character, character(1))
    known_raw <- body$knownMarkersSample
    cond$known_subset <- if (is.null(known_raw)) NULL
                         else vapply(known_raw, as.character, character(1))
  }
  if (status == 402L) {
    cond$required <- body$required
  }
  stop(structure(cond, class = cls))
}

# Tiny in-process HTTP server for unit tests. Uses webfakes (CRAN package).
#
# Each test boots a server, drives the client at it, and shuts down. This keeps
# `R CMD check --as-cran` clean (no network) and avoids brittle global state.

skip_if_no_webfakes <- function() {
  testthat::skip_if_not_installed("webfakes")
}

start_strand_server <- function(app) {
  testthat::skip_if_not_installed("webfakes")
  server <- webfakes::new_app_process(app)
  withr::defer(server$stop(), envir = parent.frame())
  server
}

testing_client <- function(server) {
  strand_client(api_key = "sk-strand-test", base_url = server$url())
}

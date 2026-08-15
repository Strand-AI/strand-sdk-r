# Strand AI Platform R client — top-level constructor.

#' Create a Strand AI Platform client
#'
#' Constructs a client object that bundles authentication, base URL, and
#' a shared `httr2` request template. The returned list is passed to every
#' other function in this package.
#'
#' Default base URL is `https://app.strandai.com`. The `/api/v1/` path is
#' appended automatically; pass the root site URL, not the API root.
#'
#' @param api_key Bearer API key (`sk-strand-...`). Defaults to the
#'   `STRAND_API_KEY` environment variable.
#' @param base_url Platform base URL (without `/api/v1`). Defaults to
#'   `STRAND_BASE_URL` env var, then `https://app.strandai.com`.
#' @param timeout Per-request timeout in seconds.
#' @param user_agent Override the default User-Agent string.
#'
#' @return An object of class `strand_client`: a list with `api_key`,
#'   `base_url`, `api_root`, `timeout`, and `user_agent`.
#' @examples
#' \dontrun{
#' client <- strand_client()  # reads STRAND_API_KEY
#' client <- strand_client(api_key = "sk-strand-...",
#'                          base_url = "http://localhost:3000")
#' }
#' @export
strand_client <- function(api_key = NULL,
                          base_url = NULL,
                          timeout = 60,
                          user_agent = NULL) {
  api_key <- api_key %||% Sys.getenv("STRAND_API_KEY", unset = NA_character_)
  if (is.na(api_key) || !nzchar(api_key)) {
    stop("No API key provided. Pass api_key=... or set STRAND_API_KEY.", call. = FALSE)
  }
  base_url <- base_url %||% Sys.getenv("STRAND_BASE_URL",
                                       unset = "https://app.strandai.com")
  base_url <- sub("/+$", "", base_url)

  structure(
    list(
      api_key = api_key,
      base_url = base_url,
      api_root = paste0(base_url, "/api/v1"),
      timeout = timeout,
      user_agent = user_agent %||% paste0("strandai-r/", utils::packageVersion("strandai"))
    ),
    class = "strand_client"
  )
}

#' @export
print.strand_client <- function(x, ...) {
  cat("<strand_client>\n")
  cat("  base_url: ", x$base_url, "\n", sep = "")
  cat("  api_key:  ", strand_key_redact(x$api_key), "\n", sep = "")
  invisible(x)
}

strand_key_redact <- function(key) {
  if (nchar(key) <= 12) return(strrep("*", nchar(key)))
  paste0(substr(key, 1, 12), "...", substr(key, nchar(key) - 3, nchar(key)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

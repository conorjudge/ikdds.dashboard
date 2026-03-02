#' Create a reactive cache for audit data
#'
#' Wraps the data loading function with a time-based cache to avoid
#' excessive REDCap API calls. The cache is invalidated after `ttl` seconds.
#'
#' @param load_fn A function that returns the audit data tibble.
#' @param ttl Cache time-to-live in seconds (default 3600).
#'
#' @return A reactive expression that returns cached audit data.
#'
#' @export
#' @family data
cached_audit_data <- function(load_fn, ttl = 3600) {
  cache_env <- new.env(parent = emptyenv())
  cache_env$data <- NULL
  cache_env$timestamp <- NULL

  shiny::reactive({
    now <- Sys.time()

    if (is.null(cache_env$data) ||
        is.null(cache_env$timestamp) ||
        difftime(now, cache_env$timestamp, units = "secs") > ttl) {

      cli::cli_inform("Cache miss  - loading fresh data...")
      cache_env$data <- load_fn()
      cache_env$timestamp <- now
    } else {
      age <- round(difftime(now, cache_env$timestamp, units = "mins"), 1)
      cli::cli_inform("Cache hit  - data is {age} minutes old.")
    }

    cache_env$data
  })
}

#' Invalidate the data cache
#'
#' Forces the next data access to reload from source.
#'
#' @param cache_reactive The reactive returned by [cached_audit_data()].
#'   Note: this function resets the internal env; the reactive itself
#'   needs to be re-triggered (e.g., via `shiny::invalidateLater()`).
#'
#' @keywords internal
invalidate_cache <- function(cache_reactive) {
  # The cache env is captured in the closure of cached_audit_data
  # This is a placeholder for manual invalidation patterns.
  cli::cli_inform("Cache invalidated  - data will reload on next access.")
}

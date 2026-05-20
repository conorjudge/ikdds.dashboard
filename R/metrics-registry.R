#' Metrics Registry
#'
#' Reads the centralised metric definitions from `inst/metrics/metrics.yml`.
#' The result is cached so the file is only read once per R session.
#'
#' @return A named list of metric configurations.
#'
#' @export
#' @family metrics-engine
metrics_registry <- function() {
  # Return cached registry if available
  reg <- .registry_env$cache
  if (!is.null(reg)) {
    return(reg)
  }

  yml_path <- system.file("metrics", "metrics.yml", package = "ikdds.dashboard")

  if (yml_path == "") {
    # Fallback for development: try relative path
    yml_path <- file.path("inst", "metrics", "metrics.yml")
  }

  if (!file.exists(yml_path)) {
    cli::cli_abort("Metrics YAML not found at {yml_path}")
  }


  reg <- yaml::read_yaml(yml_path)
  .registry_env$cache <- reg
  reg
}

#' Get configuration for a single metric
#'
#' Convenience wrapper around [metrics_registry()] for looking up one metric.
#'
#' @param metric_id Character string matching a key in `metrics.yml`.
#' @param registry Optional pre-loaded registry (avoids re-read).
#'
#' @return A list with the metric configuration.
#'
#' @export
#' @family metrics-engine
get_metric_config <- function(metric_id, registry = metrics_registry()) {
  cfg <- registry[[metric_id]]
  if (is.null(cfg)) {
    cli::cli_abort("Unknown metric ID: {metric_id}")
  }
  cfg
}

#' Reset the metrics registry cache
#'
#' Useful for testing or after modifying the YAML file.
#'
#' @return Invisible NULL.
#'
#' @export
#' @family metrics-engine
reset_registry_cache <- function() {
  .registry_env$cache <- NULL
  invisible(NULL)
}

# Internal environment for caching the registry
.registry_env <- new.env(parent = emptyenv())

#' Generate reproducible R code string for a metric
#'
#' Creates a string of valid R code that reproduces a given plot or table
#' using the ikdds.dashboard package functions.
#'
#' @param metric_fn Character name of the metric function (e.g.,
#'   `"compute_urr_achievement"`).
#' @param plot_fn Character name of the plot function (e.g.,
#'   `"plot_caterpillar_proportion"`).
#' @param additional_args Character string of additional arguments.
#' @param filters Named list of active filters (centre_code, consultant).
#'
#' @return A character string of reproducible R code.
#'
#' @export
#' @family plots
generate_plot_code <- function(metric_fn, plot_fn,
                                additional_args = "",
                                filters = list()) {
  filter_code <- ""
  if (length(filters) > 0) {
    filter_parts <- character()
    if (!is.null(filters$centre_code) && length(filters$centre_code) > 0) {
      centres_str <- paste0('"', filters$centre_code, '"', collapse = ", ")
      filter_parts <- c(filter_parts,
        glue::glue('  dplyr::filter(centre_code %in% c({centres_str}))'))
    }
    if (!is.null(filters$consultant) && length(filters$consultant) > 0) {
      consult_str <- paste0('"', filters$consultant, '"', collapse = ", ")
      filter_parts <- c(filter_parts,
        glue::glue('  dplyr::filter(consultant %in% c({consult_str}))'))
    }
    if (length(filter_parts) > 0) {
      filter_code <- paste0(
        " |>\n",
        paste(filter_parts, collapse = " |>\n")
      )
    }
  }

  code <- glue::glue(
    'library(ikdds.dashboard)\n',
    '\n',
    '# Load data\n',
    'config <- dashboard_config()\n',
    'df <- get_audit_data(config){filter_code}\n',
    '\n',
    '# Compute metric\n',
    'metric_data <- {metric_fn}(df{additional_args})\n',
    '\n',
    '# Create plot\n',
    'p <- {plot_fn}(metric_data)\n',
    'print(p)\n'
  )

  as.character(code)
}

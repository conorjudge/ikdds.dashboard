#' Compute national average for proportion data
#'
#' Weighted average proportion across all centres.
#'
#' @param metric_df Tibble from `compute_centre_proportion()`.
#'
#' @return Numeric proportion (0-1 scale).
#'
#' @export
#' @family plot-helpers
national_average_proportion <- function(metric_df) {
  total_x <- sum(metric_df$x, na.rm = TRUE)
  total_n <- sum(metric_df$n, na.rm = TRUE)
  if (total_n == 0) return(NA_real_)
  total_x / total_n
}

#' Compute national average for median data
#'
#' Simple mean of centre medians, weighted by sample size.
#'
#' @param metric_df Tibble from `compute_centre_median()`.
#'
#' @return Numeric value.
#'
#' @export
#' @family plot-helpers
national_average_median <- function(metric_df) {
  if (nrow(metric_df) == 0) return(NA_real_)
  valid <- !is.na(metric_df$median) & !is.na(metric_df$n)
  if (!any(valid)) return(NA_real_)
  stats::weighted.mean(metric_df$median[valid], metric_df$n[valid])
}

#' Order centres for caterpillar plot
#'
#' Arranges centres by the point estimate for consistent caterpillar ordering.
#'
#' @param metric_df Tibble with `centre_code` and a value column.
#' @param value_col Name of the value column to sort by.
#'
#' @return Factor vector of ordered centre codes.
#'
#' @keywords internal
order_centres <- function(metric_df, value_col = "proportion") {
  ordered <- metric_df |>
    dplyr::arrange(.data[[value_col]])

  factor(metric_df$centre_code, levels = ordered$centre_code)
}

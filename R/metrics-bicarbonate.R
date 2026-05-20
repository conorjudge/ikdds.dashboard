#' Compute potassium in range by centre
#'
#' Proportion with potassium 4-6 mmol/L.
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-bicarbonate
compute_potassium_achievement <- function(df, group_col = "centre_code") {
  compute_centre_proportion(df, .data$qbla9, lower = 4, upper = 6,
                             metric_label = "K+ 4-6 mmol/L",
                             group_col = group_col)
}

#' Compute bicarbonate in range by centre
#'
#' Proportion with bicarbonate 18-26 mmol/L.
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-bicarbonate
compute_bicarbonate_achievement <- function(df, group_col = "centre_code") {
  compute_centre_proportion(df, .data$qbla4, lower = 18, upper = 26,
                             metric_label = "HCO3 18-26 mmol/L",
                             group_col = group_col)
}

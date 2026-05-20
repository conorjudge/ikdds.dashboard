#' Compute pre-HD blood pressure achievement by centre
#'
#' Proportion of patients with pre-HD SBP <140 AND DBP <90.
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()] with Wilson CIs.
#'
#' @export
#' @family metrics-bp
compute_pre_bp_achievement <- function(df, group_col = "centre_code") {
  df_bp <- df |>
    dplyr::filter(!is.na(.data$qblg3) & !is.na(.data$qblg4)) |>
    dplyr::mutate(
      bp_achieved = as.numeric(.data$qblg3 < 140 & .data$qblg4 < 90)
    )

  compute_centre_proportion(df_bp, .data$bp_achieved, lower = 1, upper = 1,
                             metric_label = "Pre-HD BP <140/90",
                             group_col = group_col)
}

#' Compute post-HD blood pressure achievement by centre
#'
#' Proportion of patients with post-HD SBP <130 AND DBP <80.
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()] with Wilson CIs.
#'
#' @export
#' @family metrics-bp
compute_post_bp_achievement <- function(df, group_col = "centre_code") {
  df_bp <- df |>
    dplyr::filter(!is.na(.data$qblg6) & !is.na(.data$qblg7)) |>
    dplyr::mutate(
      bp_achieved = as.numeric(.data$qblg6 < 130 & .data$qblg7 < 80)
    )

  compute_centre_proportion(df_bp, .data$bp_achieved, lower = 1, upper = 1,
                             metric_label = "Post-HD BP <130/80",
                             group_col = group_col)
}

#' Compute blood pressure summary statistics by centre
#'
#' Returns mean/median pre and post-HD SBP and DBP per centre.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with summary BP stats per centre.
#'
#' @export
#' @family metrics-bp
compute_bp_summary <- function(df) {
  df |>
    dplyr::group_by(.data$centre_code, .data$centre_name) |>
    dplyr::summarise(
      n = dplyr::n(),
      pre_sbp_mean   = round(mean(.data$qblg3, na.rm = TRUE), 1),
      pre_sbp_median = round(stats::median(.data$qblg3, na.rm = TRUE), 1),
      pre_dbp_mean   = round(mean(.data$qblg4, na.rm = TRUE), 1),
      pre_dbp_median = round(stats::median(.data$qblg4, na.rm = TRUE), 1),
      post_sbp_mean   = round(mean(.data$qblg6, na.rm = TRUE), 1),
      post_sbp_median = round(stats::median(.data$qblg6, na.rm = TRUE), 1),
      post_dbp_mean   = round(mean(.data$qblg7, na.rm = TRUE), 1),
      post_dbp_median = round(stats::median(.data$qblg7, na.rm = TRUE), 1),
      .groups = "drop"
    )
}

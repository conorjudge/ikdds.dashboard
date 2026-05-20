#' Compute vascular access type distribution by centre
#'
#' Returns counts and proportions for AVF, AVG, and catheter access.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `access_type`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-access
compute_access_distribution <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$qhd20)) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$qhd20) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::rename(access_type = "qhd20") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$centre_code, .data$access_type)
}

#' Compute AVF rate by centre (for funnel plot)
#'
#' Returns the proportion of patients with AVF access per centre.
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble with Wilson CIs for AVF proportion.
#'
#' @export
#' @family metrics-access
compute_avf_rate <- function(df, group_col = "centre_code") {
  df_access <- df |>
    dplyr::filter(!is.na(.data$qhd20)) |>
    dplyr::mutate(is_avf = as.numeric(.data$qhd20 == "AVF"))

  compute_centre_proportion(df_access, .data$is_avf, lower = 1, upper = 1,
                             metric_label = "AVF Rate",
                             group_col = group_col)
}

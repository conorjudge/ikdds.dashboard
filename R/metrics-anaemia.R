#' Compute haemoglobin median by centre
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_median()].
#'
#' @export
#' @family metrics-anaemia
compute_hb_median <- function(df, group_col = "centre_code") {
  compute_centre_median(df, .data$qble1, metric_label = "Hb Median (g/dL)",
                         group_col = group_col)
}

#' Compute haemoglobin 10-12 g/dL achievement by centre
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-anaemia
compute_hb_achievement <- function(df, group_col = "centre_code") {
  compute_centre_proportion(df, .data$qble1, lower = 10, upper = 12,
                             metric_label = "Hb 10-12 g/dL",
                             group_col = group_col)
}

#' Compute haemoglobin distribution data by centre
#'
#' Returns binned Hb counts for histogram/density plotting.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `hb_band`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-anaemia
compute_hb_distribution <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$qble1)) |>
    dplyr::mutate(
      hb_band = cut(
        .data$qble1,
        breaks = c(0, 8, 9, 10, 11, 12, 13, Inf),
        labels = c("<8", "8-9", "9-10", "10-11", "11-12", "12-13", ">13"),
        right = FALSE
      )
    ) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$hb_band) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup()
}

#' Compute ferritin >=200 ug/L achievement by centre
#'
#' @param df Audit data tibble.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-anaemia
compute_ferritin_achievement <- function(df, group_col = "centre_code") {
  compute_centre_proportion(df, .data$qblf1, lower = 200, upper = Inf,
                             metric_label = "Ferritin >=200 ug/L",
                             group_col = group_col)
}

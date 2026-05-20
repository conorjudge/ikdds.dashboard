#' Validate machine-populated fields
#'
#' Replaces zero values with NA for fields that should not be zero
#' (e.g., URR, blood pressure). A URR of 0 or BP of 0 indicates
#' missing/invalid machine data rather than a true clinical value.
#'
#' @param df Audit data tibble.
#'
#' @return The tibble with 0 values replaced by NA in machine fields.
#'
#' @export
#' @family metrics-dialysis
validate_machine_fields <- function(df) {
  machine_fields <- c("qblg9", "qblg3", "qblg4", "qblg6", "qblg7",
                       "hdp02")
  for (field in intersect(machine_fields, names(df))) {
    df[[field]] <- ifelse(df[[field]] == 0, NA_real_, df[[field]])
  }
  df
}

#' Compute URR median by centre
#'
#' @param df Audit data tibble.
#' @param filter_3x If `TRUE`, only include patients on 3x/week HD.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_median()].
#'
#' @export
#' @family metrics-dialysis
compute_urr_median <- function(df, filter_3x = FALSE,
                                group_col = "centre_code") {
  if (filter_3x) {
    df <- df |> dplyr::filter(.data$hdp01 == 3)
  }
  label <- if (filter_3x) "URR Median (3x/wk)" else "URR Median (All)"
  compute_centre_median(df, .data$qblg9, metric_label = label,
                         group_col = group_col)
}

#' Compute URR >65% achievement by centre
#'
#' @param df Audit data tibble.
#' @param filter_3x If `TRUE`, only include patients on 3x/week HD.
#' @param group_col Grouping column: `"centre_code"` or `"consultant"`.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-dialysis
compute_urr_achievement <- function(df, filter_3x = FALSE,
                                     group_col = "centre_code") {
  if (filter_3x) {
    df <- df |> dplyr::filter(.data$hdp01 == 3)
  }
  label <- if (filter_3x) "URR >65% (3x/wk)" else "URR >65% (All)"
  # Use 65.001 to enforce strict >65% (compute_centre_proportion uses >=)
  compute_centre_proportion(df, .data$qblg9, lower = 65.001, upper = Inf,
                             metric_label = label, group_col = group_col)
}

#' Compute session frequency distribution by centre
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `hd_freq`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-dialysis
compute_session_frequency <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$hdp01)) |>
    dplyr::mutate(
      hd_freq = dplyr::case_when(
        .data$hdp01 < 3  ~ "<3x/week",
        .data$hdp01 == 3 ~ "3x/week",
        .data$hdp01 > 3  ~ ">3x/week"
      )
    ) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$hd_freq) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup()
}

#' Compute session duration distribution by centre
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `duration_band`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-dialysis
compute_session_duration <- function(df) {
  df |>
    dplyr::filter(!is.na(.data$hdp02)) |>
    dplyr::mutate(
      duration_band = cut(
        .data$hdp02,
        breaks = c(0, 180, 210, 240, 270, Inf),
        labels = c("<3hrs", "3-3.5hrs", "3.5-4hrs", "4-4.5hrs", ">4.5hrs"),
        right = FALSE
      )
    ) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$duration_band) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup()
}

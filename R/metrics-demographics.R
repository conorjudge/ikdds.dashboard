#' Compute patient counts by centre
#'
#' @param df Audit data tibble from [get_audit_data()].
#'
#' @return A tibble with `centre_code`, `centre_name`, `n_patients`.
#'
#' @export
#' @family metrics-demographics
compute_patient_counts <- function(df) {
  df |>
    dplyr::group_by(.data$centre_code, .data$centre_name) |>
    dplyr::summarise(n_patients = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(.data$n_patients))
}

#' Compute age distribution by centre
#'
#' Returns age summary statistics and binned counts per centre.
#'
#' @param df Audit data tibble.
#'
#' @return A list with:
#'   - `summary`: tibble of mean, median, SD, min, max per centre
#'   - `bins`: tibble of age band counts per centre
#'
#' @export
#' @family metrics-demographics
compute_age_distribution <- function(df) {
  summary_tbl <- df |>
    dplyr::group_by(.data$centre_code, .data$centre_name) |>
    dplyr::summarise(
      n       = dplyr::n(),
      mean    = round(mean(.data$age, na.rm = TRUE)),
      median  = round(stats::median(.data$age, na.rm = TRUE)),
      q1      = round(stats::quantile(.data$age, 0.25, na.rm = TRUE)),
      q3      = round(stats::quantile(.data$age, 0.75, na.rm = TRUE)),
      iqr     = round(stats::IQR(.data$age, na.rm = TRUE)),
      min     = round(min(.data$age, na.rm = TRUE)),
      max     = round(max(.data$age, na.rm = TRUE)),
      .groups = "drop"
    )

  bins_tbl <- df |>
    dplyr::mutate(
      age_band = cut(
        .data$age,
        breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
        labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
        right = FALSE
      )
    ) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$age_band) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup()

  list(summary = summary_tbl, bins = bins_tbl)
}

#' Compute ethnicity distribution by centre
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `ethnicity`,
#'   `count`, `pct`, `pct_missing`.
#'
#' @export
#' @family metrics-demographics
compute_ethnicity_breakdown <- function(df) {
  if (!"ethnicity" %in% names(df)) {
    return(tibble::tibble(
      centre_code = character(), centre_name = character(),
      ethnicity = character(), count = integer(), pct = numeric(),
      pct_missing = numeric()
    ))
  }

  # Compute missing rate
  missing_info <- df |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::summarise(
      pct_missing = round(sum(is.na(.data$ethnicity)) / dplyr::n() * 100),
      .groups = "drop"
    )

  df |>
    dplyr::filter(!is.na(.data$ethnicity)) |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$ethnicity) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100)) |>
    dplyr::ungroup() |>
    dplyr::left_join(missing_info, by = "centre_code")
}

#' Compute gender breakdown by centre
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `gender`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-demographics
compute_gender_breakdown <- function(df) {
  df |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$gender) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup()
}

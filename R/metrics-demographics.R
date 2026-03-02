#' Compute patient counts by centre
#'
#' @param df Audit data tibble from [get_audit_data()].
#'
#' @return A tibble with `centre_code`, `centre_name`, `n_patients`.
#'
#' @export
#' @family metrics-demographics
compute_patient_counts <- function(df) {
  df %>%
    dplyr::group_by(.data$centre_code, .data$centre_name) %>%
    dplyr::summarise(n_patients = dplyr::n(), .groups = "drop") %>%
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
  summary_tbl <- df %>%
    dplyr::group_by(.data$centre_code, .data$centre_name) %>%
    dplyr::summarise(
      n       = dplyr::n(),
      mean    = round(mean(.data$age, na.rm = TRUE), 1),
      median  = round(stats::median(.data$age, na.rm = TRUE), 1),
      sd      = round(stats::sd(.data$age, na.rm = TRUE), 1),
      min     = min(.data$age, na.rm = TRUE),
      max     = max(.data$age, na.rm = TRUE),
      .groups = "drop"
    )

  bins_tbl <- df %>%
    dplyr::mutate(
      age_band = cut(
        .data$age,
        breaks = c(0, 30, 40, 50, 60, 70, 80, Inf),
        labels = c("<30", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
        right = FALSE
      )
    ) %>%
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$age_band) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$centre_code) %>%
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) %>%
    dplyr::ungroup()

  list(summary = summary_tbl, bins = bins_tbl)
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
  df %>%
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$gender) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$centre_code) %>%
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) %>%
    dplyr::ungroup()
}

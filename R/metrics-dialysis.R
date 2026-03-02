#' Compute URR median by centre
#'
#' @param df Audit data tibble.
#' @param filter_3x If `TRUE`, only include patients on 3x/week HD.
#'
#' @return A tibble from [compute_centre_median()].
#'
#' @export
#' @family metrics-dialysis
compute_urr_median <- function(df, filter_3x = FALSE) {
  if (filter_3x) {
    df <- df %>% dplyr::filter(.data$hdp01 == 3)
  }
  label <- if (filter_3x) "URR Median (3x/wk)" else "URR Median (All)"
  compute_centre_median(df, .data$qblg9, metric_label = label)
}

#' Compute URR >65% achievement by centre
#'
#' @param df Audit data tibble.
#' @param filter_3x If `TRUE`, only include patients on 3x/week HD.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-dialysis
compute_urr_achievement <- function(df, filter_3x = FALSE) {
  if (filter_3x) {
    df <- df %>% dplyr::filter(.data$hdp01 == 3)
  }
  label <- if (filter_3x) "URR >65% (3x/wk)" else "URR >65% (All)"
  compute_centre_proportion(df, .data$qblg9, lower = 65, upper = Inf,
                             metric_label = label)
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
  df %>%
    dplyr::filter(!is.na(.data$hdp01)) %>%
    dplyr::mutate(
      hd_freq = dplyr::case_when(
        .data$hdp01 < 3  ~ "<3x/week",
        .data$hdp01 == 3 ~ "3x/week",
        .data$hdp01 > 3  ~ ">3x/week"
      )
    ) %>%
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$hd_freq) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$centre_code) %>%
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) %>%
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
  df %>%
    dplyr::filter(!is.na(.data$hdp02)) %>%
    dplyr::mutate(
      duration_band = cut(
        .data$hdp02,
        breaks = c(0, 180, 210, 240, 270, Inf),
        labels = c("<3hrs", "3-3.5hrs", "3.5-4hrs", "4-4.5hrs", ">4.5hrs"),
        right = FALSE
      )
    ) %>%
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$duration_band) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::group_by(.data$centre_code) %>%
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) %>%
    dplyr::ungroup()
}

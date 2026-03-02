#' Compute phosphate in range by centre
#'
#' Proportion with phosphate 1.1-1.7 mmol/L.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-biochemistry
compute_phosphate_achievement <- function(df) {
  compute_centre_proportion(df, .data$qblb1, lower = 1.1, upper = 1.7,
                             metric_label = "PO4 1.1-1.7 mmol/L")
}

#' Compute adjusted calcium in range by centre
#'
#' Proportion with adjusted calcium 2.2-2.5 mmol/L.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-biochemistry
compute_calcium_achievement <- function(df) {
  compute_centre_proportion(df, .data$qblb4, lower = 2.2, upper = 2.5,
                             metric_label = "Adj Ca 2.2-2.5 mmol/L")
}

#' Compute PTH in range by centre
#'
#' Proportion with PTH 16-72 pmol/L.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-biochemistry
compute_pth_achievement <- function(df) {
  compute_centre_proportion(df, .data$qblb9, lower = 16, upper = 72,
                             metric_label = "PTH 16-72 pmol/L")
}

#' Compute simultaneous CKD-MBD control by centre
#'
#' Proportion of patients achieving all three CKD-MBD targets
#' simultaneously: PO4 1.1-1.7, Ca 2.2-2.5, PTH 16-72.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble from [compute_centre_proportion()].
#'
#' @export
#' @family metrics-biochemistry
compute_ckd_mbd_simultaneous <- function(df) {
  df_mbd <- df %>%
    dplyr::filter(
      !is.na(.data$qblb1) & !is.na(.data$qblb4) & !is.na(.data$qblb9)
    ) %>%
    dplyr::mutate(
      all_in_range = as.numeric(
        .data$qblb1 >= 1.1 & .data$qblb1 <= 1.7 &
        .data$qblb4 >= 2.2 & .data$qblb4 <= 2.5 &
        .data$qblb9 >= 16  & .data$qblb9 <= 72
      )
    )

  compute_centre_proportion(df_mbd, .data$all_in_range, lower = 1, upper = 1,
                             metric_label = "Simultaneous CKD-MBD Control")
}

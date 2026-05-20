#' Compute ERA PRD classification proportions
#'
#' Maps PRD codes to ERA-EDTA groups and computes proportions by centre.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `prd_group`,
#'   `count`, `pct`.
#'
#' @export
#' @family metrics-prd
compute_prd_proportions <- function(df) {
  prd_lookup <- load_era_prd_codes()

  df_with_group <- df |>
    dplyr::mutate(code = as.character(.data$dxs01)) |>
    dplyr::left_join(
      prd_lookup |> dplyr::mutate(code = as.character(.data$code)),
      by = "code"
    ) |>
    dplyr::mutate(
      prd_group = dplyr::if_else(
        is.na(.data$group),
        "Unknown/Missing",
        .data$group
      )
    )

  df_with_group |>
    dplyr::group_by(.data$centre_code, .data$centre_name, .data$prd_group) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$centre_code, dplyr::desc(.data$count))
}

#' Compute national PRD summary
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `prd_group`, `count`, `pct`.
#'
#' @export
#' @family metrics-prd
compute_prd_national <- function(df) {
  prd_lookup <- load_era_prd_codes()

  df |>
    dplyr::mutate(code = as.character(.data$dxs01)) |>
    dplyr::left_join(
      prd_lookup |> dplyr::mutate(code = as.character(.data$code)),
      by = "code"
    ) |>
    dplyr::mutate(
      prd_group = dplyr::if_else(
        is.na(.data$group),
        "Unknown/Missing",
        .data$group
      )
    ) |>
    dplyr::group_by(.data$prd_group) |>
    dplyr::summarise(count = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(pct = round(.data$count / sum(.data$count) * 100, 1)) |>
    dplyr::arrange(dplyr::desc(.data$count))
}

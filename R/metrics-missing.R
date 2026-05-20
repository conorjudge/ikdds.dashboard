#' Compute missing data percentage per centre and variable
#'
#' Calculates the proportion of missing values for each clinical variable
#' at each centre.
#'
#' @param df Audit data tibble.
#' @param variables Character vector of column names to check. If `NULL`,
#'   uses all clinical variable columns.
#'
#' @return A tibble with `centre_code`, `centre_name`, `variable`,
#'   `n_total`, `n_missing`, `pct_missing`.
#'
#' @export
#' @family metrics-missing
compute_missing_data <- function(df, variables = NULL) {
  if (is.null(variables)) {
    variables <- c("qblg9", "qblg3", "qblg4", "qblg6", "qblg7",
                    "qblb1", "qblb4", "qblb9", "qbla9", "qbla4",
                    "qble1", "qblf1", "qhd20", "dxs01")
  }

  # Filter to variables that actually exist in the data

  variables <- intersect(variables, names(df))

  var_labels <- c(
    qblg9 = "URR", qblg3 = "Pre SBP", qblg4 = "Pre DBP",
    qblg6 = "Post SBP", qblg7 = "Post DBP",
    qblb1 = "Phosphate", qblb4 = "Adj. Calcium", qblb9 = "PTH",
    qbla9 = "Potassium", qbla4 = "Bicarbonate",
    qble1 = "Haemoglobin", qblf1 = "Ferritin",
    qhd20 = "Vascular Access", dxs01 = "PRD Code"
  )

  results <- list()

  for (v in variables) {
    label <- if (v %in% names(var_labels)) var_labels[[v]] else v

    result <- df |>
      dplyr::group_by(.data$centre_code, .data$centre_name) |>
      dplyr::summarise(
        n_total   = dplyr::n(),
        n_missing = sum(is.na(.data[[v]])),
        .groups   = "drop"
      ) |>
      dplyr::mutate(
        pct_missing = round(.data$n_missing / .data$n_total * 100, 1),
        variable    = label
      )

    results[[v]] <- result
  }

  dplyr::bind_rows(results)
}

#' Compute national missing data summary
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `variable`, `n_total`, `n_missing`, `pct_missing`.
#'
#' @export
#' @family metrics-missing
compute_missing_national <- function(df) {
  if (nrow(df) == 0) {
    return(tibble::tibble(
      variable = character(), n_total = integer(),
      n_missing = integer(), pct_missing = numeric()
    ))
  }

  variables <- c("qblg9", "qblg3", "qblg4", "qblg6", "qblg7",
                  "qblb1", "qblb4", "qblb9", "qbla9", "qbla4",
                  "qble1", "qblf1", "qhd20", "dxs01")
  variables <- intersect(variables, names(df))

  var_labels <- c(
    qblg9 = "URR", qblg3 = "Pre SBP", qblg4 = "Pre DBP",
    qblg6 = "Post SBP", qblg7 = "Post DBP",
    qblb1 = "Phosphate", qblb4 = "Adj. Calcium", qblb9 = "PTH",
    qbla9 = "Potassium", qbla4 = "Bicarbonate",
    qble1 = "Haemoglobin", qblf1 = "Ferritin",
    qhd20 = "Vascular Access", dxs01 = "PRD Code"
  )

  purrr::map_dfr(variables, function(v) {
    label <- if (v %in% names(var_labels)) var_labels[[v]] else v
    tibble::tibble(
      variable    = label,
      n_total     = nrow(df),
      n_missing   = sum(is.na(df[[v]])),
      pct_missing = round(sum(is.na(df[[v]])) / nrow(df) * 100, 1)
    )
  })
}

#' Compute unmapped patient counts per centre
#'
#' Identifies patients not mapped to a consultant per centre.
#'
#' @param df Audit data tibble.
#'
#' @return A tibble with `centre_code`, `centre_name`, `n_total`,
#'   `n_unmapped`, `pct_unmapped`.
#'
#' @export
#' @family metrics-missing
compute_unmapped_patients <- function(df) {
  if (!"consultant" %in% names(df)) {
    return(tibble::tibble(
      centre_code = character(), centre_name = character(),
      n_total = integer(), n_unmapped = integer(), pct_unmapped = numeric()
    ))
  }

  df |>
    dplyr::group_by(.data$centre_code, .data$centre_name) |>
    dplyr::summarise(
      n_total = dplyr::n(),
      n_unmapped = sum(is.na(.data$consultant)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_unmapped = round(.data$n_unmapped / .data$n_total * 100)
    ) |>
    dplyr::arrange(dplyr::desc(.data$pct_unmapped))
}

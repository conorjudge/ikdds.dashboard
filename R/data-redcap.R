#' Get audit data from REDCap
#'
#' Reads patient data from REDCap using the `ikdds` package, then selects
#' and transforms the relevant fields for the dashboard.
#'
#' @param config A `dashboard_config` object with REDCap credentials.
#'
#' @return A tibble in the same format as [generate_synthetic_data()].
#'
#' @keywords internal
get_redcap_audit_data <- function(config) {
  if (!requireNamespace("ikdds", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg ikdds} package is required for REDCap data access.",
      "i" = "Install with: {.code devtools::install('../ikdds_r/ikdds')}"
    ))
  }

  # Build ikdds config from dashboard config
  ikdds_config <- list(
    redcap_uri  = config$redcap_uri,
    redcap_token = config$redcap_token,
    batch_size  = 200L
  )
  class(ikdds_config) <- c("ikdds_config", "list")

  # Read relevant forms from REDCap
  raw <- ikdds::ikdds_redcap_read(
    ikdds_config,
    forms = c("demographics", "diagnoses", "quarterly_bloods",
              "hd_prescription")
  )

  # Map REDCap fields to dashboard columns
  centres <- load_centres()

  df <- raw |>
    dplyr::filter(!is.na(.data$record_id)) |>
    dplyr::mutate(
      record_id   = as.character(.data$record_id),
      age         = calculate_age(.data$idn03),
      gender      = dplyr::case_when(
        .data$pat00 == "M" ~ "Male",
        .data$pat00 == "F" ~ "Female",
        TRUE ~ NA_character_
      ),
      centre_code = map_centre_code(.data$redcap_data_access_group),
      dxs01       = as.character(.data$dxs01),
      qblg9       = safe_numeric(.data$qblg9),
      hdp01       = safe_integer(.data$hdp01),
      hdp02       = safe_integer(.data$hdp02),
      qblg3       = safe_numeric(.data$qblg3),
      qblg4       = safe_numeric(.data$qblg4),
      qblg6       = safe_numeric(.data$qblg6),
      qblg7       = safe_numeric(.data$qblg7),
      qblb1       = safe_numeric(.data$qblb1),
      qblb4       = safe_numeric(.data$qblb4),
      qblb9       = safe_numeric(.data$qblb9),
      qbla9       = safe_numeric(.data$qbla9),
      qbla4       = safe_numeric(.data$qbla4),
      qble1       = safe_numeric(.data$qble1),
      qblf1       = safe_numeric(.data$qblf1),
      qhd20       = map_access_type(.data$qhd20)
    ) |>
    dplyr::left_join(
      centres |> dplyr::select("centre_code", "centre_name", "region",
                                "unit_code", "unit_name", "unit_type"),
      by = "centre_code"
    )

  # Add consultant field (from REDCap if available)
  if ("consultant" %in% names(raw)) {
    df$consultant <- raw$consultant
  } else {
    df$consultant <- NA_character_
  }

  df |>
    dplyr::select(dplyr::all_of(audit_data_columns()))
}

#' Calculate age from date of birth
#'
#' @param dob Character vector of dates in YYYY-MM-DD format.
#'
#' @return Numeric vector of ages in years.
#'
#' @keywords internal
calculate_age <- function(dob) {
  dob_date <- as.Date(dob, format = "%Y-%m-%d")
  as.numeric(difftime(Sys.Date(), dob_date, units = "days")) / 365.25
}

#' Safely convert to numeric
#'
#' @param x Vector to convert.
#'
#' @return Numeric vector.
#'
#' @keywords internal
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Safely convert to integer
#'
#' @param x Vector to convert.
#'
#' @return Integer vector.
#'
#' @keywords internal
safe_integer <- function(x) {
  suppressWarnings(as.integer(x))
}

#' Map data access group to centre code
#'
#' @param dag Character vector of REDCap data access group names.
#'
#' @return Character vector of centre codes.
#'
#' @keywords internal
map_centre_code <- function(dag) {
  # REDCap DAGs typically match centre codes
  toupper(substr(as.character(dag), 1, 3))
}

#' Map vascular access code to type
#'
#' @param code Character vector of access codes from REDCap.
#'
#' @return Character vector of access type labels.
#'
#' @keywords internal
map_access_type <- function(code) {
  dplyr::case_when(
    code %in% c("1", "AVF") ~ "AVF",
    code %in% c("2", "AVG") ~ "AVG",
    code %in% c("3", "Catheter", "CVC") ~ "Catheter",
    TRUE ~ NA_character_
  )
}

#' Get audit data from configured source
#'
#' Factory function that returns audit data from either the synthetic
#' generator or REDCap, depending on the `data_source` setting in config.
#' Both sources return identically structured tibbles.
#'
#' @param config A `dashboard_config` object from [dashboard_config()].
#'
#' @return A tibble with one row per patient and columns:
#'   `record_id`, `centre_code`, `centre_name`, `consultant`, `age`,
#'   `gender`, `dxs01`, `qblg9` (URR), `hdp01` (HD freq), `hdp02` (HD
#'   duration), `qblg3`/`qblg4` (pre BP), `qblg6`/`qblg7` (post BP),
#'   `qblb1` (PO4), `qblb4` (Ca), `qblb9` (PTH), `qbla9` (K),
#'   `qbla4` (HCO3), `qble1` (Hb), `qblf1` (ferritin), `qhd20` (access).
#'
#' @export
#' @family data
#'
#' @examples
#' config <- dashboard_config()
#' df <- get_audit_data(config)
#' nrow(df)
get_audit_data <- function(config) {
  switch(config$data_source,
    synthetic = {
      cli::cli_inform("Loading synthetic audit data...")
      generate_synthetic_data()
    },
    redcap = {
      cli::cli_inform("Loading data from REDCap...")
      get_redcap_audit_data(config)
    },
    cli::cli_abort("Unknown data source: {.val {config$data_source}}")
  )
}

#' Expected column names in audit data
#'
#' @return Character vector of required column names.
#'
#' @keywords internal
audit_data_columns <- function() {
  c("record_id", "centre_code", "centre_name", "region",
    "unit_code", "unit_name", "unit_type",
    "consultant", "is_acute", "age",
    "gender", "ethnicity", "dxs01", "qblg9", "hdp01", "hdp02",
    "qblg3", "qblg4", "qblg6", "qblg7", "qblb1", "qblb4", "qblb9",
    "qbla9", "qbla4", "qble1", "qblf1", "qhd20")
}

#' Validate audit data structure
#'
#' Checks that the data frame has all required columns with correct types.
#'
#' @param df A data frame to validate.
#'
#' @return `df` invisibly if valid; errors otherwise.
#'
#' @keywords internal
validate_audit_data <- function(df) {
  required <- audit_data_columns()
  missing <- setdiff(required, names(df))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Audit data is missing required columns:",
      "x" = "{.var {missing}}"
    ))
  }

  invisible(df)
}

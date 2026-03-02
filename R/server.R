#' Application Server
#'
#' Wires all Shiny modules together.
#'
#' @param config A `dashboard_config` object.
#'
#' @return A Shiny server function.
#'
#' @export
#' @family app
app_server <- function(config = dashboard_config()) {

  function(input, output, session) {

    # Load audit data once on app start
    audit_data <- shiny::reactive({
      shiny::withProgress(message = "Loading audit data...", {
        get_audit_data(config)
      })
    })

    # Filter module
    filters <- mod_filters_server("filters", audit_data)

    # Domain modules  - all receive filtered data
    mod_overview_server("overview", filters$filtered_data)
    mod_demographics_server("demographics", filters$filtered_data)
    mod_prd_server("prd", filters$filtered_data)
    mod_dialysis_server("dialysis", filters$filtered_data)
    mod_bp_server("bp", filters$filtered_data)
    mod_biochemistry_server("biochemistry", filters$filtered_data)
    mod_bicarbonate_server("bicarbonate", filters$filtered_data)
    mod_anaemia_server("anaemia", filters$filtered_data)
    mod_access_server("access", filters$filtered_data)
    mod_drilldown_server("drilldown", filters$filtered_data)
  }
}

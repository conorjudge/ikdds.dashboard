#' Application Server
#'
#' Wires all Shiny modules together. Applies role-based data filtering
#' before passing data to modules.
#'
#' @param config A `dashboard_config` object.
#'
#' @return A Shiny server function.
#'
#' @export
#' @family app
app_server <- function(config = dashboard_config()) {

  function(input, output, session) {

    # Determine user role and centre
    user_role <- get_user_role(config)
    user_centre <- get_user_centre(config)

    # Load audit data once on app start, applying role filter
    audit_data <- shiny::reactive({
      shiny::withProgress(message = "Loading audit data...", {
        raw <- get_audit_data(config)
        # Validate machine-populated fields (replace 0s with NA)
        if (exists("validate_machine_fields", mode = "function")) {
          raw <- validate_machine_fields(raw)
        }
        # Apply role-based filtering
        filter_by_role(raw, role = user_role, user_centre = user_centre)
      })
    })

    # Footer timestamp
    output$last_refreshed <- shiny::renderText({
      audit_data()
      paste("Last Refreshed:", format(Sys.time(), "%d %B %Y %H:%M"))
    })

    # Role indicator in header
    output$role_indicator <- shiny::renderUI({
      label <- role_display_label(user_role, user_centre)
      shiny::tags$span(class = "role-indicator", label)
    })

    # Filter module
    filters <- mod_filters_server("filters", audit_data)

    # Filter chips display in header
    output$filter_chips <- shiny::renderUI({
      af <- filters$active_filters()
      chips <- list()

      if (length(af$centre_code) > 0) {
        for (cc in af$centre_code) {
          chips <- c(chips, list(
            shiny::span(
              class = "filter-chip",
              paste0("Centre: ", cc),
              shiny::tags$span(
                class = "remove",
                shiny::HTML("&times;")
              )
            )
          ))
        }
      }

      if (length(af$consultant) > 0) {
        for (con in af$consultant) {
          chips <- c(chips, list(
            shiny::span(
              class = "filter-chip",
              paste0("Consultant: ", con),
              shiny::tags$span(
                class = "remove",
                shiny::HTML("&times;")
              )
            )
          ))
        }
      }

      if (isTRUE(af$exclude_acute)) {
        chips <- c(chips, list(
          shiny::span(
            class = "filter-chip",
            "Acute excluded",
            shiny::tags$span(class = "remove", shiny::HTML("&times;"))
          )
        ))
      }

      if (length(chips) == 0) return(NULL)
      shiny::tagList(chips)
    })

    # Filter state text for header subtitle
    output$filter_state <- shiny::renderText({
      af <- filters$active_filters()
      if (length(af$centre_code) == 0 && length(af$consultant) == 0) {
        "Displaying data for: National"
      } else if (length(af$centre_code) > 0) {
        paste("Displaying data for:", paste(af$centre_code, collapse = ", "))
      } else {
        paste("Filtered by consultant:", paste(af$consultant, collapse = ", "))
      }
    })

    # Domain modules — all receive filtered data and compare_by
    mod_overview_server("overview", filters$filtered_data)
    mod_demographics_server("demographics", filters$filtered_data)
    mod_prd_server("prd", filters$filtered_data)
    mod_dialysis_server("dialysis", filters$filtered_data,
                         compare_by = filters$compare_by)
    mod_bp_server("bp", filters$filtered_data,
                   compare_by = filters$compare_by)
    mod_biochemistry_server("biochemistry", filters$filtered_data,
                             compare_by = filters$compare_by)
    mod_bicarbonate_server("bicarbonate", filters$filtered_data,
                            compare_by = filters$compare_by)
    mod_anaemia_server("anaemia", filters$filtered_data,
                        compare_by = filters$compare_by)
    mod_access_server("access", filters$filtered_data)
    mod_drilldown_server("drilldown", filters$filtered_data)

    # New modules
    mod_data_quality_server("data_quality", filters$filtered_data)
    mod_centre_profile_server("centre_profile", filters$filtered_data,
                               audit_data, filters$active_filters)
    mod_individual_server("individual", filters$filtered_data,
                           audit_data, filters$active_filters)
    mod_methods_server("methods")
    mod_working_groups_server("working_groups")
  }
}

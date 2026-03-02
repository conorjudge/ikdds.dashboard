#' Filter sidebar module  - UI
#'
#' Centre and consultant filter controls displayed in the sidebar.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()] of filter inputs.
#'
#' @export
#' @family modules
mod_filters_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::selectInput(
      ns("centre"),
      label = "Centre",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::selectInput(
      ns("consultant"),
      label = "Consultant",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::actionButton(
      ns("reset"),
      "Reset Filters",
      class = "btn-outline-secondary btn-sm w-100 mt-2"
    ),
    shiny::hr(),
    shiny::tags$p(
      class = "text-muted small",
      "Select centres or consultants to filter. Leave blank for national view."
    )
  )
}

#' Filter sidebar module  - Server
#'
#' Manages filter state and returns filtered data as a reactive.
#'
#' @param id Module namespace ID.
#' @param audit_data Reactive tibble of audit data.
#'
#' @return A list with:
#'   - `filtered_data`: reactive tibble of filtered data
#'   - `active_filters`: reactive list of active filter values
#'
#' @export
#' @family modules
mod_filters_server <- function(id, audit_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Update centre choices when data loads
    shiny::observe({
      df <- audit_data()
      centres <- sort(unique(df$centre_code))
      centre_labels <- df %>%
        dplyr::distinct(.data$centre_code, .data$centre_name) %>%
        dplyr::arrange(.data$centre_code)
      choices <- stats::setNames(
        centre_labels$centre_code,
        paste0(centre_labels$centre_code, " - ", centre_labels$centre_name)
      )
      shiny::updateSelectInput(session, "centre", choices = choices)
    })

    # Update consultant choices based on selected centres
    shiny::observe({
      df <- audit_data()
      if (length(input$centre) > 0) {
        df <- df %>% dplyr::filter(.data$centre_code %in% input$centre)
      }
      consultants <- sort(unique(df$consultant))
      shiny::updateSelectInput(session, "consultant", choices = consultants)
    })

    # Reset button
    shiny::observeEvent(input$reset, {
      shiny::updateSelectInput(session, "centre", selected = character(0))
      shiny::updateSelectInput(session, "consultant", selected = character(0))
    })

    # Filtered data reactive
    filtered_data <- shiny::reactive({
      df <- audit_data()
      if (length(input$centre) > 0) {
        df <- df %>% dplyr::filter(.data$centre_code %in% input$centre)
      }
      if (length(input$consultant) > 0) {
        df <- df %>% dplyr::filter(.data$consultant %in% input$consultant)
      }
      df
    })

    active_filters <- shiny::reactive({
      list(
        centre_code = input$centre,
        consultant = input$consultant
      )
    })

    list(
      filtered_data = filtered_data,
      active_filters = active_filters
    )
  })
}

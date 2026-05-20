#' Filter sidebar module  - UI
#'
#' Region, unit, centre, and consultant filter controls displayed in the
#' sidebar. Includes compare-by toggle (Region/Centre/Unit/Consultant) and
#' acute patient exclusion checkbox.
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
    shiny::div(
      class = "compare-by-control",
      shiny::radioButtons(
        ns("compare_by"), "Compare by",
        choices = c("Region" = "region", "Centre" = "centre",
                    "Unit" = "unit", "Consultant" = "consultant"),
        selected = "centre", inline = TRUE
      )
    ),
    shiny::hr(),
    shiny::selectInput(
      ns("region"),
      label = "Region",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::selectInput(
      ns("unit"),
      label = "Unit",
      choices = NULL,
      multiple = TRUE,
      selectize = TRUE
    ),
    shiny::selectInput(
      ns("unit_type"),
      label = "Unit Type",
      choices = c("All" = "", "Renal" = "Renal",
                  "Satellite" = "Satellite", "Dialysis" = "Dialysis"),
      selected = "",
      multiple = FALSE
    ),
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
    shiny::hr(),
    shiny::checkboxInput(
      ns("exclude_acute"),
      "Exclude acute patients",
      value = FALSE
    ),
    shiny::actionButton(
      ns("reset"),
      "Reset Filters",
      class = "btn-outline-secondary btn-sm w-100 mt-2"
    ),
    shiny::hr(),
    shiny::tags$p(
      class = "text-muted small",
      "Select regions, units, centres, or consultants to filter. ",
      "Leave blank for national view."
    )
  )
}

#' Filter sidebar module  - Server
#'
#' Manages filter state and returns filtered data as a reactive.
#' Supports cascading Region -> Unit -> Centre -> Consultant filters.
#'
#' @param id Module namespace ID.
#' @param audit_data Reactive tibble of audit data.
#'
#' @return A list with:
#'   - `filtered_data`: reactive tibble of filtered data
#'   - `active_filters`: reactive list of active filter values
#'   - `compare_by`: reactive string ("region", "centre", "unit", or "consultant")
#'
#' @export
#' @family modules
mod_filters_server <- function(id, audit_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Update region choices when data loads
    shiny::observe({
      df <- audit_data()
      if ("region" %in% names(df)) {
        regions <- sort(unique(df$region[!is.na(df$region)]))
        shiny::updateSelectInput(session, "region", choices = regions)
      }
    })

    # Update unit choices — cascade from region selection
    shiny::observe({
      df <- audit_data()

      # Filter by region if selected
      if (length(input$region) > 0 && "region" %in% names(df)) {
        df <- df |> dplyr::filter(.data$region %in% input$region)
      }

      if ("unit_code" %in% names(df)) {
        units <- df |>
          dplyr::distinct(.data$unit_code, .data$unit_name) |>
          dplyr::arrange(.data$unit_code)
        choices <- stats::setNames(
          units$unit_code,
          paste0(units$unit_code, " - ", units$unit_name)
        )
        shiny::updateSelectInput(session, "unit", choices = choices)
      }
    })

    # Update centre choices — cascade from region and unit selection
    shiny::observe({
      df <- audit_data()

      # Filter by region if selected
      if (length(input$region) > 0 && "region" %in% names(df)) {
        df <- df |> dplyr::filter(.data$region %in% input$region)
      }

      # Filter by unit if selected
      if (length(input$unit) > 0 && "unit_code" %in% names(df)) {
        df <- df |> dplyr::filter(.data$unit_code %in% input$unit)
      }

      # Filter by unit_type if selected
      if (nzchar(input$unit_type %||% "") && "unit_type" %in% names(df)) {
        df <- df |> dplyr::filter(.data$unit_type == input$unit_type)
      }

      centre_labels <- df |>
        dplyr::distinct(.data$centre_code, .data$centre_name) |>
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
        df <- df |> dplyr::filter(.data$centre_code %in% input$centre)
      } else if (length(input$unit) > 0 && "unit_code" %in% names(df)) {
        df <- df |> dplyr::filter(.data$unit_code %in% input$unit)
      } else if (length(input$region) > 0 && "region" %in% names(df)) {
        df <- df |> dplyr::filter(.data$region %in% input$region)
      }
      consultants <- sort(unique(df$consultant))
      shiny::updateSelectInput(session, "consultant", choices = consultants)
    })

    # Reset button
    shiny::observeEvent(input$reset, {
      shiny::updateSelectInput(session, "region", selected = character(0))
      shiny::updateSelectInput(session, "unit", selected = character(0))
      shiny::updateSelectInput(session, "unit_type", selected = "")
      shiny::updateSelectInput(session, "centre", selected = character(0))
      shiny::updateSelectInput(session, "consultant", selected = character(0))
      shiny::updateCheckboxInput(session, "exclude_acute", value = FALSE)
    })

    # Filtered data reactive
    filtered_data <- shiny::reactive({
      df <- audit_data()

      # Region filter
      if (length(input$region) > 0 && "region" %in% names(df)) {
        df <- df |> dplyr::filter(.data$region %in% input$region)
      }

      # Unit filter
      if (length(input$unit) > 0 && "unit_code" %in% names(df)) {
        df <- df |> dplyr::filter(.data$unit_code %in% input$unit)
      }

      # Unit type filter
      if (nzchar(input$unit_type %||% "") && "unit_type" %in% names(df)) {
        df <- df |> dplyr::filter(.data$unit_type == input$unit_type)
      }

      # Centre filter
      if (length(input$centre) > 0) {
        df <- df |> dplyr::filter(.data$centre_code %in% input$centre)
      }

      # Consultant filter
      if (length(input$consultant) > 0) {
        df <- df |> dplyr::filter(.data$consultant %in% input$consultant)
      }

      # Acute patient exclusion
      if (isTRUE(input$exclude_acute) && "is_acute" %in% names(df)) {
        df <- df |> dplyr::filter(!.data$is_acute)
      }

      df
    })

    active_filters <- shiny::reactive({
      list(
        region = input$region,
        unit_code = input$unit,
        unit_type = input$unit_type,
        centre_code = input$centre,
        consultant = input$consultant,
        exclude_acute = input$exclude_acute
      )
    })

    compare_by <- shiny::reactive({
      input$compare_by %||% "centre"
    })

    list(
      filtered_data = filtered_data,
      active_filters = active_filters,
      compare_by = compare_by
    )
  })
}

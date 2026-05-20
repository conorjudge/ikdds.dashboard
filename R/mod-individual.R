#' Individual Patient QA module - UI
#'
#' Compare individual patient results against centre and national averages.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_individual_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "Select a centre and patient to compare individual results against ",
      "centre and national averages. This supports individual-level QA to ",
      "identify patients needing clinical attention."
    ),
    shiny::uiOutput(ns("individual_content"))
  )
}

#' Individual Patient QA module - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble of filtered data.
#' @param audit_data Reactive tibble of unfiltered audit data (for national reference).
#' @param active_filters Reactive list of active filter values.
#'
#' @export
#' @family modules
mod_individual_server <- function(id, filtered_data, audit_data,
                                   active_filters) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Metric definitions ---
    metric_defs <- list(
      list(label = "URR (%)",     col = "qblg9", lo = 65,   hi = Inf,  op = "gt"),
      list(label = "Pre SBP",     col = "qblg3", lo = -Inf, hi = 140,  op = "lt"),
      list(label = "Pre DBP",     col = "qblg4", lo = -Inf, hi = 90,   op = "lt"),
      list(label = "Post SBP",    col = "qblg6", lo = -Inf, hi = 130,  op = "lt"),
      list(label = "Post DBP",    col = "qblg7", lo = -Inf, hi = 80,   op = "lt"),
      list(label = "PO4 (mmol/L)",col = "qblb1", lo = 1.1,  hi = 1.7,  op = "range"),
      list(label = "Ca (mmol/L)", col = "qblb4", lo = 2.2,  hi = 2.5,  op = "range"),
      list(label = "PTH (pmol/L)",col = "qblb9", lo = 16,   hi = 72,   op = "range"),
      list(label = "K+ (mmol/L)", col = "qbla9", lo = 4,    hi = 6,    op = "range"),
      list(label = "HCO3 (mmol/L)", col = "qbla4", lo = 18, hi = 26,   op = "range"),
      list(label = "Hb (g/dL)",   col = "qble1", lo = 10,   hi = 12,   op = "range")
    )

    # --- Helper: format target string ---
    format_target <- function(m) {
      switch(m$op,
        gt    = paste0(">", m$lo),
        lt    = paste0("<", m$hi),
        range = paste0(m$lo, "-", m$hi)
      )
    }

    # --- Helper: check if value is in target ---
    in_target <- function(val, m) {
      if (is.na(val)) return(NA)
      switch(m$op,
        gt    = val > m$lo,
        lt    = val < m$hi,
        range = val >= m$lo & val <= m$hi
      )
    }

    # --- Centre selector choices ---
    centre_choices <- shiny::reactive({
      df <- filtered_data()
      sort(unique(df$centre_code[!is.na(df$centre_code)]))
    })

    # --- Patient choices for selected centre ---
    patient_choices <- shiny::reactive({
      shiny::req(input$centre_select)
      df <- filtered_data()
      centre_df <- df[df$centre_code == input$centre_select, , drop = FALSE]
      sort(unique(centre_df$record_id[!is.na(centre_df$record_id)]))
    })

    # --- Update patient selector when centre changes ---
    shiny::observeEvent(input$centre_select, {
      choices <- patient_choices()
      shiny::updateSelectInput(session, "patient_select",
                                choices = choices,
                                selected = if (length(choices) > 0) choices[1] else NULL)
    })

    # --- Main UI output ---
    output$individual_content <- shiny::renderUI({
      centres <- centre_choices()

      if (length(centres) == 0) {
        return(
          bslib::card(
            bslib::card_body(
              shiny::div(
                style = "text-align: center; padding: 3rem;",
                shiny::icon("user", style = "font-size: 3rem; color: #ccc;"),
                shiny::h4("No Data Available", style = "color: #666;"),
                shiny::p(class = "text-muted", "No centres found in the filtered data.")
              )
            )
          )
        )
      }

      shiny::tagList(
        # Selector row
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          shiny::selectInput(ns("centre_select"), "Centre",
                              choices = centres,
                              selected = centres[1]),
          shiny::selectInput(ns("patient_select"), "Patient (Record ID)",
                              choices = NULL),
          shiny::div(
            style = "padding-top: 1.7rem;",
            shiny::actionButton(ns("go_btn"), "Load Patient",
                                 class = "btn-primary",
                                 icon = shiny::icon("search"))
          )
        ),
        shiny::uiOutput(ns("patient_detail"))
      )
    })

    # --- Selected patient data (updates on button click) ---
    selected_patient <- shiny::eventReactive(input$go_btn, {
      shiny::req(input$centre_select, input$patient_select)
      df <- filtered_data()
      pt <- df[df$record_id == input$patient_select &
               df$centre_code == input$centre_select, , drop = FALSE]
      if (nrow(pt) == 0) return(NULL)
      pt[1, , drop = FALSE]
    })

    # --- Patient detail panel ---
    output$patient_detail <- shiny::renderUI({
      pt <- selected_patient()
      if (is.null(pt)) return(NULL)

      shiny::tagList(
        # KPI boxes
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          bslib::value_box(
            title = "Patient ID",
            value = shiny::textOutput(ns("kpi_patient_id")),
            showcase = shiny::icon("id-card"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Centre",
            value = shiny::textOutput(ns("kpi_centre")),
            showcase = shiny::icon("building"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Consultant",
            value = shiny::textOutput(ns("kpi_consultant")),
            showcase = shiny::icon("user-doctor"),
            theme = "primary"
          )
        ),

        # Comparison table
        bslib::card(
          bslib::card_header("Patient vs Centre vs National"),
          bslib::card_body(DT::DTOutput(ns("comparison_table")))
        ),

        # Bullet chart
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Patient Performance Bullet Chart"),
          bslib::card_body(
            plotly::plotlyOutput(ns("bullet_chart"), height = "500px")
          )
        ),

        source_footnote(
          "Individual patient data shown for clinical QA purposes only. Centre and national medians computed from the current audit dataset."
        )
      )
    })

    # --- KPI outputs ---

    output$kpi_patient_id <- shiny::renderText({
      pt <- selected_patient()
      if (is.null(pt)) return("--")
      as.character(pt$record_id[1])
    })

    output$kpi_centre <- shiny::renderText({
      pt <- selected_patient()
      if (is.null(pt)) return("--")
      as.character(pt$centre_code[1])
    })

    output$kpi_consultant <- shiny::renderText({
      pt <- selected_patient()
      if (is.null(pt)) return("--")
      con <- pt$consultant[1]
      if (is.na(con)) "Unmapped" else as.character(con)
    })

    # --- Comparison table ---

    output$comparison_table <- DT::renderDT({
      pt <- selected_patient()
      if (is.null(pt)) return(NULL)

      centre_code <- pt$centre_code[1]
      df_centre <- filtered_data()
      df_centre <- df_centre[df_centre$centre_code == centre_code, , drop = FALSE]
      df_national <- audit_data()

      rows <- lapply(metric_defs, function(m) {
        col <- m$col
        # Patient value
        pt_val <- if (col %in% names(pt)) pt[[col]][1] else NA_real_

        # Centre median
        centre_vals <- if (col %in% names(df_centre)) {
          df_centre[[col]][!is.na(df_centre[[col]])]
        } else numeric(0)
        centre_med <- if (length(centre_vals) > 0) {
          round(stats::median(centre_vals), 1)
        } else NA_real_

        # National median
        national_vals <- if (col %in% names(df_national)) {
          df_national[[col]][!is.na(df_national[[col]])]
        } else numeric(0)
        national_med <- if (length(national_vals) > 0) {
          round(stats::median(national_vals), 1)
        } else NA_real_

        # Status
        status <- if (is.na(pt_val)) {
          "Missing"
        } else if (in_target(pt_val, m)) {
          "In range"
        } else {
          "Out of range"
        }

        tibble::tibble(
          Metric           = m$label,
          Patient          = if (is.na(pt_val)) NA_real_ else round(pt_val, 1),
          `Centre Median`  = centre_med,
          `National Median` = national_med,
          Target           = format_target(m),
          Status           = status
        )
      })

      comparison_df <- dplyr::bind_rows(rows)

      dt <- hse_datatable(comparison_df,
                            caption = paste("Patient", pt$record_id[1],
                                            "- Metric Comparison"),
                            pageLength = 11)

      # Colour the Status column
      dt <- DT::formatStyle(
        dt, "Status",
        color = DT::styleEqual(
          c("In range", "Out of range", "Missing"),
          c(hse_colours()[["green"]], hse_colours()[["red"]], hse_colours()[["grey"]])
        ),
        fontWeight = DT::styleEqual(
          c("In range", "Out of range", "Missing"),
          c("normal", "bold", "normal")
        )
      )

      dt
    })

    # --- Bullet chart ---

    output$bullet_chart <- plotly::renderPlotly({
      pt <- selected_patient()
      if (is.null(pt)) return(NULL)

      centre_code <- pt$centre_code[1]
      df_centre <- filtered_data()
      df_centre <- df_centre[df_centre$centre_code == centre_code, , drop = FALSE]
      df_national <- audit_data()

      chart_data <- purrr::map_dfr(metric_defs, function(m) {
        col <- m$col
        pt_val <- if (col %in% names(pt)) pt[[col]][1] else NA_real_

        centre_vals <- if (col %in% names(df_centre)) {
          df_centre[[col]][!is.na(df_centre[[col]])]
        } else numeric(0)
        centre_med <- if (length(centre_vals) > 0) {
          stats::median(centre_vals)
        } else NA_real_

        national_vals <- if (col %in% names(df_national)) {
          df_national[[col]][!is.na(df_national[[col]])]
        } else numeric(0)
        national_med <- if (length(national_vals) > 0) {
          stats::median(national_vals)
        } else NA_real_

        tibble::tibble(
          metric   = m$label,
          patient  = pt_val,
          centre   = centre_med,
          national = national_med
        )
      })

      chart_data <- chart_data |>
        dplyr::filter(!is.na(.data$patient)) |>
        dplyr::mutate(metric = factor(.data$metric, levels = rev(.data$metric)))

      if (nrow(chart_data) == 0) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::labs(title = "No patient data available for chart")
        ))
      }

      p <- ggplot2::ggplot(chart_data) +
        # National median bar (background)
        ggplot2::geom_col(
          ggplot2::aes(x = .data$metric, y = .data$national),
          fill = "#E9ECEF", width = 0.6
        ) +
        # Patient value bar (foreground, narrow)
        ggplot2::geom_col(
          ggplot2::aes(x = .data$metric, y = .data$patient),
          fill = hse_colours()[["teal"]], width = 0.3
        ) +
        # Centre median marker
        ggplot2::geom_point(
          ggplot2::aes(x = .data$metric, y = .data$centre),
          shape = "|", size = 5, colour = hse_colours()[["orange"]]
        ) +
        # National median marker
        ggplot2::geom_point(
          ggplot2::aes(x = .data$metric, y = .data$national),
          shape = "|", size = 4, colour = hse_colours()[["red"]]
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste("Patient", pt$record_id[1], "vs Averages"),
          x = NULL,
          y = "Value"
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 10, r = 15, b = 15, l = 15),
          plot.title = ggplot2::element_text(size = 13, face = "bold")
        )

      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 50, t = 50, l = 120, r = 20))
    })
  })
}

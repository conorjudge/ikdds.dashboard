#' Data Quality module - UI
#'
#' Displays completeness heatmap, flagged centres, unmapped patients,
#' and summary KPIs with HSE colour coding.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_data_quality_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "Data quality overview showing completeness rates across centres and metrics. ",
      "Centres with any metric below 70% completeness are flagged. ",
      "Metrics from flagged centres may be suppressed in plots. ",
      "Colour coding: ",
      shiny::tags$span(style = "color:#28A745;font-weight:bold;", "green"),
      " (\u226590%), ",
      shiny::tags$span(style = "color:#F58220;font-weight:bold;", "amber"),
      " (70-89%), ",
      shiny::tags$span(style = "color:#DC3545;font-weight:bold;", "red"),
      " (<70%)."
    ),
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "Overall Completeness",
        value = shiny::textOutput(ns("overall_completeness")),
        showcase = shiny::icon("chart-pie"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Centres Below 70%",
        value = shiny::textOutput(ns("centres_below_threshold")),
        showcase = shiny::icon("triangle-exclamation"),
        theme = "warning"
      ),
      bslib::value_box(
        title = "Suppressed Cells",
        value = shiny::textOutput(ns("suppressed_cells")),
        showcase = shiny::icon("eye-slash"),
        theme = "secondary"
      ),
      bslib::value_box(
        title = "Unmapped Patients",
        value = shiny::textOutput(ns("unmapped_count")),
        showcase = shiny::icon("user-xmark"),
        theme = "info"
      )
    ),
    bslib::card(
      bslib::card_header("Completeness by Centre and Metric"),
      bslib::card_body(DT::DTOutput(ns("completeness_heatmap")))
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Completeness by Metric (National)"),
        bslib::card_body(
          plotly::plotlyOutput(ns("completeness_bar"), height = "420px")
        )
      ),
      bslib::card(
        bslib::card_header("Centres Flagged for Low Completeness"),
        bslib::card_body(DT::DTOutput(ns("flagged_centres")))
      )
    ),
    bslib::card(
      bslib::card_header("Unmapped Patients by Centre"),
      bslib::card_body(DT::DTOutput(ns("unmapped_table")))
    ),
    source_footnote(
      "Completeness threshold (70%) based on ERA-EDTA Best Practice recommendations."
    )
  )
}

#' Data Quality module - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble of filtered audit data.
#'
#' @export
#' @family modules
mod_data_quality_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Compute missing data per centre and variable
    missing_data <- shiny::reactive({
      compute_missing_data(filtered_data())
    })

    # Compute national missing data
    national_missing <- shiny::reactive({
      compute_missing_national(filtered_data())
    })

    # Pivot to centre x metric matrix for heatmap
    completeness_matrix <- shiny::reactive({
      md <- missing_data()
      md |>
        dplyr::mutate(pct_complete = round(100 - .data$pct_missing)) |>
        dplyr::select("centre_code", "centre_name", "variable",
                       "pct_complete") |>
        tidyr::pivot_wider(
          id_cols = c("centre_code", "centre_name"),
          names_from = "variable",
          values_from = "pct_complete"
        )
    })

    # Flag centres where any metric has >30% missing (< 70% complete)
    flagged <- shiny::reactive({
      md <- missing_data()
      flagged_centres <- md |>
        dplyr::filter(.data$pct_missing > 30) |>
        dplyr::mutate(pct_missing = round(.data$pct_missing)) |>
        dplyr::select("centre_code", "centre_name", "variable",
                       "pct_missing") |>
        dplyr::arrange(.data$centre_code, dplyr::desc(.data$pct_missing))
      flagged_centres
    })

    # Unmapped patients
    unmapped <- shiny::reactive({
      compute_unmapped_patients(filtered_data())
    })

    # --- KPI outputs ---

    output$overall_completeness <- shiny::renderText({
      nm <- national_missing()
      if (nrow(nm) == 0) return("N/A")
      avg <- round(100 - mean(nm$pct_missing, na.rm = TRUE))
      paste0(avg, "%")
    })

    output$centres_below_threshold <- shiny::renderText({
      md <- missing_data()
      n_flagged <- md |>
        dplyr::filter(.data$pct_missing > 30) |>
        dplyr::distinct(.data$centre_code) |>
        nrow()
      as.character(n_flagged)
    })

    output$suppressed_cells <- shiny::renderText({
      md <- missing_data()
      n_suppressed <- sum(md$n_total < 10, na.rm = TRUE)
      as.character(n_suppressed)
    })

    output$unmapped_count <- shiny::renderText({
      um <- unmapped()
      total <- sum(um$n_unmapped, na.rm = TRUE)
      as.character(total)
    })

    # --- Completeness heatmap table ---

    output$completeness_heatmap <- DT::renderDT({
      mat <- completeness_matrix()
      if (nrow(mat) == 0) {
        return(hse_datatable(tibble::tibble(Message = "No data available")))
      }

      metric_cols <- setdiff(names(mat), c("centre_code", "centre_name"))

      dt <- DT::datatable(
        mat,
        rownames = FALSE,
        class = "compact stripe hover",
        options = list(
          pageLength = 20,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", text = "Download CSV")
          )
        )
      )

      # Apply HSE green-amber-red background gradient
      for (col in metric_cols) {
        dt <- DT::formatStyle(
          dt, col,
          backgroundColor = DT::styleInterval(
            c(70, 90),
            c("#FFCCCC", "#FFFFCC", "#CCFFCC")
          )
        )
      }

      dt
    })

    # --- National completeness bar chart ---

    output$completeness_bar <- plotly::renderPlotly({
      nm <- national_missing()
      if (nrow(nm) == 0) return(NULL)

      nm <- nm |>
        dplyr::mutate(
          pct_complete = round(100 - .data$pct_missing),
          variable = stats::reorder(.data$variable, .data$pct_complete)
        )

      p <- ggplot2::ggplot(nm, ggplot2::aes(
        x = .data$variable,
        y = .data$pct_complete,
        fill = .data$pct_complete
      )) +
        ggplot2::geom_col() +
        ggplot2::geom_hline(yintercept = 70, linetype = "dashed",
                             colour = "#DC3545", linewidth = 0.7) +
        ggplot2::scale_fill_gradient2(
          low = "#DC3545", mid = "#FFC107", high = "#28A745",
          midpoint = 85, limits = c(0, 100),
          guide = "none"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = "Data Completeness by Metric",
          x = NULL,
          y = "Completeness (%)"
        ) +
        hse_ggplot_theme() +
        ggplot2::annotate("text", x = 0.5, y = 72, label = "70% threshold",
                           hjust = 0, colour = "#DC3545", size = 3)

      plotly::ggplotly(p, tooltip = c("x", "y")) |>
        plotly::layout(margin = list(b = 50, t = 50, l = 80, r = 20))
    })

    # --- Flagged centres table ---

    output$flagged_centres <- DT::renderDT({
      fl <- flagged()
      if (nrow(fl) == 0) {
        return(hse_datatable(
          tibble::tibble(Message = "No centres flagged - all above 70% completeness")
        ))
      }

      fl |>
        dplyr::rename(
          Centre = "centre_code",
          Name = "centre_name",
          Variable = "variable",
          `% Missing` = "pct_missing"
        ) |>
        hse_datatable(caption = "Centres with any metric >30% missing")
    })

    # --- Unmapped patients table ---

    output$unmapped_table <- DT::renderDT({
      um <- unmapped()
      if (nrow(um) == 0) {
        return(hse_datatable(
          tibble::tibble(Message = "No unmapped patients detected")
        ))
      }

      um |>
        dplyr::rename(
          Centre = "centre_code",
          Name = "centre_name",
          `Total Patients` = "n_total",
          `Unmapped` = "n_unmapped",
          `% Unmapped` = "pct_unmapped"
        ) |>
        hse_datatable(caption = "Patients without consultant mapping")
    })
  })
}

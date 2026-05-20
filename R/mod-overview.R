#' Overview module  - UI
#'
#' National summary landing page with traffic light KPI value boxes,
#' centre vs national comparison, and summary tables.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_overview_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "National summary of key haemodialysis audit metrics. ",
      "Use the filters on the right to select specific centres or consultants. ",
      "Traffic light colours: ",
      shiny::tags$span(style = "color:#28A745;font-weight:bold;", "green"),
      " (\u226590%), ",
      shiny::tags$span(style = "color:#F58220;font-weight:bold;", "amber"),
      " (70-89%), ",
      shiny::tags$span(style = "color:#DC3545;font-weight:bold;", "red"),
      " (<70%)."
    ),
    # Row 1: Summary counts
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "Total Patients",
        value = shiny::textOutput(ns("n_patients")),
        showcase = shiny::icon("users"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Centres",
        value = shiny::textOutput(ns("n_centres")),
        showcase = shiny::icon("hospital"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Consultants",
        value = shiny::textOutput(ns("n_consultants")),
        showcase = shiny::icon("user-doctor"),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Data Completeness",
        value = shiny::textOutput(ns("overall_completeness")),
        showcase = shiny::icon("chart-pie"),
        theme = "primary"
      )
    ),
    # Row 2: Traffic light clinical KPIs
    shiny::uiOutput(ns("traffic_light_kpis")),
    # Row 3: Centre vs National comparison table + missing data
    bslib::layout_columns(
      col_widths = c(7, 5),
      bslib::card(
        bslib::card_header("National Key Metrics Summary"),
        bslib::card_body(DT::DTOutput(ns("metrics_summary")))
      ),
      bslib::card(
        bslib::card_header("Patient Counts by Centre"),
        bslib::card_body(DT::DTOutput(ns("counts_table")))
      )
    ),
    bslib::card(
      bslib::card_header("Missing Data Overview"),
      bslib::card_body(DT::DTOutput(ns("missing_table")))
    ),
    source_footnote(
      "Standards: KDIGO 2024, KDOQI, ERA-EDTA Best Practice. BP target: pre-HD <140/90 mmHg."
    )
  )
}

#' Overview module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble of filtered audit data.
#'
#' @export
#' @family modules
mod_overview_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$n_patients <- shiny::renderText({
      format(nrow(filtered_data()), big.mark = ",")
    })

    output$n_centres <- shiny::renderText({
      length(unique(filtered_data()$centre_code))
    })

    output$n_consultants <- shiny::renderText({
      consultants <- filtered_data()$consultant
      length(unique(consultants[!is.na(consultants)]))
    })

    output$overall_completeness <- shiny::renderText({
      df <- filtered_data()
      vars <- c("qblg9", "qblg3", "qblg4", "qblg6", "qblg7",
                 "qblb1", "qblb4", "qblb9", "qbla9", "qbla4",
                 "qble1", "qblf1", "qhd20")
      vars <- intersect(vars, names(df))
      if (length(vars) == 0 || nrow(df) == 0) return("N/A")
      total_cells <- nrow(df) * length(vars)
      missing_cells <- sum(sapply(vars, function(v) sum(is.na(df[[v]]))))
      pct <- round((1 - missing_cells / total_cells) * 100)
      paste0(pct, "%")
    })

    # Compute KPI values reactively
    kpi_values <- shiny::reactive({
      df <- filtered_data()

      urr_pct <- {
        urr <- df$qblg9[!is.na(df$qblg9)]
        if (length(urr) > 0) round(sum(urr > 65) / length(urr) * 100) else NA_real_
      }

      bp_pct <- {
        bp <- df[!is.na(df$qblg3) & !is.na(df$qblg4), ]
        if (nrow(bp) > 0) round(sum(bp$qblg3 < 140 & bp$qblg4 < 90) / nrow(bp) * 100)
        else NA_real_
      }

      hb_pct <- {
        hb <- df$qble1[!is.na(df$qble1)]
        if (length(hb) > 0) round(sum(hb >= 10 & hb <= 12) / length(hb) * 100)
        else NA_real_
      }

      avf_pct <- {
        acc <- df$qhd20[!is.na(df$qhd20)]
        if (length(acc) > 0) round(sum(acc == "AVF") / length(acc) * 100)
        else NA_real_
      }

      po4_pct <- pct_in_range(df$qblb1, 1.1, 1.7)$pct
      po4_pct <- if (!is.na(po4_pct)) round(po4_pct) else NA_real_

      ferritin_pct <- pct_in_range(df$qblf1, 200, Inf)$pct
      ferritin_pct <- if (!is.na(ferritin_pct)) round(ferritin_pct) else NA_real_

      list(
        urr = urr_pct, bp = bp_pct, hb = hb_pct,
        avf = avf_pct, po4 = po4_pct, ferritin = ferritin_pct
      )
    })

    # Traffic light KPI row (rendered dynamically for colour theming)
    output$traffic_light_kpis <- shiny::renderUI({
      vals <- kpi_values()

      make_box <- function(title, val, icon_name) {
        theme <- traffic_light_theme(val)
        display <- if (is.na(val)) "N/A" else paste0(val, "%")
        bslib::value_box(
          title = title,
          value = display,
          showcase = shiny::icon(icon_name),
          theme = theme
        )
      }

      bslib::layout_columns(
        col_widths = c(2, 2, 2, 2, 2, 2),
        make_box("URR >65%", vals$urr, "droplet"),
        make_box("Pre-BP <140/90", vals$bp, "heart-pulse"),
        make_box("Hb 10-12", vals$hb, "vial"),
        make_box("AVF Rate", vals$avf, "syringe"),
        make_box("PO4 1.1-1.7", vals$po4, "flask"),
        make_box("Ferritin \u2265200", vals$ferritin, "vials")
      )
    })

    output$counts_table <- DT::renderDT({
      compute_patient_counts(filtered_data()) |>
        hse_datatable(caption = "Patient counts by centre")
    })

    output$metrics_summary <- DT::renderDT({
      df <- filtered_data()

      # Compute national percentages (whole numbers)
      bp_pct <- {
        bp <- df[!is.na(df$qblg3) & !is.na(df$qblg4), ]
        if (nrow(bp) > 0) round(sum(bp$qblg3 < 140 & bp$qblg4 < 90) / nrow(bp) * 100)
        else NA_real_
      }

      summary_df <- tibble::tibble(
        Metric = c("URR >65%", "Pre-HD BP <140/90", "PO4 1.1-1.7",
                    "Ca 2.2-2.5", "PTH 16-72", "K+ 4-6", "HCO3 18-26",
                    "Hb 10-12", "Ferritin >=200", "AVF Rate"),
        `National (%)` = c(
          round(pct_in_range(df$qblg9, 65, Inf)$pct),
          bp_pct,
          round(pct_in_range(df$qblb1, 1.1, 1.7)$pct),
          round(pct_in_range(df$qblb4, 2.2, 2.5)$pct),
          round(pct_in_range(df$qblb9, 16, 72)$pct),
          round(pct_in_range(df$qbla9, 4, 6)$pct),
          round(pct_in_range(df$qbla4, 18, 26)$pct),
          round(pct_in_range(df$qble1, 10, 12)$pct),
          round(pct_in_range(df$qblf1, 200, Inf)$pct),
          {
            acc <- df$qhd20[!is.na(df$qhd20)]
            if (length(acc) > 0) round(sum(acc == "AVF") / length(acc) * 100) else NA_real_
          }
        ),
        Target = c(">65%", "<140/90", "1.1-1.7 mmol/L", "2.2-2.5 mmol/L",
                    "16-72 pmol/L", "4-6 mmol/L", "18-26 mmol/L",
                    "10-12 g/dL", ">=200 ug/L", "Maximise"),
        Source = c("KDOQI", "KDIGO 2024", "KDIGO", "KDIGO", "KDIGO",
                   "Clinical", "KDIGO", "KDIGO/NICE", "Clinical", "ERA-EDTA")
      )

      dt <- hse_datatable(summary_df, caption = "National achievement rates")

      # Apply traffic light colour to National (%) column
      dt <- DT::formatStyle(
        dt, "National (%)",
        backgroundColor = DT::styleInterval(
          c(70, 90),
          c("#FFCCCC", "#FFFFCC", "#CCFFCC")
        )
      )

      dt
    })

    output$missing_table <- DT::renderDT({
      compute_missing_national(filtered_data()) |>
        dplyr::mutate(pct_missing = round(.data$pct_missing)) |>
        hse_datatable(caption = "Missing data rates")
    })
  })
}

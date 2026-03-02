#' Overview module  - UI
#'
#' National summary landing page with KPI value boxes and summary table.
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
        title = "URR >65%",
        value = shiny::textOutput(ns("urr_pct")),
        showcase = shiny::icon("droplet"),
        theme = "info"
      ),
      bslib::value_box(
        title = "Hb 10-12 g/dL",
        value = shiny::textOutput(ns("hb_pct")),
        showcase = shiny::icon("vial"),
        theme = "info"
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Patient Counts by Centre"),
        bslib::card_body(DT::DTOutput(ns("counts_table")))
      ),
      bslib::card(
        bslib::card_header("National Key Metrics Summary"),
        bslib::card_body(DT::DTOutput(ns("metrics_summary")))
      )
    ),
    bslib::card(
      bslib::card_header("Missing Data Overview"),
      bslib::card_body(DT::DTOutput(ns("missing_table")))
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

    output$n_patients <- shiny::renderText({
      format(nrow(filtered_data()), big.mark = ",")
    })

    output$n_centres <- shiny::renderText({
      length(unique(filtered_data()$centre_code))
    })

    output$urr_pct <- shiny::renderText({
      df <- filtered_data()
      urr <- df$qblg9[!is.na(df$qblg9)]
      if (length(urr) == 0) return("N/A")
      paste0(round(sum(urr > 65) / length(urr) * 100, 1), "%")
    })

    output$hb_pct <- shiny::renderText({
      df <- filtered_data()
      hb <- df$qble1[!is.na(df$qble1)]
      if (length(hb) == 0) return("N/A")
      paste0(round(sum(hb >= 10 & hb <= 12) / length(hb) * 100, 1), "%")
    })

    output$counts_table <- DT::renderDT({
      compute_patient_counts(filtered_data()) %>%
        hse_datatable(caption = "Patient counts by centre")
    })

    output$metrics_summary <- DT::renderDT({
      df <- filtered_data()
      tibble::tibble(
        Metric = c("URR >65%", "Pre-HD BP <140/90", "PO4 1.1-1.7",
                    "Ca 2.2-2.5", "PTH 16-72", "K+ 4-6", "HCO3 18-26",
                    "Hb 10-12", "Ferritin >=200"),
        `National %` = c(
          pct_in_range(df$qblg9, 65, Inf)$pct,
          {
            bp <- df[!is.na(df$qblg3) & !is.na(df$qblg4), ]
            if (nrow(bp) > 0) round(sum(bp$qblg3 < 140 & bp$qblg4 < 90) / nrow(bp) * 100, 1)
            else NA_real_
          },
          pct_in_range(df$qblb1, 1.1, 1.7)$pct,
          pct_in_range(df$qblb4, 2.2, 2.5)$pct,
          pct_in_range(df$qblb9, 16, 72)$pct,
          pct_in_range(df$qbla9, 4, 6)$pct,
          pct_in_range(df$qbla4, 18, 26)$pct,
          pct_in_range(df$qble1, 10, 12)$pct,
          pct_in_range(df$qblf1, 200, Inf)$pct
        )
      ) %>%
        dplyr::mutate(`National %` = round(.data$`National %`, 1)) %>%
        hse_datatable(caption = "National achievement rates")
    })

    output$missing_table <- DT::renderDT({
      compute_missing_national(filtered_data()) %>%
        hse_datatable(caption = "Missing data rates")
    })
  })
}

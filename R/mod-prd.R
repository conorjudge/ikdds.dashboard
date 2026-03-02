#' PRD module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_prd_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      bslib::card_header("Primary Renal Diagnosis  - ERA Classification"),
      bslib::card_body(
        plotly::plotlyOutput(ns("prd_plot"), height = "500px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("National PRD Summary"),
        bslib::card_body(DT::DTOutput(ns("national_table")))
      ),
      bslib::card(
        bslib::card_header("PRD by Centre"),
        bslib::card_body(DT::DTOutput(ns("centre_table")))
      )
    )
  )
}

#' PRD module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_prd_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$prd_plot <- plotly::renderPlotly({
      prd <- compute_prd_proportions(filtered_data())
      p <- plot_bar(prd, .data$centre_code, .data$pct, .data$prd_group,
                    title = "PRD Distribution by Centre",
                    position = "stack")
      plotly::ggplotly(p)
    })

    output$national_table <- DT::renderDT({
      compute_prd_national(filtered_data()) %>%
        hse_datatable(caption = "National PRD distribution")
    })

    output$centre_table <- DT::renderDT({
      compute_prd_proportions(filtered_data()) %>%
        tidyr::pivot_wider(
          id_cols = "prd_group",
          names_from = "centre_code",
          values_from = "pct",
          values_fill = 0
        ) %>%
        hse_datatable(caption = "PRD % by centre")
    })
  })
}

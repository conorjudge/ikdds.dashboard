#' Bicarbonate & Potassium module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_bicarbonate_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Potassium 4-6 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("k_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Potassium 4-6 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("k_funnel"), height = "350px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Bicarbonate 18-26 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hco3_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Bicarbonate 18-26 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hco3_funnel"), height = "350px")
        )
      )
    )
  )
}

#' Bicarbonate & Potassium module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_bicarbonate_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$k_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_potassium_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
    output$k_funnel <- plotly::renderPlotly({
      p <- plot_funnel(compute_potassium_achievement(filtered_data()))
      plotly::ggplotly(p)
    })

    output$hco3_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_bicarbonate_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
    output$hco3_funnel <- plotly::renderPlotly({
      p <- plot_funnel(compute_bicarbonate_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
  })
}

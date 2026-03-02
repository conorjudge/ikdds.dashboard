#' Blood Pressure module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_bp_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Pre-HD BP <140/90  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pre_bp_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Post-HD BP <130/80  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("post_bp_caterpillar"), height = "350px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Pre-HD BP <140/90  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pre_bp_funnel"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Post-HD BP <130/80  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("post_bp_funnel"), height = "350px")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Blood Pressure Summary Statistics"),
      bslib::card_body(DT::DTOutput(ns("bp_table")))
    )
  )
}

#' Blood Pressure module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_bp_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$pre_bp_caterpillar <- plotly::renderPlotly({
      metric <- compute_pre_bp_achievement(filtered_data())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p)
    })

    output$post_bp_caterpillar <- plotly::renderPlotly({
      metric <- compute_post_bp_achievement(filtered_data())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p)
    })

    output$pre_bp_funnel <- plotly::renderPlotly({
      metric <- compute_pre_bp_achievement(filtered_data())
      p <- plot_funnel(metric)
      plotly::ggplotly(p)
    })

    output$post_bp_funnel <- plotly::renderPlotly({
      metric <- compute_post_bp_achievement(filtered_data())
      p <- plot_funnel(metric)
      plotly::ggplotly(p)
    })

    output$bp_table <- DT::renderDT({
      compute_bp_summary(filtered_data()) %>%
        hse_datatable(caption = "Blood pressure statistics by centre")
    })
  })
}

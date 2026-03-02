#' Anaemia module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_anaemia_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Haemoglobin Median  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_median_plot"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Hb 10-12 g/dL  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_funnel"), height = "350px")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Haemoglobin Distribution by Centre"),
      bslib::card_body(
        plotly::plotlyOutput(ns("hb_dist_plot"), height = "400px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Ferritin >=200 ug/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ferritin_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Ferritin >=200 ug/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ferritin_funnel"), height = "350px")
        )
      )
    )
  )
}

#' Anaemia module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_anaemia_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$hb_median_plot <- plotly::renderPlotly({
      metric <- compute_hb_median(filtered_data())
      p <- plot_caterpillar_median(metric, x_label = "Hb (g/dL)")
      plotly::ggplotly(p)
    })

    output$hb_funnel <- plotly::renderPlotly({
      metric <- compute_hb_achievement(filtered_data())
      p <- plot_funnel(metric)
      plotly::ggplotly(p)
    })

    output$hb_dist_plot <- plotly::renderPlotly({
      p <- plot_distribution(
        filtered_data(), .data$qble1,
        title = "Haemoglobin Distribution",
        x_label = "Hb (g/dL)",
        target_lower = 10, target_upper = 12
      )
      plotly::ggplotly(p)
    })

    output$ferritin_caterpillar <- plotly::renderPlotly({
      metric <- compute_ferritin_achievement(filtered_data())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p)
    })

    output$ferritin_funnel <- plotly::renderPlotly({
      metric <- compute_ferritin_achievement(filtered_data())
      p <- plot_funnel(metric)
      plotly::ggplotly(p)
    })
  })
}

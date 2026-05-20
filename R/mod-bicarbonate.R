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
        full_screen = TRUE,
        bslib::card_header("Potassium 4-6 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("k_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Potassium 4-6 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("k_funnel"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Bicarbonate 18-26 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hco3_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Bicarbonate 18-26 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hco3_funnel"), height = "480px")
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
mod_bicarbonate_server <- function(id, filtered_data, compare_by = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    group_col <- shiny::reactive({
      if (is.function(compare_by)) {
        cb <- compare_by()
        if (!is.null(cb) && cb == "consultant") {
          "consultant"
        } else if (!is.null(cb) && cb == "region") {
          "region"
        } else {
          "centre_code"
        }
      } else {
        "centre_code"
      }
    })

    output$k_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "potassium_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$k_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "potassium_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    output$hco3_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "bicarbonate_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$hco3_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "bicarbonate_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })
  })
}

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
        full_screen = TRUE,
        bslib::card_header("Pre-HD BP <140/90  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pre_bp_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Post-HD BP <130/80  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("post_bp_caterpillar"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Pre-HD BP <140/90  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pre_bp_funnel"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Post-HD BP <130/80  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("post_bp_funnel"), height = "480px")
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
mod_bp_server <- function(id, filtered_data, compare_by = NULL) {
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

    output$pre_bp_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "pre_bp_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$post_bp_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "post_bp_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$pre_bp_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "pre_bp_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    output$post_bp_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "post_bp_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    output$bp_table <- DT::renderDT({
      compute_bp_summary(filtered_data()) |>
        hse_datatable(caption = "Blood pressure statistics by centre")
    })
  })
}

#' Biochemistry module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_biochemistry_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Phosphate 1.1-1.7 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("po4_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Phosphate 1.1-1.7 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("po4_funnel"), height = "350px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Adj. Calcium 2.2-2.5 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ca_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("Adj. Calcium 2.2-2.5 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ca_funnel"), height = "350px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("PTH 16-72 pmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pth_caterpillar"), height = "350px")
        )
      ),
      bslib::card(
        bslib::card_header("PTH 16-72 pmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pth_funnel"), height = "350px")
        )
      )
    ),
    bslib::card(
      bslib::card_header("Simultaneous CKD-MBD Control  - Caterpillar"),
      bslib::card_body(
        plotly::plotlyOutput(ns("mbd_caterpillar"), height = "350px")
      )
    )
  )
}

#' Biochemistry module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_biochemistry_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    # Phosphate
    output$po4_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_phosphate_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
    output$po4_funnel <- plotly::renderPlotly({
      p <- plot_funnel(compute_phosphate_achievement(filtered_data()))
      plotly::ggplotly(p)
    })

    # Calcium
    output$ca_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_calcium_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
    output$ca_funnel <- plotly::renderPlotly({
      p <- plot_funnel(compute_calcium_achievement(filtered_data()))
      plotly::ggplotly(p)
    })

    # PTH
    output$pth_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_pth_achievement(filtered_data()))
      plotly::ggplotly(p)
    })
    output$pth_funnel <- plotly::renderPlotly({
      p <- plot_funnel(compute_pth_achievement(filtered_data()))
      plotly::ggplotly(p)
    })

    # Simultaneous CKD-MBD
    output$mbd_caterpillar <- plotly::renderPlotly({
      p <- plot_caterpillar_proportion(compute_ckd_mbd_simultaneous(filtered_data()))
      plotly::ggplotly(p)
    })
  })
}

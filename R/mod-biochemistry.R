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
        full_screen = TRUE,
        bslib::card_header("Phosphate 1.1-1.7 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("po4_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Phosphate 1.1-1.7 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("po4_funnel"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Adj. Calcium 2.2-2.5 mmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ca_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Adj. Calcium 2.2-2.5 mmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ca_funnel"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("PTH 16-72 pmol/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pth_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("PTH 16-72 pmol/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("pth_funnel"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Ca x PO4 Product <4.4  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("capo4_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Ca x PO4 Product <4.4  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("capo4_funnel"), height = "480px")
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Simultaneous CKD-MBD Control  - Caterpillar"),
      bslib::card_body(
        plotly::plotlyOutput(ns("mbd_caterpillar"), height = "480px")
      )
    ),
    source_footnote(
      paste0("Targets: PO4 1.1-1.7 mmol/L, Ca 2.2-2.5 mmol/L, PTH 16-72 pmol/L, ",
             "Ca x PO4 <4.4 mmol\u00B2/L\u00B2. ",
             "Phosphate: pre-dialysis value (working group decision pending). ",
             "Standards: KDIGO 2024.")
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
mod_biochemistry_server <- function(id, filtered_data, compare_by = NULL) {
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

    # Phosphate
    output$po4_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "phosphate_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$po4_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "phosphate_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    # Calcium
    output$ca_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "calcium_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$ca_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "calcium_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    # PTH
    output$pth_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "pth_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$pth_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "pth_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    # Ca x PO4 Product
    output$capo4_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "ca_po4_product",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
    output$capo4_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "ca_po4_product",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    # Simultaneous CKD-MBD
    output$mbd_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "ckd_mbd_simultaneous",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })
  })
}

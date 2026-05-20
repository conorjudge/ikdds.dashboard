#' Dialysis Adequacy module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_dialysis_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("URR Median by Centre"),
        bslib::card_body(
          shiny::checkboxInput(ns("filter_3x_median"), "3x/week only", FALSE),
          plotly::plotlyOutput(ns("urr_median_plot"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("URR >65% Achievement by Centre"),
        bslib::card_body(
          shiny::checkboxInput(ns("filter_3x_achieve"), "3x/week only", FALSE),
          plotly::plotlyOutput(ns("urr_achieve_plot"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("URR >65%  - Funnel Plot"),
        bslib::card_body(
          plotly::plotlyOutput(ns("urr_funnel"), height = "480px")
        )
      ),
      bslib::card(
        bslib::card_header("Session Frequency"),
        bslib::card_body(DT::DTOutput(ns("freq_table")))
      )
    ),
    bslib::card(
      bslib::card_header("Session Duration Distribution"),
      bslib::card_body(DT::DTOutput(ns("duration_table")))
    ),
    source_footnote("Target: URR >65% (KDOQI). Session frequency target: 3x/week. Duration as prescribed.")
  )
}

#' Dialysis Adequacy module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_dialysis_server <- function(id, filtered_data, compare_by = NULL) {
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

    output$urr_median_plot <- plotly::renderPlotly({
      if (input$filter_3x_median) {
        # 3x/week filter: compute directly, then apply suppression manually
        metric <- compute_urr_median(filtered_data(), filter_3x = TRUE,
                                      group_col = group_col())
        metric <- apply_suppression(metric)
      } else {
        metric <- compute_with_engine(filtered_data(), "urr_median",
                                       group_col = group_col())
      }
      p <- plot_caterpillar_median(metric, x_label = "URR (%)")
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$urr_achieve_plot <- plotly::renderPlotly({
      if (input$filter_3x_achieve) {
        # 3x/week filter: compute directly, then apply suppression manually
        metric <- compute_urr_achievement(filtered_data(), filter_3x = TRUE,
                                           group_col = group_col())
        metric <- apply_suppression(metric)
      } else {
        metric <- compute_with_engine(filtered_data(), "urr_achievement",
                                       group_col = group_col())
      }
      p <- plot_caterpillar_proportion(metric, target = 0.65)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$urr_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "urr_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    output$freq_table <- DT::renderDT({
      compute_session_frequency(filtered_data()) |>
        tidyr::pivot_wider(
          id_cols = c("centre_code", "centre_name"),
          names_from = "hd_freq",
          values_from = "pct",
          values_fill = 0
        ) |>
        hse_datatable(caption = "Session frequency (% of patients)")
    })

    output$duration_table <- DT::renderDT({
      compute_session_duration(filtered_data()) |>
        tidyr::pivot_wider(
          id_cols = c("centre_code", "centre_name"),
          names_from = "duration_band",
          values_from = "pct",
          values_fill = 0
        ) |>
        hse_datatable(caption = "Session duration (% of patients)")
    })
  })
}

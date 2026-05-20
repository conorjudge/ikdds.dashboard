#' Vascular Access module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_access_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Vascular Access Distribution by Centre"),
      bslib::card_body(
        plotly::plotlyOutput(ns("access_plot"), height = "520px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("AVF Rate  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("avf_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        bslib::card_header("Access Type  - Detail Table"),
        bslib::card_body(DT::DTOutput(ns("access_table")))
      )
    )
  )
}

#' Vascular Access module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_access_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$access_plot <- plotly::renderPlotly({
      access <- compute_access_distribution(filtered_data())
      p <- plot_bar(access, .data$centre_code, .data$pct, .data$access_type,
                    title = "Vascular Access Type",
                    position = "stack")
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$avf_caterpillar <- plotly::renderPlotly({
      metric <- compute_avf_rate(filtered_data())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$access_table <- DT::renderDT({
      compute_access_distribution(filtered_data()) |>
        tidyr::pivot_wider(
          id_cols = c("centre_code", "centre_name"),
          names_from = "access_type",
          values_from = c("count", "pct"),
          values_fill = 0
        ) |>
        hse_datatable(caption = "Vascular access by centre")
    })
  })
}

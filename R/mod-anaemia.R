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
        full_screen = TRUE,
        bslib::card_header("Haemoglobin Median  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_median_plot"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Hb 10-12 g/dL  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_funnel"), height = "480px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Haemoglobin Distribution by Centre"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_dist_plot"), height = "520px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Hb Cumulative Frequency (ECDF)"),
        bslib::card_body(
          plotly::plotlyOutput(ns("hb_ecdf_plot"), height = "520px")
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Hb Band Distribution - Stacked Bar"),
      bslib::card_body(
        plotly::plotlyOutput(ns("hb_stacked_bar"), height = "480px")
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Ferritin >=200 ug/L  - Caterpillar"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ferritin_caterpillar"), height = "480px")
        )
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Ferritin >=200 ug/L  - Funnel"),
        bslib::card_body(
          plotly::plotlyOutput(ns("ferritin_funnel"), height = "480px")
        )
      )
    ),
    # Temporal trends section
    bslib::card(
      bslib::card_header("Temporal Trends"),
      bslib::card_body(
        shiny::uiOutput(ns("trends_content"))
      )
    ),
    source_footnote("Targets: Hb 10-12 g/dL (KDIGO/NICE), Ferritin >=200 ug/L. Standards: KDIGO 2024.")
  )
}

#' Anaemia module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_anaemia_server <- function(id, filtered_data, compare_by = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    output$hb_median_plot <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "hb_median",
                                     group_col = group_col())
      p <- plot_caterpillar_median(metric, x_label = "Hb (g/dL)")
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$hb_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "hb_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    output$hb_dist_plot <- plotly::renderPlotly({
      p <- plot_distribution(
        filtered_data(), .data$qble1,
        title = "Haemoglobin Distribution",
        x_label = "Hb (g/dL)",
        target_lower = 10, target_upper = 12
      )
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 60, t = 50, l = 60, r = 20))
    })

    # Cumulative frequency (ECDF) plot
    output$hb_ecdf_plot <- plotly::renderPlotly({
      df <- filtered_data()
      hb_df <- df[!is.na(df$qble1), c("centre_code", "qble1")]

      if (nrow(hb_df) == 0) {
        return(plotly::ggplotly(
          ggplot2::ggplot() + ggplot2::labs(title = "No Hb data")
        ))
      }

      p <- ggplot2::ggplot(hb_df, ggplot2::aes(
        x = .data$qble1,
        colour = .data$centre_code
      )) +
        ggplot2::stat_ecdf(linewidth = 0.8) +
        ggplot2::geom_vline(xintercept = c(10, 12), linetype = "dashed",
                             colour = "#DC3545", linewidth = 0.5) +
        ggplot2::labs(
          title = "Hb Cumulative Frequency (S-Curve)",
          x = "Hb (g/dL)",
          y = "Cumulative Proportion",
          colour = "Centre"
        ) +
        hse_ggplot_theme() +
        scale_colour_hse()

      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 60, t = 50, l = 60, r = 20))
    })

    # Stacked bar of Hb bands
    output$hb_stacked_bar <- plotly::renderPlotly({
      hb_bands <- compute_hb_distribution(filtered_data())
      if (nrow(hb_bands) == 0) {
        return(plotly::ggplotly(
          ggplot2::ggplot() + ggplot2::labs(title = "No Hb data")
        ))
      }
      p <- plot_bar(hb_bands, .data$centre_code, .data$pct, .data$hb_band,
                    title = "Hb Band Distribution by Centre",
                    y_label = "Percentage (%)", position = "stack")
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$ferritin_caterpillar <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "ferritin_achievement",
                                     group_col = group_col())
      p <- plot_caterpillar_proportion(metric)
      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 100, t = 50, l = 60, r = 20))
    })

    output$ferritin_funnel <- plotly::renderPlotly({
      metric <- compute_with_engine(filtered_data(), "ferritin_achievement",
                                     group_col = group_col())
      p <- plot_funnel(metric)
      plotly::ggplotly(p, tooltip = "text") |>
        plotly::layout(margin = list(b = 60, t = 70, l = 60, r = 20))
    })

    # Temporal trends — show placeholder until multi-period data available
    output$trends_content <- shiny::renderUI({
      df <- filtered_data()
      if ("audit_period" %in% names(df) && length(unique(df$audit_period)) > 1) {
        plotly::plotlyOutput(ns("trends_plot"), height = "400px")
      } else {
        trend_placeholder()
      }
    })
  })
}

#' Temporal trend line chart
#'
#' Plots metric achievement over time at national, unit, or centre level.
#' Requires an `audit_period` column in the data.
#'
#' @param trend_data Tibble with columns: `period`, `group`, `value`.
#' @param title Chart title.
#' @param y_label Y-axis label.
#' @param national_ref Optional national reference line value.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_trend_line <- function(trend_data, title = "Metric Trend",
                             y_label = "Achievement (%)",
                             national_ref = NULL) {
  if (nrow(trend_data) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = title, subtitle = "No temporal data available") +
        hse_ggplot_theme()
    )
  }

  p <- ggplot2::ggplot(trend_data, ggplot2::aes(
    x = .data$period,
    y = .data$value,
    colour = .data$group,
    group = .data$group
  )) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2.5)

  if (!is.null(national_ref)) {
    p <- p +
      ggplot2::geom_hline(
        yintercept = national_ref,
        linetype = "dashed",
        colour = "#DC3545",
        linewidth = 0.7
      ) +
      ggplot2::annotate("text", x = 1, y = national_ref + 1,
                         label = "National", colour = "#DC3545",
                         hjust = 0, size = 3)
  }

  p +
    ggplot2::labs(title = title, x = "Audit Period", y = y_label,
                   colour = NULL) +
    hse_ggplot_theme() +
    scale_colour_hse()
}

#' Trend placeholder UI
#'
#' Shows a placeholder message when temporal data is not yet available.
#'
#' @param message Message to display.
#'
#' @return A [shiny::div()].
#'
#' @export
#' @family plots
trend_placeholder <- function(message = "Trends available when multi-period data is loaded") {
  shiny::div(
    class = "trend-placeholder",
    shiny::icon("clock", style = "font-size: 2rem; color: #ccc;"),
    shiny::tags$br(),
    shiny::tags$p(message)
  )
}

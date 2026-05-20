#' Radar/Spider chart for PRD distribution
#'
#' Creates a plotly polar chart showing PRD category proportions by centre.
#'
#' @param prd_data Tibble from [compute_prd_proportions()] with columns
#'   `centre_code`, `prd_group`, `pct`.
#'
#' @return A plotly object.
#'
#' @export
#' @family plots
plot_radar_prd <- function(prd_data) {
  if (nrow(prd_data) == 0) {
    return(plotly::plot_ly() |>
      plotly::layout(title = "No PRD data available"))
  }

  centres <- unique(prd_data$centre_code)
  colours <- hse_colours()
  colour_vec <- unname(colours[seq_len(min(length(centres), length(colours)))])

  fig <- plotly::plot_ly(type = "scatterpolar", fill = "toself")

  for (i in seq_along(centres)) {
    cc <- centres[i]
    cc_data <- prd_data[prd_data$centre_code == cc, ]

    fig <- fig |>
      plotly::add_trace(
        r = cc_data$pct,
        theta = cc_data$prd_group,
        name = cc,
        line = list(color = colour_vec[i]),
        fillcolor = paste0(colour_vec[i], "33")
      )
  }

  fig |>
    plotly::layout(
      title = list(text = "PRD Distribution by Centre (Radar)"),
      polar = list(
        radialaxis = list(visible = TRUE, range = c(0, max(prd_data$pct, na.rm = TRUE) * 1.1))
      ),
      showlegend = TRUE,
      legend = list(orientation = "h", y = -0.15)
    )
}

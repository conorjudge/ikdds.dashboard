#' Grouped bar chart
#'
#' Creates a grouped or stacked bar chart for categorical metrics by centre.
#'
#' @param df Tibble with `centre_code`, `category`, `count`/`pct` columns.
#' @param x_col Unquoted column for x-axis (typically `centre_code`).
#' @param y_col Unquoted column for y-axis (typically `pct` or `count`).
#' @param fill_col Unquoted column for fill grouping.
#' @param title Plot title.
#' @param y_label Y-axis label.
#' @param position `"dodge"` for grouped, `"stack"` for stacked.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_bar <- function(df, x_col, y_col, fill_col,
                      title = NULL, y_label = "Percentage (%)",
                      position = "dodge") {
  x_col <- rlang::enquo(x_col)
  y_col <- rlang::enquo(y_col)
  fill_col <- rlang::enquo(fill_col)

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = !!x_col, y = !!y_col, fill = !!fill_col)
  ) +
    ggplot2::geom_col(position = position, width = 0.7) +
    scale_fill_hse() +
    ggplot2::labs(title = title, x = NULL, y = y_label, fill = NULL) +
    hse_ggplot_theme() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 15, b = 60, l = 15),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )

  p
}

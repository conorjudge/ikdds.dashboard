#' Histogram/density plot by centre
#'
#' Creates a faceted histogram or overlaid density plot for a continuous
#' variable, optionally highlighting a target range.
#'
#' @param df Audit data tibble.
#' @param value_col Unquoted column name.
#' @param title Plot title.
#' @param x_label X-axis label.
#' @param target_lower Optional lower target bound.
#' @param target_upper Optional upper target bound.
#' @param facet If `TRUE`, facet by centre (default TRUE).
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_distribution <- function(df, value_col, title = NULL,
                               x_label = "Value",
                               target_lower = NULL,
                               target_upper = NULL,
                               facet = TRUE) {
  value_col <- rlang::enquo(value_col)
  colours <- hse_colours()

  p <- ggplot2::ggplot(
    df %>% dplyr::filter(!is.na(!!value_col)),
    ggplot2::aes(x = !!value_col)
  ) +
    ggplot2::geom_histogram(
      fill = colours[["teal"]], colour = "white",
      bins = 20, alpha = 0.8
    )

  if (!is.null(target_lower)) {
    p <- p + ggplot2::geom_vline(
      xintercept = target_lower,
      linetype = "dashed", colour = colours[["green"]]
    )
  }

  if (!is.null(target_upper)) {
    p <- p + ggplot2::geom_vline(
      xintercept = target_upper,
      linetype = "dashed", colour = colours[["green"]]
    )
  }

  p <- p +
    ggplot2::labs(title = title, x = x_label, y = "Count") +
    hse_ggplot_theme()

  if (facet) {
    p <- p + ggplot2::facet_wrap(~ centre_code, scales = "free_y")
  }

  p
}

#' Caterpillar plot for proportions
#'
#' Creates an ordered dot-and-whisker plot showing centre proportions
#' with Wilson score confidence intervals and a national average reference line.
#'
#' @param metric_df Tibble from [compute_centre_proportion()].
#' @param title Plot title.
#' @param x_label X-axis label.
#' @param target Optional target proportion (0-1) to show as reference line.
#' @param pct_scale If `TRUE`, display x-axis as percentages (default TRUE).
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_caterpillar_proportion <- function(metric_df, title = NULL,
                                         x_label = "Proportion (%)",
                                         target = NULL,
                                         pct_scale = TRUE) {
  colours <- hse_colours()
  nat_avg <- national_average_proportion(metric_df)

  plot_df <- metric_df %>%
    dplyr::mutate(
      centre_ordered = order_centres(metric_df, "proportion")
    )

  multiplier <- if (pct_scale) 100 else 1

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$proportion * multiplier,
      y = .data$centre_ordered
    )
  ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(
        xmin = .data$lower_ci * multiplier,
        xmax = .data$upper_ci * multiplier
      ),
      height = 0.3, colour = colours[["teal"]], linewidth = 0.8
    ) +
    ggplot2::geom_point(
      size = 3, colour = colours[["teal"]]
    )

  if (!is.null(nat_avg) && !is.na(nat_avg)) {
    p <- p + ggplot2::geom_vline(
      xintercept = nat_avg * multiplier,
      linetype = "dashed", colour = colours[["red"]], linewidth = 0.6
    )
  }

  if (!is.null(target)) {
    p <- p + ggplot2::geom_vline(
      xintercept = target * multiplier,
      linetype = "dotted", colour = colours[["green"]], linewidth = 0.8
    )
  }

  p <- p +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      x = x_label,
      y = NULL
    ) +
    hse_ggplot_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#E9ECEF")
    )

  if (pct_scale) {
    p <- p + ggplot2::scale_x_continuous(
      labels = function(x) paste0(round(x), "%"),
      limits = c(0, 100)
    )
  }

  p
}

#' Caterpillar plot for medians
#'
#' Creates an ordered dot-and-whisker plot showing centre medians
#' with confidence intervals.
#'
#' @param metric_df Tibble from [compute_centre_median()].
#' @param title Plot title.
#' @param x_label X-axis label.
#' @param target Optional target value to show as reference line.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_caterpillar_median <- function(metric_df, title = NULL,
                                     x_label = "Value",
                                     target = NULL) {
  colours <- hse_colours()
  nat_avg <- national_average_median(metric_df)

  plot_df <- metric_df %>%
    dplyr::mutate(
      centre_ordered = order_centres(metric_df, "median")
    )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$median, y = .data$centre_ordered)
  ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lower_ci, xmax = .data$upper_ci),
      height = 0.3, colour = colours[["teal"]], linewidth = 0.8
    ) +
    ggplot2::geom_point(size = 3, colour = colours[["teal"]])

  if (!is.null(nat_avg) && !is.na(nat_avg)) {
    p <- p + ggplot2::geom_vline(
      xintercept = nat_avg,
      linetype = "dashed", colour = colours[["red"]], linewidth = 0.6
    )
  }

  if (!is.null(target)) {
    p <- p + ggplot2::geom_vline(
      xintercept = target,
      linetype = "dotted", colour = colours[["green"]], linewidth = 0.8
    )
  }

  p +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      x = x_label,
      y = NULL
    ) +
    hse_ggplot_theme() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#E9ECEF")
    )
}

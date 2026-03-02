#' Funnel plot for proportions
#'
#' Creates a Spiegelhalter-style funnel plot with 95% and 99.7% control
#' limits, used by the UK Renal Registry for centre comparison.
#'
#' @param metric_df Tibble from [compute_centre_proportion()].
#' @param title Plot title.
#' @param y_label Y-axis label.
#' @param pct_scale If `TRUE`, display y-axis as percentages (default TRUE).
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_funnel <- function(metric_df, title = NULL, y_label = "Proportion (%)",
                         pct_scale = TRUE) {
  colours <- hse_colours()
  nat_avg <- national_average_proportion(metric_df)

  if (is.na(nat_avg)) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Insufficient data"))
  }

  n_range <- seq(
    max(1, min(metric_df$n) - 10),
    max(metric_df$n) + 20
  )
  limits <- funnel_limits(nat_avg, n_range)

  multiplier <- if (pct_scale) 100 else 1

  p <- ggplot2::ggplot() +
    # 99.7% limits (outer)
    ggplot2::geom_ribbon(
      data = limits,
      ggplot2::aes(
        x = .data$n,
        ymin = .data$lower_997 * multiplier,
        ymax = .data$upper_997 * multiplier
      ),
      fill = colours[["light_grey"]], alpha = 0.5
    ) +
    # 95% limits (inner)
    ggplot2::geom_ribbon(
      data = limits,
      ggplot2::aes(
        x = .data$n,
        ymin = .data$lower_95 * multiplier,
        ymax = .data$upper_95 * multiplier
      ),
      fill = colours[["light_teal"]], alpha = 0.3
    ) +
    # National average line
    ggplot2::geom_hline(
      yintercept = nat_avg * multiplier,
      linetype = "dashed", colour = colours[["red"]], linewidth = 0.6
    ) +
    # Centre points
    ggplot2::geom_point(
      data = metric_df,
      ggplot2::aes(
        x = .data$n,
        y = .data$proportion * multiplier
      ),
      size = 3, colour = colours[["teal"]]
    ) +
    # Centre labels
    ggplot2::geom_text(
      data = metric_df,
      ggplot2::aes(
        x = .data$n,
        y = .data$proportion * multiplier,
        label = .data$centre_code
      ),
      vjust = -1, size = 3, colour = colours[["dark_grey"]]
    ) +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      x = "Number of Patients",
      y = y_label
    ) +
    hse_ggplot_theme()

  if (pct_scale) {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x), "%")
    )
  }

  p
}

#' Funnel plot for proportions
#'
#' Creates a Spiegelhalter-style funnel plot with 95% and 99.7% control
#' limits, used by the UK Renal Registry for centre comparison.
#' Style matches IKDDS audit report: open circle points, dark teal/purple
#' funnel lines, orange solid national mean, legend at top.
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
  # Handle suppression: use all data for national average, exclude suppressed from plot
  has_suppressed <- "suppressed" %in% names(metric_df)
  has_labels <- "label" %in% names(metric_df)
  n_suppressed <- 0L

  nat_avg <- national_average_proportion(metric_df)

  if (has_suppressed) {
    n_suppressed <- sum(metric_df$suppressed, na.rm = TRUE)
    plot_metric_df <- metric_df |> dplyr::filter(!.data$suppressed)
  } else {
    plot_metric_df <- metric_df
  }

  if (is.na(nat_avg)) {
    return(ggplot2::ggplot() + ggplot2::labs(title = "Insufficient data"))
  }

  n_range <- seq(
    max(1, min(metric_df$n) - 10),
    max(metric_df$n) + 20
  )
  limits <- funnel_limits(nat_avg, n_range)

  multiplier <- if (pct_scale) 100 else 1

  # Colours matching draft: dark purple-teal for funnel lines, orange for mean
  funnel_col <- "#2E2D88"   # dark indigo/purple for funnel lines
  mean_col   <- "#E68A00"   # orange for national mean

  p <- ggplot2::ggplot() +
    # 95% limits (solid lines)
    ggplot2::geom_line(
      data = limits,
      ggplot2::aes(x = .data$n, y = .data$lower_95 * multiplier,
                   linetype = "95% limits"),
      colour = funnel_col, linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = limits,
      ggplot2::aes(x = .data$n, y = .data$upper_95 * multiplier,
                   linetype = "95% limits"),
      colour = funnel_col, linewidth = 0.7
    ) +
    # 99.7% limits (dashed lines)
    ggplot2::geom_line(
      data = limits,
      ggplot2::aes(x = .data$n, y = .data$lower_997 * multiplier,
                   linetype = "99.7% limits"),
      colour = funnel_col, linewidth = 0.7
    ) +
    ggplot2::geom_line(
      data = limits,
      ggplot2::aes(x = .data$n, y = .data$upper_997 * multiplier,
                   linetype = "99.7% limits"),
      colour = funnel_col, linewidth = 0.7
    ) +
    # National average line
    ggplot2::geom_hline(
      ggplot2::aes(yintercept = nat_avg * multiplier,
                   linetype = "National mean"),
      colour = mean_col, linewidth = 0.8
    ) +
    # Linetype scale
    ggplot2::scale_linetype_manual(
      name = NULL,
      values = c("National mean" = "solid",
                 "95% limits" = "solid",
                 "99.7% limits" = "dashed"),
      guide = ggplot2::guide_legend(
        override.aes = list(
          colour = c(mean_col, funnel_col, funnel_col)
        )
      )
    ) +
    # Centre points (open circles) — only non-suppressed
    ggplot2::geom_point(
      data = plot_metric_df,
      ggplot2::aes(
        x = .data$n,
        y = .data$proportion * multiplier,
        text = paste0(.data$centre_code, ": ",
                      round(.data$proportion * 100, 1), "%\nn=", .data$n)
      ),
      shape = 1, size = 2.5, colour = hse_colours()[["teal"]], stroke = 0.8
    ) +
    # Centre labels (repelled to avoid overlap) — use label col if available
    ggrepel::geom_text_repel(
      data = {
        tmp <- plot_metric_df
        tmp$plot_label <- if (has_labels) tmp$label else tmp$centre_code
        tmp
      },
      ggplot2::aes(
        x = .data$n,
        y = .data$proportion * multiplier,
        label = .data$plot_label
      ),
      size = 3, colour = hse_colours()[["teal"]],
      max.overlaps = Inf,
      box.padding = 0.35,
      segment.colour = "grey70",
      segment.size = 0.3
    ) +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      subtitle = if (n_suppressed > 0) {
        paste0("Centres suppressed (n<10): ", n_suppressed)
      } else {
        NULL
      },
      x = "Number of patients with data in centre",
      y = y_label
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 15, b = 15, l = 15),
      legend.position = "top",
      legend.margin = ggplot2::margin(t = 0, b = 5),
      legend.box.margin = ggplot2::margin(b = 5),
      plot.title = ggplot2::element_text(size = 13, face = "bold")
    )

  if (pct_scale) {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x))
    )
  }

  p
}

#' Caterpillar plot for proportions
#'
#' Creates a vertical ordered dot-and-whisker plot showing centre proportions
#' with Wilson score confidence intervals and a national average reference line.
#' Style matches IKDDS audit report conventions: black points and whiskers,
#' red solid national mean line, centres on x-axis sorted descending.
#'
#' @param metric_df Tibble from [compute_centre_proportion()].
#' @param title Plot title.
#' @param y_label Y-axis label.
#' @param target Optional target proportion (0-1) to show as reference line.
#' @param pct_scale If `TRUE`, display y-axis as percentages (default TRUE).
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_caterpillar_proportion <- function(metric_df, title = NULL,
                                         y_label = "Proportion (%)",
                                         target = NULL,
                                         pct_scale = TRUE,
                                         x_label = NULL) {
  # Back-compat: if caller passes x_label (old API), use as y_label

if (!is.null(x_label) && missing(y_label)) y_label <- x_label

  has_suppressed <- "suppressed" %in% names(metric_df)

  # Separate suppressed and non-suppressed data
  if (has_suppressed) {
    plot_df <- metric_df |> dplyr::filter(!.data$suppressed)
    suppressed_df <- metric_df |> dplyr::filter(.data$suppressed)
  } else {
    plot_df <- metric_df
    suppressed_df <- metric_df[0, ]
  }

  nat_avg <- national_average_proportion(metric_df)

  multiplier <- if (pct_scale) 100 else 1

  # Use centre_code for axis labels
  label_var <- "centre_code"

  # Early return if all centres suppressed
  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = title %||% "All centres suppressed (n<10)")
    )
  }

  # Order centres descending by proportion (highest on left)
  plot_df <- plot_df |>
    dplyr::mutate(
      centre_ordered = stats::reorder(.data[[label_var]],
                                       -.data$proportion)
    )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = .data$centre_ordered,
      y = .data$proportion * multiplier
    )
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$lower_ci * multiplier,
        ymax = .data$upper_ci * multiplier
      ),
      width = 0.3, colour = "black", linewidth = 0.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = "%"),
      size = 2.5, colour = "black"
    )

  if (!is.null(nat_avg) && !is.na(nat_avg)) {
    p <- p + ggplot2::geom_hline(
      ggplot2::aes(yintercept = nat_avg * multiplier,
                   colour = "National mean"),
      linetype = "solid", linewidth = 0.7
    ) +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = c("National mean" = "red")
      )
  }

  if (!is.null(target)) {
    p <- p + ggplot2::geom_hline(
      yintercept = target * multiplier,
      linetype = "dotted", colour = "#28A745", linewidth = 0.8
    )
  }

  p <- p +
    ggplot2::scale_shape_manual(
      name = NULL,
      values = c("%" = 19),
      labels = c("%" = "%"),
      guide = ggplot2::guide_legend(order = 1)
    ) +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      y = y_label,
      x = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 15, b = 60, l = 15),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1,
                                            size = 9),
      legend.position = "right",
      legend.justification = c(1, 1),
      legend.margin = ggplot2::margin(l = 5),
      plot.title = ggplot2::element_text(size = 13, face = "bold")
    )

  if (pct_scale) {
    p <- p + ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(x))
    )
  }

  p
}

#' Caterpillar plot for medians
#'
#' Creates a vertical ordered dot-and-whisker plot showing centre medians
#' with confidence intervals. Style matches IKDDS audit report.
#'
#' @param metric_df Tibble from [compute_centre_median()].
#' @param title Plot title.
#' @param y_label Y-axis label.
#' @param target Optional target value to show as reference line.
#'
#' @return A ggplot2 object.
#'
#' @export
#' @family plots
plot_caterpillar_median <- function(metric_df, title = NULL,
                                     y_label = "Value",
                                     target = NULL,
                                     x_label = NULL) {
  # Back-compat: if caller passes x_label (old API), use as y_label
  if (!is.null(x_label) && missing(y_label)) y_label <- x_label

  has_suppressed <- "suppressed" %in% names(metric_df)

  # Separate suppressed and non-suppressed data
  if (has_suppressed) {
    plot_df <- metric_df |> dplyr::filter(!.data$suppressed)
    suppressed_df <- metric_df |> dplyr::filter(.data$suppressed)
  } else {
    plot_df <- metric_df
    suppressed_df <- metric_df[0, ]
  }

  nat_avg <- national_average_median(metric_df)

  # Use centre_code for axis labels
  label_var <- "centre_code"

  # Early return if all centres suppressed
  if (nrow(plot_df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = title %||% "All centres suppressed (n<10)")
    )
  }

  # Order centres descending by median (highest on left)
  plot_df <- plot_df |>
    dplyr::mutate(
      centre_ordered = stats::reorder(.data[[label_var]],
                                       -.data$median)
    )

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$centre_ordered, y = .data$median)
  ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$lower_ci, ymax = .data$upper_ci),
      width = 0.3, colour = "black", linewidth = 0.5
    ) +
    ggplot2::geom_point(size = 2.5, colour = "black")

  if (!is.null(nat_avg) && !is.na(nat_avg)) {
    p <- p + ggplot2::geom_hline(
      ggplot2::aes(yintercept = nat_avg, colour = "National mean"),
      linetype = "solid", linewidth = 0.7
    ) +
      ggplot2::scale_colour_manual(
        name = NULL,
        values = c("National mean" = "red")
      )
  }

  if (!is.null(target)) {
    p <- p + ggplot2::geom_hline(
      yintercept = target,
      linetype = "dotted", colour = "#28A745", linewidth = 0.8
    )
  }

  p +
    ggplot2::labs(
      title = title %||% unique(metric_df$metric),
      y = y_label,
      x = NULL
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(t = 10, r = 15, b = 60, l = 15),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1,
                                            size = 9),
      legend.position = "right",
      legend.justification = c(1, 1),
      legend.margin = ggplot2::margin(l = 5),
      plot.title = ggplot2::element_text(size = 13, face = "bold")
    )
}

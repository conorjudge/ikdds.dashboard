#' Wilson score confidence interval for a proportion
#'
#' Computes the Wilson score interval, which has better coverage properties
#' than the normal approximation, especially for small samples.
#'
#' @param x Number of successes.
#' @param n Number of trials.
#' @param conf_level Confidence level (default 0.95).
#'
#' @return A list with `estimate`, `lower`, `upper`.
#'
#' @export
#' @family metrics-helpers
#'
#' @examples
#' wilson_ci(30, 100)
wilson_ci <- function(x, n, conf_level = 0.95) {
  if (n == 0) {
    return(list(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  p_hat <- x / n
  denom <- 1 + z^2 / n
  centre <- (p_hat + z^2 / (2 * n)) / denom
  margin <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n) / denom

  list(
    estimate = p_hat,
    lower = max(0, centre - margin),
    upper = min(1, centre + margin)
  )
}

#' Confidence interval for a median (order statistic method)
#'
#' Uses the exact binomial method to compute the CI for a population median.
#'
#' @param x Numeric vector (NAs removed internally).
#' @param conf_level Confidence level (default 0.95).
#'
#' @return A list with `estimate`, `lower`, `upper`.
#'
#' @export
#' @family metrics-helpers
#'
#' @examples
#' median_ci(rnorm(50))
median_ci <- function(x, conf_level = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n == 0) {
    return(list(estimate = NA_real_, lower = NA_real_, upper = NA_real_))
  }

  if (n < 3) {
    return(list(
      estimate = stats::median(x),
      lower = min(x),
      upper = max(x)
    ))
  }

  x_sorted <- sort(x)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)

  # Exact binomial CI for order statistics

  lo_idx <- max(1, floor(n / 2 - z * sqrt(n) / 2))
  hi_idx <- min(n, ceiling(n / 2 + 1 + z * sqrt(n) / 2))

  list(
    estimate = stats::median(x),
    lower = x_sorted[lo_idx],
    upper = x_sorted[hi_idx]
  )
}

#' Percentage in range
#'
#' Computes the proportion of non-missing values that fall within
#' a specified range, inclusive of bounds.
#'
#' @param x Numeric vector.
#' @param lower Lower bound (use `-Inf` for no lower bound).
#' @param upper Upper bound (use `Inf` for no upper bound).
#'
#' @return A list with `n_total`, `n_valid`, `n_in_range`, `pct`.
#'
#' @export
#' @family metrics-helpers
#'
#' @examples
#' pct_in_range(c(1.0, 1.5, 2.0, NA), lower = 1.1, upper = 1.7)
pct_in_range <- function(x, lower = -Inf, upper = Inf) {
  n_total <- length(x)
  x_valid <- x[!is.na(x)]
  n_valid <- length(x_valid)
  n_in_range <- sum(x_valid >= lower & x_valid <= upper)

  list(
    n_total = n_total,
    n_valid = n_valid,
    n_in_range = n_in_range,
    pct = if (n_valid > 0) n_in_range / n_valid * 100 else NA_real_
  )
}

#' Spiegelhalter funnel plot limits
#'
#' Computes 95% and 99.7% (2-sigma and 3-sigma) control limits for a
#' funnel plot of proportions, following the methodology used by the
#' UK Renal Registry.
#'
#' @param target_rate National/overall target proportion (0-1 scale).
#' @param n_range Integer vector of sample sizes to compute limits for.
#'
#' @return A tibble with columns `n`, `lower_95`, `upper_95`,
#'   `lower_997`, `upper_997`.
#'
#' @export
#' @family metrics-helpers
#'
#' @examples
#' funnel_limits(0.65, n_range = 10:200)
funnel_limits <- function(target_rate, n_range) {
  z95 <- stats::qnorm(0.975)
  z997 <- stats::qnorm(0.9985)

  se <- sqrt(target_rate * (1 - target_rate) / n_range)

  tibble::tibble(
    n = n_range,
    lower_95  = pmax(0, target_rate - z95 * se),
    upper_95  = pmin(1, target_rate + z95 * se),
    lower_997 = pmax(0, target_rate - z997 * se),
    upper_997 = pmin(1, target_rate + z997 * se)
  )
}

#' Compute centre-level proportion with Wilson CI
#'
#' Wrapper that groups audit data by centre, counts successes, and returns
#' a tibble with Wilson score CIs ready for caterpillar/funnel plotting.
#'
#' @param df Audit data tibble.
#' @param value_col Unquoted column name of the numeric value.
#' @param lower Lower bound of target range.
#' @param upper Upper bound of target range.
#' @param metric_label Human-readable label for the metric.
#'
#' @return A tibble with columns: `centre_code`, `centre_name`, `n`, `x`,
#'   `proportion`, `lower_ci`, `upper_ci`, `metric`.
#'
#' @export
#' @family metrics-helpers
compute_centre_proportion <- function(df, value_col, lower = -Inf, upper = Inf,
                                       metric_label = "Metric") {
  value_col <- rlang::enquo(value_col)

  result <- df %>%
    dplyr::filter(!is.na(!!value_col)) %>%
    dplyr::group_by(.data$centre_code, .data$centre_name) %>%
    dplyr::summarise(
      n = dplyr::n(),
      x = sum(!!value_col >= lower & !!value_col <= upper),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci = list(wilson_ci(.data$x, .data$n)),
      proportion = .data$ci$estimate,
      lower_ci = .data$ci$lower,
      upper_ci = .data$ci$upper
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"ci") %>%
    dplyr::mutate(metric = metric_label)

  result
}

#' Compute centre-level median with CI
#'
#' Wrapper that groups audit data by centre and returns median with CI.
#'
#' @param df Audit data tibble.
#' @param value_col Unquoted column name of the numeric value.
#' @param metric_label Human-readable label for the metric.
#'
#' @return A tibble with columns: `centre_code`, `centre_name`, `n`,
#'   `median`, `lower_ci`, `upper_ci`, `metric`.
#'
#' @export
#' @family metrics-helpers
compute_centre_median <- function(df, value_col, metric_label = "Metric") {
  value_col <- rlang::enquo(value_col)

  df %>%
    dplyr::filter(!is.na(!!value_col)) %>%
    dplyr::group_by(.data$centre_code, .data$centre_name) %>%
    dplyr::summarise(
      n = dplyr::n(),
      values = list(!!value_col),
      .groups = "drop"
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ci = list(median_ci(.data$values)),
      median = .data$ci$estimate,
      lower_ci = .data$ci$lower,
      upper_ci = .data$ci$upper
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-"values", -"ci") %>%
    dplyr::mutate(metric = metric_label)
}

#' Apply small-n suppression to a metric data frame
#'
#' Sets proportion/median/CI values to NA where the centre sample size is
#' below the minimum threshold. Adds a logical `suppressed` column.
#'
#' @param metric_df Tibble from `compute_centre_proportion()` or
#'   `compute_centre_median()`.
#' @param min_n Minimum sample size (default 10).
#'
#' @return The input tibble with `suppressed` column added and values
#'   set to NA for suppressed rows.
#'
#' @export
#' @family metrics-engine
apply_suppression <- function(metric_df, min_n = 10) {
  metric_df <- metric_df |>
    dplyr::mutate(suppressed = .data$n < min_n)

  # Determine which value columns exist and NA them for suppressed rows
  # Include "n" to prevent leaking exact small counts (privacy)
  value_cols <- intersect(
    c("proportion", "median", "lower_ci", "upper_ci", "x", "n"),
    names(metric_df)
  )

  for (col in value_cols) {
    metric_df[[col]] <- ifelse(metric_df$suppressed, NA_real_, metric_df[[col]])
  }

  metric_df
}

#' Add completeness labels to a metric data frame
#'
#' Computes the percentage of missing values per group from the raw data
#' and adds `pct_missing` and `label` columns to the metric data frame.
#' The label format is "Centre Name (X% missing)".
#'
#' @param metric_df Tibble from `compute_centre_proportion()` or similar.
#' @param raw_data The raw audit data tibble (before aggregation).
#' @param value_field Character name of the column to check missingness for.
#' @param group_col Character name of the grouping column (default "centre_code").
#' @param label_col Character name of the label column (default "centre_name").
#'
#' @return The input tibble with `pct_missing` and `label` columns added.
#'
#' @export
#' @family metrics-engine
add_completeness_labels <- function(metric_df, raw_data, value_field,
                                     group_col = "centre_code",
                                     label_col = "centre_name") {
  if (!value_field %in% names(raw_data)) {
    # If the field doesn't exist, skip completeness
    metric_df$pct_missing <- NA_real_
    if (label_col %in% names(metric_df)) {
      metric_df$label <- metric_df[[label_col]]
    } else {
      metric_df$label <- metric_df[[group_col]]
    }
    return(metric_df)
  }

  # Ensure group_col exists in raw_data

  if (!group_col %in% names(raw_data)) {
    metric_df$pct_missing <- NA_real_
    if (label_col %in% names(metric_df)) {
      metric_df$label <- metric_df[[label_col]]
    } else if (group_col %in% names(metric_df)) {
      metric_df$label <- metric_df[[group_col]]
    } else {
      metric_df$label <- NA_character_
    }
    return(metric_df)
  }

  # Compute missingness per group
  missing_info <- raw_data |>
    dplyr::group_by(.data[[group_col]]) |>
    dplyr::summarise(
      n_total_raw = dplyr::n(),
      n_missing = sum(is.na(.data[[value_field]])),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      pct_missing = round(.data$n_missing / .data$n_total_raw * 100, 0)
    ) |>
    dplyr::select(dplyr::all_of(group_col), "pct_missing")

  metric_df <- metric_df |>
    dplyr::left_join(missing_info, by = group_col)

  # Build label with missingness annotation
  lbl_col_use <- if (label_col %in% names(metric_df)) {
    label_col
  } else if (group_col %in% names(metric_df)) {
    group_col
  } else {
    NULL
  }

  if (!is.null(lbl_col_use)) {
    metric_df <- metric_df |>
      dplyr::mutate(
        label = dplyr::if_else(
          is.na(.data$pct_missing) | .data$pct_missing == 0,
          .data[[lbl_col_use]],
          paste0(.data[[lbl_col_use]], " (", .data$pct_missing, "% missing)")
        )
      )
  } else {
    metric_df$label <- NA_character_
  }

  metric_df
}

#' Compute a metric through the engine pipeline
#'
#' Looks up the metric configuration, calls the compute function,
#' adds completeness labels, and applies suppression.
#'
#' @param data Audit data tibble.
#' @param metric_id Character string matching a key in `metrics.yml`.
#' @param registry Optional pre-loaded registry.
#' @param group_col Grouping column: "centre_code" or "consultant" (default "centre_code").
#'
#' @return An enriched metric data frame with `suppressed`, `pct_missing`,
#'   and `label` columns.
#'
#' @export
#' @family metrics-engine
compute_with_engine <- function(data, metric_id, registry = metrics_registry(),
                                 group_col = "centre_code") {
  cfg <- get_metric_config(metric_id, registry)

  # Resolve the compute function from the package namespace
  fn_name <- cfg$compute_fn
  fn <- tryCatch(
    get(fn_name, envir = asNamespace("ikdds.dashboard"), mode = "function"),
    error = function(e) {
      # Fallback to global search path
      tryCatch(
        match.fun(fn_name),
        error = function(e2) {
          cli::cli_abort("Cannot find compute function: {fn_name}")
        }
      )
    }
  )

  # Call the compute function
  # For functions that use compute_centre_proportion/median internally,
  # we pass group_col if they accept it
  fn_args <- formals(fn)
  if ("group_col" %in% names(fn_args)) {
    metric_df <- fn(data, group_col = group_col)
  } else {
    metric_df <- fn(data)
  }

  # Determine label column based on group
  label_col <- if (group_col == "consultant") "consultant" else "centre_name"

  # Add completeness labels
  value_field <- cfg$value_field
  if (!is.null(value_field)) {
    metric_df <- add_completeness_labels(
      metric_df, data, value_field,
      group_col = group_col,
      label_col = label_col
    )
  }

  # Apply suppression
  sup_cfg <- cfg$suppression
  min_n <- if (!is.null(sup_cfg$min_n)) sup_cfg$min_n else 10
  metric_df <- apply_suppression(metric_df, min_n = min_n)

  metric_df
}

#' Compute a metric grouped by consultant
#'
#' Convenience wrapper around [compute_with_engine()] that groups by
#' consultant instead of centre.
#'
#' @param data Audit data tibble.
#' @param metric_id Character string matching a key in `metrics.yml`.
#' @param registry Optional pre-loaded registry.
#'
#' @return An enriched metric data frame grouped by consultant.
#'
#' @export
#' @family metrics-engine
compute_consultant_metric <- function(data, metric_id,
                                       registry = metrics_registry()) {
  compute_with_engine(data, metric_id, registry = registry,
                       group_col = "consultant")
}

#' Compute risk-adjusted metric (stub)
#'
#' Placeholder for future hierarchical model-based risk adjustment.
#'
#' @param data Audit data tibble.
#' @param metric_id Character string.
#'
#' @return NULL (not yet implemented).
#'
#' @export
#' @family metrics-engine
compute_risk_adjusted <- function(data, metric_id) {
  # TODO: Implement hierarchical model (Phase 2)
  # Will use lme4 or brms for risk-adjusted centre comparisons
  NULL
}

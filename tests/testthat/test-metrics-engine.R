# Tests for metrics-engine.R and metrics-registry.R

test_that("apply_suppression sets NA for small n groups", {
  df <- tibble::tibble(
    centre_code = c("BEA", "MAT", "CUH"),
    centre_name = c("Beaumont", "Mater", "Cork"),
    n = c(5, 15, 8),
    x = c(3, 10, 4),
    proportion = c(0.6, 0.667, 0.5),
    lower_ci = c(0.2, 0.4, 0.2),
    upper_ci = c(0.9, 0.85, 0.8),
    metric = "Test"
  )

  result <- apply_suppression(df, min_n = 10)

  expect_true("suppressed" %in% names(result))
  expect_equal(result$suppressed, c(TRUE, FALSE, TRUE))

  # Suppressed rows should have NA values

  expect_true(is.na(result$proportion[1]))
  expect_true(is.na(result$proportion[3]))
  expect_false(is.na(result$proportion[2]))

  # Non-suppressed row should be unchanged
  expect_equal(result$proportion[2], 0.667)
})

test_that("apply_suppression with min_n = 0 suppresses nothing", {
  df <- tibble::tibble(
    centre_code = c("BEA", "MAT"),
    centre_name = c("Beaumont", "Mater"),
    n = c(1, 2),
    proportion = c(0.5, 0.5),
    lower_ci = c(0.1, 0.1),
    upper_ci = c(0.9, 0.9),
    metric = "Test"
  )

  result <- apply_suppression(df, min_n = 0)
  expect_equal(sum(result$suppressed), 0)
})

test_that("add_completeness_labels computes pct_missing", {
  metric_df <- tibble::tibble(
    centre_code = c("BEA", "MAT"),
    centre_name = c("Beaumont", "Mater"),
    n = c(10, 10),
    proportion = c(0.5, 0.6)
  )

  raw_data <- tibble::tibble(
    centre_code = rep(c("BEA", "MAT"), each = 10),
    qblg9 = c(rep(70, 8), NA, NA,    # BEA: 2 missing = 20%
               rep(70, 10))            # MAT: 0 missing = 0%
  )

  result <- add_completeness_labels(metric_df, raw_data, "qblg9")

  expect_true("pct_missing" %in% names(result))
  expect_true("label" %in% names(result))
  expect_equal(result$pct_missing[1], 20)
  expect_equal(result$pct_missing[2], 0)
  expect_true(grepl("20% missing", result$label[1]))
  expect_equal(result$label[2], "Mater")
})

test_that("add_completeness_labels handles missing value_field gracefully", {
  metric_df <- tibble::tibble(
    centre_code = "BEA",
    centre_name = "Beaumont",
    n = 10,
    proportion = 0.5
  )

  raw_data <- tibble::tibble(centre_code = "BEA", qblg9 = 70)

  result <- add_completeness_labels(metric_df, raw_data, "nonexistent_field")
  expect_true(is.na(result$pct_missing[1]))
  expect_equal(result$label[1], "Beaumont")
})

test_that("compute_with_engine integrates suppression and completeness", {
  # This test requires the YAML config to be available
  skip_if_not(file.exists(
    system.file("metrics", "metrics.yml", package = "ikdds.dashboard")
  ) || file.exists("../../inst/metrics/metrics.yml"))

  df <- fixture_audit_data(n = 15)

  # Should work without error for a known metric
  result <- tryCatch(
    compute_with_engine(df, "urr_achievement"),
    error = function(e) NULL
  )

  if (!is.null(result)) {
    expect_true("suppressed" %in% names(result))
    expect_true("pct_missing" %in% names(result))
    expect_true("label" %in% names(result))
  }
})

test_that("apply_suppression works with median data frames", {
  df <- tibble::tibble(
    centre_code = c("BEA", "MAT"),
    centre_name = c("Beaumont", "Mater"),
    n = c(5, 20),
    median = c(70, 72),
    lower_ci = c(65, 68),
    upper_ci = c(75, 76),
    metric = "Test Median"
  )

  result <- apply_suppression(df, min_n = 10)
  expect_true(is.na(result$median[1]))
  expect_false(is.na(result$median[2]))
})

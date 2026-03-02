# Tests for metrics helper functions

test_that("wilson_ci returns correct estimate", {
  ci <- wilson_ci(30, 100)
  expect_equal(ci$estimate, 0.30)
  expect_true(ci$lower < 0.30)
  expect_true(ci$upper > 0.30)
})

test_that("wilson_ci handles zero denominator", {
  ci <- wilson_ci(0, 0)
  expect_true(is.na(ci$estimate))
  expect_true(is.na(ci$lower))
  expect_true(is.na(ci$upper))
})

test_that("wilson_ci bounds are within [0,1]", {
  ci <- wilson_ci(1, 100)
  expect_true(ci$lower >= 0)
  ci <- wilson_ci(99, 100)
  expect_true(ci$upper <= 1)
})

test_that("wilson_ci is symmetric for p=0.5", {
  ci <- wilson_ci(50, 100)
  expect_equal(round(0.5 - ci$lower, 4), round(ci$upper - 0.5, 4))
})

test_that("median_ci returns correct median", {
  x <- c(1, 2, 3, 4, 5)
  ci <- median_ci(x)
  expect_equal(ci$estimate, 3)
  expect_true(ci$lower <= 3)
  expect_true(ci$upper >= 3)
})

test_that("median_ci handles empty vector", {
  ci <- median_ci(numeric(0))
  expect_true(is.na(ci$estimate))
})

test_that("median_ci handles NAs", {
  x <- c(1, NA, 3, NA, 5)
  ci <- median_ci(x)
  expect_equal(ci$estimate, 3)
})

test_that("pct_in_range computes correctly", {
  result <- pct_in_range(c(1.0, 1.5, 2.0, 1.3), lower = 1.1, upper = 1.7)
  expect_equal(result$n_total, 4)
  expect_equal(result$n_valid, 4)
  expect_equal(result$n_in_range, 2)
  expect_equal(result$pct, 50)
})

test_that("pct_in_range handles NAs", {
  result <- pct_in_range(c(1.5, NA, 2.0), lower = 1.1, upper = 1.7)
  expect_equal(result$n_total, 3)
  expect_equal(result$n_valid, 2)
  expect_equal(result$n_in_range, 1)
  expect_equal(result$pct, 50)
})

test_that("pct_in_range handles all NA", {
  result <- pct_in_range(c(NA, NA), lower = 1, upper = 2)
  expect_true(is.na(result$pct))
})

test_that("funnel_limits returns correct structure", {
  fl <- funnel_limits(0.65, n_range = 50:100)
  expect_s3_class(fl, "tbl_df")
  expect_equal(nrow(fl), 51)
  expect_true(all(c("n", "lower_95", "upper_95", "lower_997", "upper_997") %in% names(fl)))
})

test_that("funnel_limits are monotonically narrowing", {
  fl <- funnel_limits(0.5, n_range = 10:200)
  expect_true(all(diff(fl$upper_95 - fl$lower_95) <= 0))
})

test_that("funnel_limits bounds are within [0,1]", {
  fl <- funnel_limits(0.5, n_range = 1:500)
  expect_true(all(fl$lower_95 >= 0))
  expect_true(all(fl$upper_95 <= 1))
  expect_true(all(fl$lower_997 >= 0))
  expect_true(all(fl$upper_997 <= 1))
})

test_that("compute_centre_proportion works with fixture data", {
  df <- fixture_audit_data_minimal()
  result <- compute_centre_proportion(df, .data$qblg9, lower = 65, upper = Inf,
                                       metric_label = "URR >65%")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)  # 2 centres
  expect_true(all(c("centre_code", "proportion", "lower_ci", "upper_ci") %in% names(result)))
})

test_that("compute_centre_median works with fixture data", {
  df <- fixture_audit_data_minimal()
  result <- compute_centre_median(df, .data$qblg9, metric_label = "URR Median")
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("median" %in% names(result))
})

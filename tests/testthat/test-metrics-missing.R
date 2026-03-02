# Tests for missing data metrics

test_that("compute_missing_data returns correct structure", {
  df <- fixture_audit_data_minimal()
  result <- compute_missing_data(df)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("centre_code", "variable", "n_total",
                     "n_missing", "pct_missing") %in% names(result)))
})

test_that("compute_missing_data detects zero missingness on complete data", {
  df <- fixture_audit_data_minimal()
  result <- compute_missing_data(df, variables = "age")

  # age has no missing in minimal fixture
  expect_true(all(result$pct_missing == 0))
})

test_that("compute_missing_national returns one row per variable", {
  df <- fixture_audit_data_minimal()
  result <- compute_missing_national(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), length(unique(result$variable)))
})

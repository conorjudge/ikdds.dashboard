# Tests for demographics metrics

test_that("compute_patient_counts returns correct counts", {
  df <- fixture_audit_data_minimal()
  result <- compute_patient_counts(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$n_patients == 3))
})

test_that("compute_age_distribution returns summary and bins", {
  df <- fixture_audit_data_minimal()
  result <- compute_age_distribution(df)

  expect_type(result, "list")
  expect_true("summary" %in% names(result))
  expect_true("bins" %in% names(result))

  expect_equal(nrow(result$summary), 2)
  expect_true("median" %in% names(result$summary))
  # Check IQR is present instead of SD
  expect_true("iqr" %in% names(result$summary))
  expect_true("q1" %in% names(result$summary))
  expect_true("q3" %in% names(result$summary))
  # Ages should be integers (rounded)
  expect_true(all(result$summary$median == round(result$summary$median)))
})

test_that("compute_gender_breakdown sums to 100%", {
  df <- fixture_audit_data_minimal()
  result <- compute_gender_breakdown(df)

  by_centre <- result |>
    dplyr::group_by(.data$centre_code) |>
    dplyr::summarise(total_pct = sum(.data$pct))

  expect_true(all(abs(by_centre$total_pct - 100) < 0.2))
})

test_that("compute_ethnicity_breakdown returns expected structure", {
  df <- fixture_audit_data_minimal()
  result <- compute_ethnicity_breakdown(df)

  expect_s3_class(result, "tbl_df")
  expect_true("ethnicity" %in% names(result))
  expect_true("pct_missing" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("compute_ethnicity_breakdown handles missing ethnicity column", {
  df <- fixture_audit_data_minimal()
  df$ethnicity <- NULL
  result <- compute_ethnicity_breakdown(df)
  expect_equal(nrow(result), 0)
})

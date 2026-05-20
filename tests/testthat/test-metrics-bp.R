# Tests for blood pressure metrics

test_that("compute_pre_bp_achievement works correctly", {
  df <- fixture_audit_data_minimal()
  result <- compute_pre_bp_achievement(df)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true(all(result$proportion >= 0 & result$proportion <= 1))

  # BEA: patients have SBP 135, 150, 128 and DBP 80, 95, 75

  # <140/90: patient 1 (135/80) and patient 3 (128/75) = 2/3
  bea <- result |> dplyr::filter(.data$centre_code == "BEA")
  expect_equal(round(bea$proportion, 4), round(2/3, 4))
})

test_that("compute_post_bp_achievement works correctly", {
  df <- fixture_audit_data_minimal()
  result <- compute_post_bp_achievement(df)

  expect_s3_class(result, "tbl_df")
  expect_true(all(result$proportion >= 0 & result$proportion <= 1))
})

test_that("compute_bp_summary returns all columns", {
  df <- fixture_audit_data_minimal()
  result <- compute_bp_summary(df)

  expected_cols <- c("pre_sbp_mean", "pre_dbp_mean",
                     "post_sbp_mean", "post_dbp_mean")
  expect_true(all(expected_cols %in% names(result)))
})

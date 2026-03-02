# Tests for vascular access metrics

test_that("compute_access_distribution returns all access types", {
  df <- fixture_audit_data_minimal()
  result <- compute_access_distribution(df)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("access_type", "count", "pct") %in% names(result)))
  expect_true(all(result$access_type %in% c("AVF", "AVG", "Catheter")))
})

test_that("compute_access_distribution percentages sum to 100", {
  df <- fixture_audit_data_minimal()
  result <- compute_access_distribution(df)

  by_centre <- result %>%
    dplyr::group_by(.data$centre_code) %>%
    dplyr::summarise(total_pct = sum(.data$pct))

  expect_true(all(abs(by_centre$total_pct - 100) < 0.2))
})

test_that("compute_avf_rate returns proportions with CIs", {
  df <- fixture_audit_data_minimal()
  result <- compute_avf_rate(df)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("proportion", "lower_ci", "upper_ci") %in% names(result)))
  expect_true(all(result$proportion >= 0 & result$proportion <= 1))
})

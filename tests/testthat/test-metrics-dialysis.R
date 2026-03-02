# Tests for dialysis metrics

test_that("compute_urr_median returns expected structure", {
  df <- fixture_audit_data_minimal()
  result <- compute_urr_median(df)
  expect_s3_class(result, "tbl_df")
  expect_true("median" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("compute_urr_median filters 3x/week correctly", {
  df <- fixture_audit_data_minimal()
  # Patient 5 has hdp01 = 2, should be excluded in 3x filter
  result_all <- compute_urr_median(df, filter_3x = FALSE)
  result_3x  <- compute_urr_median(df, filter_3x = TRUE)

  mat_all <- result_all %>% dplyr::filter(.data$centre_code == "MAT")
  mat_3x  <- result_3x %>% dplyr::filter(.data$centre_code == "MAT")

  expect_equal(mat_all$n, 3)
  expect_equal(mat_3x$n, 2)  # one patient excluded
})

test_that("compute_urr_achievement returns proportions", {
  df <- fixture_audit_data_minimal()
  result <- compute_urr_achievement(df)

  expect_true(all(result$proportion >= 0 & result$proportion <= 1))
  expect_true("lower_ci" %in% names(result))
})

test_that("compute_session_frequency returns all categories", {
  df <- fixture_audit_data_minimal()
  result <- compute_session_frequency(df)

  expect_true("hd_freq" %in% names(result))
  expect_true(any(result$hd_freq == "3x/week"))
})

test_that("compute_session_duration returns duration bands", {
  df <- fixture_audit_data_minimal()
  result <- compute_session_duration(df)

  expect_true("duration_band" %in% names(result))
})

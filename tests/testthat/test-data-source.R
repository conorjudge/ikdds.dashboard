# Tests for data source factory

test_that("get_audit_data returns data for synthetic source", {
  config <- mock_dashboard_config("synthetic")
  df <- get_audit_data(config)

  expect_s3_class(df, "tbl_df")
  expect_true(nrow(df) > 0)
  expect_true("record_id" %in% names(df))
  expect_true("centre_code" %in% names(df))
})

test_that("get_audit_data errors for unknown source", {
  config <- mock_dashboard_config("synthetic")
  config$data_source <- "unknown"
  expect_error(get_audit_data(config), "Unknown data source")
})

test_that("validate_audit_data passes for valid data", {
  df <- fixture_audit_data()
  expect_invisible(validate_audit_data(df))
})

test_that("validate_audit_data errors for missing columns", {
  df <- tibble::tibble(record_id = "1", centre_code = "BEA")
  expect_error(validate_audit_data(df), "missing required columns")
})

test_that("audit_data_columns returns expected length", {
  cols <- audit_data_columns()
  expect_equal(length(cols), 28)
  expect_true("record_id" %in% cols)
  expect_true("qblg9" %in% cols)
})

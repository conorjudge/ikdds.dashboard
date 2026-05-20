# Tests for new metrics added in clinical feedback update

test_that("compute_unmapped_patients returns correct structure", {
  df <- fixture_audit_data_minimal()
  result <- compute_unmapped_patients(df)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("centre_code", "centre_name", "n_total",
                     "n_unmapped", "pct_unmapped") %in% names(result)))
})

test_that("compute_unmapped_patients detects NA consultants", {
  df <- fixture_audit_data_minimal()
  df$consultant[1] <- NA_character_  # Make one patient unmapped
  result <- compute_unmapped_patients(df)

  bea <- result[result$centre_code == "BEA", ]
  expect_equal(bea$n_unmapped, 1)
})

test_that("compute_ca_po4_achievement returns correct structure", {
  df <- fixture_audit_data_minimal()
  result <- compute_ca_po4_achievement(df)

  expect_s3_class(result, "tbl_df")
  expect_true("proportion" %in% names(result))
  expect_true(all(result$proportion >= 0 & result$proportion <= 1))
})

test_that("validate_machine_fields replaces zeros with NA", {
  df <- fixture_audit_data_minimal()
  df$qblg9[1] <- 0
  df$qblg3[2] <- 0

  result <- validate_machine_fields(df)
  expect_true(is.na(result$qblg9[1]))
  expect_true(is.na(result$qblg3[2]))
  # Non-zero values should be unchanged
  expect_false(is.na(result$qblg9[2]))
})

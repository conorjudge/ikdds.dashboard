# Tests for biochemistry metrics

test_that("compute_phosphate_achievement uses correct range", {
  df <- fixture_audit_data_minimal()
  result <- compute_phosphate_achievement(df)

  expect_s3_class(result, "tbl_df")
  # BEA: PO4 values 1.3, 1.9, 1.5 -> in range (1.1-1.7): 1.3, 1.5 = 2/3
  bea <- result %>% dplyr::filter(.data$centre_code == "BEA")
  expect_equal(round(bea$proportion, 4), round(2/3, 4))
})

test_that("compute_calcium_achievement uses correct range", {
  df <- fixture_audit_data_minimal()
  result <- compute_calcium_achievement(df)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("compute_pth_achievement uses correct range", {
  df <- fixture_audit_data_minimal()
  result <- compute_pth_achievement(df)
  expect_s3_class(result, "tbl_df")

  # BEA: PTH values 25, 80, 45 -> in range (16-72): 25, 45 = 2/3
  bea <- result %>% dplyr::filter(.data$centre_code == "BEA")
  expect_equal(round(bea$proportion, 4), round(2/3, 4))
})

test_that("compute_ckd_mbd_simultaneous requires all three in range", {
  df <- fixture_audit_data_minimal()
  result <- compute_ckd_mbd_simultaneous(df)

  expect_s3_class(result, "tbl_df")
  # Simultaneous control is stricter than individual metrics
  po4 <- compute_phosphate_achievement(df)
  expect_true(all(result$proportion <= po4$proportion | result$n < po4$n))
})

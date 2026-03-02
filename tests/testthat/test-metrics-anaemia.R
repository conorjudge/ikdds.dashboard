# Tests for anaemia metrics

test_that("compute_hb_median returns expected values", {
  df <- fixture_audit_data_minimal()
  result <- compute_hb_median(df)

  expect_s3_class(result, "tbl_df")
  expect_true("median" %in% names(result))
  # BEA: Hb 10.5, 9.0, 11.5 -> median = 10.5
  bea <- result %>% dplyr::filter(.data$centre_code == "BEA")
  expect_equal(bea$median, 10.5)
})

test_that("compute_hb_achievement uses 10-12 range", {
  df <- fixture_audit_data_minimal()
  result <- compute_hb_achievement(df)

  # BEA: Hb 10.5, 9.0, 11.5 -> in range (10-12): 10.5, 11.5 = 2/3
  bea <- result %>% dplyr::filter(.data$centre_code == "BEA")
  expect_equal(round(bea$proportion, 4), round(2/3, 4))
})

test_that("compute_hb_distribution returns all bands", {
  df <- fixture_audit_data(n = 20)
  result <- compute_hb_distribution(df)

  expect_true("hb_band" %in% names(result))
  expect_true("count" %in% names(result))
  expect_true("pct" %in% names(result))
})

test_that("compute_ferritin_achievement uses >=200 threshold", {
  df <- fixture_audit_data_minimal()
  result <- compute_ferritin_achievement(df)

  # BEA: Ferritin 250, 150, 400 -> >=200: 250, 400 = 2/3
  bea <- result %>% dplyr::filter(.data$centre_code == "BEA")
  expect_equal(round(bea$proportion, 4), round(2/3, 4))
})

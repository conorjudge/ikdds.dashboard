# Tests for data quality module logic

test_that("compute_missing_data returns expected structure", {
  df <- fixture_audit_data(n = 10)

  result <- compute_missing_data(df)

  expect_true(tibble::is_tibble(result))
  expect_true(all(c("centre_code", "centre_name", "variable",
                      "n_total", "n_missing", "pct_missing") %in% names(result)))
  expect_true(nrow(result) > 0)
})

test_that("compute_missing_data detects missing values correctly", {
  df <- fixture_audit_data_minimal()
  # Introduce some NAs
  df$qblg9[1] <- NA
  df$qblg9[4] <- NA

  result <- compute_missing_data(df, variables = "qblg9")

  bea <- result[result$centre_code == "BEA", ]
  mat <- result[result$centre_code == "MAT", ]

  expect_equal(bea$n_missing, 1)
  expect_equal(mat$n_missing, 1)
})

test_that("compute_missing_national returns one row per variable", {
  df <- fixture_audit_data(n = 10)

  result <- compute_missing_national(df)

  expect_true(tibble::is_tibble(result))
  expect_true(all(c("variable", "n_total", "n_missing", "pct_missing") %in%
                     names(result)))
  # Should have as many rows as variables
  expect_true(nrow(result) > 0)
  # All pct_missing should be numeric
  expect_true(is.numeric(result$pct_missing))
})

test_that("completeness matrix pivot works correctly", {
  df <- fixture_audit_data(n = 10)

  md <- compute_missing_data(df)
  mat <- md |>
    dplyr::mutate(pct_complete = 100 - pct_missing) |>
    dplyr::select(centre_code, centre_name, variable, pct_complete) |>
    tidyr::pivot_wider(
      id_cols = c("centre_code", "centre_name"),
      names_from = "variable",
      values_from = "pct_complete"
    )

  # Should have one row per centre
  expect_equal(nrow(mat), 3)
  # All completeness values should be 0-100
  numeric_cols <- setdiff(names(mat), c("centre_code", "centre_name"))
  for (col in numeric_cols) {
    vals <- mat[[col]]
    expect_true(all(vals >= 0 & vals <= 100, na.rm = TRUE))
  }
})

test_that("flagging centres with low completeness works", {
  df <- fixture_audit_data_minimal()
  # Make BEA have lots of missing data for one variable
  df$qblg9[1:3] <- NA  # All BEA records missing URR

  md <- compute_missing_data(df, variables = "qblg9")
  flagged <- md |>
    dplyr::filter(pct_missing > 30) |>
    dplyr::distinct(centre_code)

  expect_true("BEA" %in% flagged$centre_code)
})

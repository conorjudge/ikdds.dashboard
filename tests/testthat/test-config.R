# Tests for dashboard configuration

test_that("dashboard_config returns expected structure", {
  config <- dashboard_config()
  expect_s3_class(config, "dashboard_config")
  expect_true(all(c("data_source", "redcap_uri", "redcap_token",
                     "cache_ttl", "app_title", "debug") %in% names(config)))
})

test_that("dashboard_config defaults to synthetic data source", {
  config <- dashboard_config()
  expect_equal(config$data_source, "synthetic")
})

test_that("dashboard_config reads from env list", {
  env <- list(
    IKDDS_DASH_DATA_SOURCE = "redcap",
    IKDDS_REDCAP_URI = "https://redcap.example.com/api/",
    IKDDS_REDCAP_TOKEN = "abc123",
    IKDDS_DASH_CACHE_TTL = "120"
  )
  config <- dashboard_config(env = env)
  expect_equal(config$data_source, "redcap")
  expect_equal(config$redcap_uri, "https://redcap.example.com/api/")
  expect_equal(config$cache_ttl, 120L)
})

test_that("dashboard_config normalises data_source to lowercase", {
  env <- list(IKDDS_DASH_DATA_SOURCE = "Synthetic")
  config <- dashboard_config(env = env)
  expect_equal(config$data_source, "synthetic")
})

test_that("dashboard_config parses debug flag", {
  env <- list(IKDDS_DASH_DEBUG = "true")
  config <- dashboard_config(env = env)
  expect_true(config$debug)

  env2 <- list(IKDDS_DASH_DEBUG = "false")
  config2 <- dashboard_config(env = env2)
  expect_false(config2$debug)
})

test_that("validate_dashboard_config passes for synthetic config", {
  config <- mock_dashboard_config("synthetic")
  expect_invisible(validate_dashboard_config(config))
})

test_that("validate_dashboard_config passes for complete redcap config", {
  config <- mock_dashboard_config("redcap")
  expect_invisible(validate_dashboard_config(config))
})

test_that("validate_dashboard_config errors for invalid data_source", {
  config <- mock_dashboard_config("synthetic")
  config$data_source <- "invalid"
  expect_error(validate_dashboard_config(config), "Invalid data source")
})

test_that("validate_dashboard_config errors for missing redcap credentials", {
  config <- mock_dashboard_config_incomplete()
  expect_error(validate_dashboard_config(config), "REDCap data source requires")
})

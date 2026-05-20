# Tests for access control

test_that("get_user_role returns admin by default", {
  config <- dashboard_config()
  expect_equal(get_user_role(config), "admin")
})

test_that("get_user_role reads from config", {
  env <- list(IKDDS_DASH_USER_ROLE = "clinical")
  config <- dashboard_config(env = env)
  expect_equal(get_user_role(config), "clinical")
})

test_that("get_user_centre returns NULL when not set", {
  config <- dashboard_config()
  expect_null(get_user_centre(config))
})

test_that("get_user_centre returns centre when set", {
  env <- list(IKDDS_DASH_USER_CENTRE = "BEA")
  config <- dashboard_config(env = env)
  expect_equal(get_user_centre(config), "BEA")
})

test_that("filter_by_role does not filter for admin", {
  df <- fixture_audit_data_minimal()
  result <- filter_by_role(df, role = "admin")
  expect_equal(nrow(result), nrow(df))
})

test_that("filter_by_role filters for clinical user", {
  df <- fixture_audit_data_minimal()
  result <- filter_by_role(df, role = "clinical", user_centre = "BEA")
  expect_true(all(result$centre_code == "BEA"))
  expect_equal(nrow(result), 3)
})

test_that("filter_by_role does not filter research role", {
  df <- fixture_audit_data_minimal()
  result <- filter_by_role(df, role = "research")
  expect_equal(nrow(result), nrow(df))
})

test_that("role_display_label returns correct labels", {
  expect_equal(role_display_label("admin"), "Admin (National)")
  expect_match(role_display_label("clinical", "BEA"), "Clinical")
  expect_match(role_display_label("clinical", "BEA"), "BEA")
  expect_equal(role_display_label("research"), "Research")
})

test_that("traffic_light_theme returns correct themes", {
  expect_equal(traffic_light_theme(95), "success")
  expect_equal(traffic_light_theme(80), "warning")
  expect_equal(traffic_light_theme(60), "danger")
  expect_equal(traffic_light_theme(NA), "secondary")
})

test_that("traffic_light_colour returns correct hex codes", {
  expect_equal(traffic_light_colour(95), "#28A745")
  expect_equal(traffic_light_colour(80), "#F58220")
  expect_equal(traffic_light_colour(60), "#DC3545")
  expect_equal(traffic_light_colour(NA_real_), "#6C757D")
})

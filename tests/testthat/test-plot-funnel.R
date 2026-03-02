# Tests for funnel plots

test_that("plot_funnel returns ggplot", {
  df <- fixture_audit_data_minimal()
  metric <- compute_phosphate_achievement(df)
  p <- plot_funnel(metric)

  expect_s3_class(p, "ggplot")
})

test_that("plot_funnel includes ribbon layers for limits", {
  df <- fixture_audit_data_minimal()
  metric <- compute_urr_achievement(df)
  p <- plot_funnel(metric)

  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomRibbon" %in% layer_classes)
})

test_that("plot_funnel handles single centre", {
  df <- fixture_audit_data_minimal() %>%
    dplyr::filter(.data$centre_code == "BEA")
  metric <- compute_urr_achievement(df)
  p <- plot_funnel(metric)

  expect_s3_class(p, "ggplot")
})

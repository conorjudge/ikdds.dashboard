# Tests for caterpillar plots

test_that("plot_caterpillar_proportion returns ggplot", {
  df <- fixture_audit_data_minimal()
  metric <- compute_urr_achievement(df)
  p <- plot_caterpillar_proportion(metric)

  expect_s3_class(p, "ggplot")
})

test_that("plot_caterpillar_proportion handles target line", {
  df <- fixture_audit_data_minimal()
  metric <- compute_urr_achievement(df)
  p <- plot_caterpillar_proportion(metric, target = 0.65)

  expect_s3_class(p, "ggplot")
  # Should have a geom_hline layer for the target
  layer_classes <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_true("GeomHline" %in% layer_classes)
})

test_that("plot_caterpillar_median returns ggplot", {
  df <- fixture_audit_data_minimal()
  metric <- compute_urr_median(df)
  p <- plot_caterpillar_median(metric)

  expect_s3_class(p, "ggplot")
})

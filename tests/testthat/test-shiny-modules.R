# Tests for Shiny modules using testServer()

test_that("mod_filters_server returns filtered data", {
  test_data <- fixture_audit_data_minimal()

  shiny::testServer(
    mod_filters_server,
    args = list(audit_data = shiny::reactive(test_data)),
    {
      # With no filters, should return all data
      result <- filtered_data()
      expect_equal(nrow(result), nrow(test_data))

      # Set centre filter
      session$setInputs(centre = "BEA")
      result <- filtered_data()
      expect_true(all(result$centre_code == "BEA"))
      expect_equal(nrow(result), 3)
    }
  )
})

test_that("mod_filters_server active_filters reflects input", {
  test_data <- fixture_audit_data_minimal()

  shiny::testServer(
    mod_filters_server,
    args = list(audit_data = shiny::reactive(test_data)),
    {
      session$setInputs(centre = c("BEA", "MAT"))
      filters <- active_filters()
      expect_equal(filters$centre_code, c("BEA", "MAT"))
    }
  )
})

test_that("mod_overview_server renders patient count", {
  test_data <- fixture_audit_data_minimal()

  shiny::testServer(
    mod_overview_server,
    args = list(filtered_data = shiny::reactive(test_data)),
    {
      expect_equal(output$n_patients, "6")
      expect_equal(output$n_centres, "2")
    }
  )
})

test_that("mod_drilldown_server filters by centre", {
  test_data <- fixture_audit_data_minimal()

  shiny::testServer(
    mod_drilldown_server,
    args = list(filtered_data = shiny::reactive(test_data)),
    {
      session$setInputs(drilldown_centre = "BEA")
      result <- drilldown_data()
      expect_equal(nrow(result), 3)
    }
  )
})

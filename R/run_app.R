#' Launch the IKDDS Dashboard
#'
#' Starts the Shiny application. Configuration is read from environment
#' variables via [dashboard_config()].
#'
#' @param config Optional `dashboard_config` object. If `NULL`,
#'   reads from environment variables.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object (invisibly).
#'
#' @export
#' @family app
#'
#' @examples
#' \dontrun{
#' # Launch with synthetic data (default)
#' run_app()
#'
#' # Launch with REDCap data
#' Sys.setenv(IKDDS_DASH_DATA_SOURCE = "redcap")
#' run_app()
#' }
run_app <- function(config = NULL, ...) {
  if (is.null(config)) {
    config <- dashboard_config()
  }
  validate_dashboard_config(config)

  cli::cli_inform(c(
    "i" = "Starting IKDDS Dashboard",
    "i" = "Data source: {.val {config$data_source}}",
    "i" = "Title: {.val {config$app_title}}"
  ))

  app <- shiny::shinyApp(
    ui = app_ui(config),
    server = app_server(config),
    options = list(launch.browser = TRUE),
    ...
  )

  shiny::runApp(
    app,
    host = "0.0.0.0",
    port = as.integer(Sys.getenv("PORT", "3838"))
  )
}

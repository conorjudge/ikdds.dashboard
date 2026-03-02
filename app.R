# Posit Connect / shinyapps.io entrypoint
# This file enables deployment to hosting platforms.

library(ikdds.dashboard)

config <- dashboard_config()
validate_dashboard_config(config)

shiny::shinyApp(
  ui = app_ui(config),
  server = app_server(config)
)

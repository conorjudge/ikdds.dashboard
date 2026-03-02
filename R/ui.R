#' Application UI
#'
#' Constructs the full dashboard UI using [bslib::page_navbar()].
#'
#' @param config A `dashboard_config` object.
#'
#' @return A Shiny UI definition.
#'
#' @export
#' @family app
app_ui <- function(config = dashboard_config()) {
  bslib::page_navbar(
    title = shiny::span(
      shiny::tags$img(
        src = "hse-logo.png", height = "30px",
        style = "margin-right: 10px;",
        onerror = "this.style.display='none'"
      ),
      config$app_title
    ),
    id = "main_nav",
    theme = hse_theme(),
    header = shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", href = "custom.css")
    ),
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 250,
      mod_filters_ui("filters")
    ),

    bslib::nav_panel(
      title = "Overview",
      icon = shiny::icon("chart-line"),
      mod_overview_ui("overview")
    ),
    bslib::nav_panel(
      title = "Demographics",
      icon = shiny::icon("users"),
      mod_demographics_ui("demographics")
    ),
    bslib::nav_panel(
      title = "PRD",
      icon = shiny::icon("stethoscope"),
      mod_prd_ui("prd")
    ),
    bslib::nav_panel(
      title = "Dialysis",
      icon = shiny::icon("droplet"),
      mod_dialysis_ui("dialysis")
    ),
    bslib::nav_panel(
      title = "Blood Pressure",
      icon = shiny::icon("heart-pulse"),
      mod_bp_ui("bp")
    ),
    bslib::nav_panel(
      title = "Biochemistry",
      icon = shiny::icon("flask"),
      mod_biochemistry_ui("biochemistry")
    ),
    bslib::nav_panel(
      title = "Bicarb / K+",
      icon = shiny::icon("vials"),
      mod_bicarbonate_ui("bicarbonate")
    ),
    bslib::nav_panel(
      title = "Anaemia",
      icon = shiny::icon("vial"),
      mod_anaemia_ui("anaemia")
    ),
    bslib::nav_panel(
      title = "Access",
      icon = shiny::icon("syringe"),
      mod_access_ui("access")
    ),
    bslib::nav_panel(
      title = "Drill-down",
      icon = shiny::icon("table"),
      mod_drilldown_ui("drilldown")
    ),

    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::tags$span(
        class = "navbar-text text-light small",
        paste0("v", utils::packageVersion("ikdds.dashboard"))
      )
    )
  )
}

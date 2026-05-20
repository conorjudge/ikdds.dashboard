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
      config$app_title,
      shiny::uiOutput("role_indicator", inline = TRUE)
    ),
    id = "main_nav",
    fillable = FALSE,
    theme = hse_theme(),
    header = shiny::tagList(
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", href = "custom.css")
      ),
      shiny::div(
        class = "header-subtitle",
        shiny::textOutput("filter_state", inline = TRUE),
        shiny::uiOutput("filter_chips", inline = TRUE)
      )
    ),
    sidebar = bslib::sidebar(
      title = "Filters",
      width = 250,
      position = "right",
      open = "desktop",
      mod_filters_ui("filters")
    ),
    footer = shiny::div(
      class = "dashboard-footer",
      shiny::div(
        class = "footer-left",
        shiny::textOutput("last_refreshed", inline = TRUE)
      ),
      shiny::div(
        class = "footer-right",
        "Data Source: IKDDS / HSE"
      )
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
      title = "Data Quality",
      icon = shiny::icon("clipboard-check"),
      mod_data_quality_ui("data_quality")
    ),
    bslib::nav_panel(
      title = "Centre Profile",
      icon = shiny::icon("building"),
      mod_centre_profile_ui("centre_profile")
    ),
    bslib::nav_panel(
      title = "Individual",
      icon = shiny::icon("user"),
      mod_individual_ui("individual")
    ),
    bslib::nav_panel(
      title = "Working Groups",
      icon = shiny::icon("people-group"),
      mod_working_groups_ui("working_groups")
    ),
    bslib::nav_panel(
      title = "Methods",
      icon = shiny::icon("book"),
      mod_methods_ui("methods")
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

#' Page description block
#'
#' Creates a styled "About this page" description element.
#'
#' @param ... Content to display inside the description block.
#'
#' @return A [shiny::div()] with class `page-description`.
#'
#' @keywords internal
page_description <- function(...) {
  shiny::div(class = "page-description", ...)
}

#' Source footnote block
#'
#' Creates a styled footnote referencing clinical guideline sources.
#'
#' @param text Character string of the footnote text.
#'
#' @return A [shiny::div()] with class `source-footnote`.
#'
#' @export
#' @family ui
source_footnote <- function(text) {
  shiny::div(class = "source-footnote", text)
}

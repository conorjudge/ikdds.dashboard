#' Demographics module  - UI
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_demographics_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Age Distribution by Centre"),
        bslib::card_body(
          plotly::plotlyOutput(ns("age_plot"), height = "400px")
        )
      ),
      bslib::card(
        bslib::card_header("Gender Breakdown by Centre"),
        bslib::card_body(
          plotly::plotlyOutput(ns("gender_plot"), height = "400px")
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(
        bslib::card_header("Age Summary Statistics"),
        bslib::card_body(DT::DTOutput(ns("age_table")))
      ),
      bslib::card(
        bslib::card_header("Patient Counts"),
        bslib::card_body(DT::DTOutput(ns("counts_table")))
      )
    )
  )
}

#' Demographics module  - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble.
#'
#' @export
#' @family modules
mod_demographics_server <- function(id, filtered_data) {
  shiny::moduleServer(id, function(input, output, session) {

    age_data <- shiny::reactive({
      compute_age_distribution(filtered_data())
    })

    output$age_plot <- plotly::renderPlotly({
      bins <- age_data()$bins
      p <- plot_bar(bins, .data$centre_code, .data$pct, .data$age_band,
                    title = "Age Distribution", y_label = "Percentage (%)")
      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })

    output$gender_plot <- plotly::renderPlotly({
      gender <- compute_gender_breakdown(filtered_data())
      p <- plot_bar(gender, .data$centre_code, .data$pct, .data$gender,
                    title = "Gender Breakdown", y_label = "Percentage (%)")
      plotly::ggplotly(p) %>%
        plotly::layout(legend = list(orientation = "h", y = -0.2))
    })

    output$age_table <- DT::renderDT({
      age_data()$summary %>%
        hse_datatable(caption = "Age statistics by centre")
    })

    output$counts_table <- DT::renderDT({
      compute_patient_counts(filtered_data()) %>%
        hse_datatable(caption = "Patient counts")
    })
  })
}

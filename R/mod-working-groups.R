#' Working Groups module - UI
#'
#' Displays clinical working group assignments, review status,
#' and metric group responsibilities.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_working_groups_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "Clinical working groups are responsible for reviewing and agreeing ",
      "metric definitions, target ranges, and summary statistics annually. ",
      "Each group consists of 2 consultant nephrologists and 1 pharmacist."
    ),

    bslib::card(
      bslib::card_header("Working Group Assignments"),
      bslib::card_body(DT::DTOutput(ns("groups_table")))
    ),

    bslib::card(
      bslib::card_header("Review Schedule"),
      bslib::card_body(
        shiny::HTML(
          paste0(
            "<p>Working groups meet annually to:</p>",
            "<ul>",
            "<li>Review metric definitions and clinical relevance</li>",
            "<li>Agree summary statistics (median vs mean) for each metric</li>",
            "<li>Update target ranges based on latest guidelines (KDIGO, KDOQI, ERA-EDTA)</li>",
            "<li>Review data quality thresholds and suppression policies</li>",
            "<li>Recommend new metrics or retire outdated ones</li>",
            "</ul>",
            "<p>Next review cycle: <strong>Q1 2027</strong></p>"
          )
        )
      )
    ),

    bslib::card(
      bslib::card_header("Metric Group Standards"),
      bslib::card_body(DT::DTOutput(ns("standards_table")))
    ),

    source_footnote(
      paste0("Working group structure agreed at clinical feedback session, 5 March 2026. ",
             "Review cycle: annual. Contact NRO for membership changes.")
    )
  )
}

#' Working Groups module - Server
#'
#' @param id Module namespace ID.
#'
#' @export
#' @family modules
mod_working_groups_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    output$groups_table <- DT::renderDT({
      groups <- tibble::tibble(
        `Metric Domain` = c(
          "Dialysis Adequacy",
          "Blood Pressure",
          "CKD-MBD (Biochemistry)",
          "Anaemia Management",
          "Vascular Access",
          "Data Quality"
        ),
        `Lead Consultant 1` = c(
          "TBC", "TBC", "TBC", "TBC", "TBC", "TBC"
        ),
        `Lead Consultant 2` = c(
          "TBC", "TBC", "TBC", "TBC", "TBC", "TBC"
        ),
        Pharmacist = c(
          "TBC", "TBC", "TBC", "TBC", "TBC", "TBC"
        ),
        Status = c(
          "Due for Review", "Due for Review", "Due for Review",
          "Due for Review", "Due for Review", "Due for Review"
        ),
        `Last Reviewed` = c(
          "N/A", "N/A", "N/A", "N/A", "N/A", "N/A"
        ),
        `Next Review` = rep("Q1 2027", 6)
      )

      dt <- hse_datatable(groups, caption = "Working group membership and status")
      DT::formatStyle(
        dt, "Status",
        backgroundColor = DT::styleEqual(
          c("Reviewed", "Under Review", "Due for Review"),
          c("#CCFFCC", "#FFFFCC", "#FFCCCC")
        )
      )
    })

    output$standards_table <- DT::renderDT({
      standards <- tibble::tibble(
        `Metric Domain` = c(
          "Dialysis Adequacy", "Dialysis Adequacy",
          "Blood Pressure", "Blood Pressure",
          "CKD-MBD", "CKD-MBD", "CKD-MBD", "CKD-MBD",
          "Anaemia", "Anaemia",
          "Vascular Access"
        ),
        Metric = c(
          "URR >65%", "URR Median",
          "Pre-HD BP <140/90", "Post-HD BP <130/80",
          "PO4 1.1-1.7", "Ca 2.2-2.5", "PTH 16-72", "Ca x PO4 <4.4",
          "Hb 10-12", "Ferritin >=200",
          "AVF Rate"
        ),
        `Summary Stat` = c(
          "Proportion", "Median",
          "Proportion", "Proportion",
          "Proportion", "Proportion", "Proportion", "Proportion",
          "Proportion", "Proportion",
          "Proportion"
        ),
        Source = c(
          "KDOQI", "Clinical",
          "KDIGO 2024", "KDIGO 2024",
          "KDIGO", "KDIGO", "KDIGO", "ERA-EDTA",
          "KDIGO/NICE", "Clinical",
          "ERA-EDTA"
        ),
        `Agreed` = rep("Pending", 11)
      )

      hse_datatable(standards, caption = "Metric standards and sources")
    })
  })
}

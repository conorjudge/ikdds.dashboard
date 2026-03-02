#' Code viewer module  - UI
#'
#' Button that opens a modal showing reproducible R code.
#'
#' @param id Module namespace ID.
#' @param label Button label (default "View R Code").
#'
#' @return A [shiny::actionButton()].
#'
#' @export
#' @family modules
mod_code_viewer_ui <- function(id, label = "View R Code") {
  ns <- shiny::NS(id)

  shiny::actionButton(
    ns("show_code"),
    label = label,
    icon = shiny::icon("code"),
    class = "btn-outline-primary btn-sm"
  )
}

#' Code viewer module  - Server
#'
#' @param id Module namespace ID.
#' @param code_reactive A reactive that returns an R code string.
#'
#' @export
#' @family modules
mod_code_viewer_server <- function(id, code_reactive) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$show_code, {
      code <- code_reactive()

      shiny::showModal(
        shiny::modalDialog(
          title = "Reproducible R Code",
          size = "l",
          easyClose = TRUE,
          shiny::div(
            class = "code-viewer",
            shiny::tags$pre(
              shiny::tags$code(code)
            )
          ),
          shiny::tags$p(
            class = "text-muted mt-2",
            "Copy this code to reproduce this visualisation in R."
          ),
          footer = shiny::tagList(
            shiny::actionButton(
              session$ns("copy_code"), "Copy to Clipboard",
              class = "btn-primary",
              onclick = sprintf(
                "navigator.clipboard.writeText(document.querySelector('#%s .code-viewer code').textContent)",
                session$ns("show_code")
              )
            ),
            shiny::modalButton("Close")
          )
        )
      )
    })
  })
}

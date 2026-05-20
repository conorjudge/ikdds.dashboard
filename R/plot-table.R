#' HSE-styled DT table
#'
#' Wraps [DT::datatable()] with consistent HSE styling and formatting.
#'
#' @param df Data frame to display.
#' @param caption Table caption.
#' @param pageLength Rows per page (default 15).
#' @param scrollX Enable horizontal scrolling (default TRUE).
#'
#' @return A DT::datatable object.
#'
#' @export
#' @family plots
hse_datatable <- function(df, caption = NULL, pageLength = 15,
                           scrollX = TRUE) {
  dt <- DT::datatable(
    df,
    caption = caption,
    rownames = FALSE,
    class = "compact stripe hover",
    extensions = "Buttons",
    options = list(
      pageLength = pageLength,
      scrollX = scrollX,
      dom = "Bfrtip",
      buttons = list(
        list(extend = "csv", text = "Download CSV"),
        list(extend = "excel", text = "Download Excel")
      ),
      language = list(
        search = "Filter:",
        lengthMenu = "Show _MENU_ rows"
      )
    )
  )

  # Add conditional formatting for suppressed rows
  if ("suppressed" %in% names(df)) {
    dt <- DT::formatStyle(
      dt, "suppressed",
      target = "row",
      color = DT::styleEqual(c(TRUE, FALSE), c("#999999", "inherit")),
      fontStyle = DT::styleEqual(c(TRUE, FALSE), c("italic", "normal"))
    )
  }

  dt
}

#' Format percentage columns in DT
#'
#' Applies conditional colour formatting to percentage columns.
#'
#' @param dt A DT::datatable object.
#' @param columns Character vector of column names to format.
#' @param target_lower Lower target (for green highlighting).
#' @param target_upper Upper target.
#'
#' @return A DT::datatable object with formatting applied.
#'
#' @export
#' @family plots
format_pct_column <- function(dt, columns, target_lower = NULL,
                                target_upper = NULL) {
  for (col in columns) {
    dt <- DT::formatStyle(
      dt, col,
      background = DT::styleInterval(
        c(50, 75),
        c("#FFCCCC", "#FFFFCC", "#CCFFCC")
      )
    )
  }
  dt
}

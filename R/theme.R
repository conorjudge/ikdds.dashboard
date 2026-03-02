#' HSE bslib theme
#'
#' Creates a Bootstrap 5 theme styled with HSE brand colours.
#'
#' @return A [bslib::bs_theme()] object.
#'
#' @export
#' @family theme
hse_theme <- function() {
  bslib::bs_theme(
    version   = 5,
    bootswatch = "flatly",
    primary   = "#00617F",
    secondary = "#6C757D",
    success   = "#28A745",
    info      = "#17A2B8",
    warning   = "#FFC107",
    danger    = "#DC3545",
    bg        = "#FFFFFF",
    fg        = "#212529",
    base_font = bslib::font_google("Open Sans"),
    heading_font = bslib::font_google("Open Sans"),
    font_scale = 0.95
  )
}

#' HSE colour palette
#'
#' Named vector of HSE brand colours for use in ggplot2.
#'
#' @return Named character vector of hex colours.
#'
#' @export
#' @family theme
hse_colours <- function() {
  c(
    teal       = "#00617F",
    dark_teal  = "#004B5F",
    light_teal = "#4DA8C4",
    green      = "#28A745",
    orange     = "#F58220",
    red        = "#DC3545",
    grey       = "#6C757D",
    light_grey = "#E9ECEF",
    dark_grey  = "#343A40",
    blue       = "#17A2B8",
    purple     = "#6F42C1"
  )
}

#' HSE ggplot2 theme
#'
#' Consistent ggplot2 theme matching the HSE dashboard style.
#'
#' @param base_size Base font size (default 12).
#'
#' @return A [ggplot2::theme()] object.
#'
#' @export
#' @family theme
hse_ggplot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        colour = "#00617F", face = "bold", size = base_size * 1.2
      ),
      plot.subtitle = ggplot2::element_text(
        colour = "#6C757D", size = base_size * 0.9
      ),
      axis.title = ggplot2::element_text(colour = "#343A40"),
      axis.text = ggplot2::element_text(colour = "#6C757D"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E9ECEF"),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold", size = base_size * 0.85),
      strip.text = ggplot2::element_text(face = "bold", colour = "#00617F")
    )
}

#' Scale colour using HSE palette
#'
#' @param ... Passed to [ggplot2::scale_colour_manual()].
#'
#' @return A ggplot2 scale.
#'
#' @export
#' @family theme
scale_colour_hse <- function(...) {
  ggplot2::scale_colour_manual(values = unname(hse_colours()), ...)
}

#' Scale fill using HSE palette
#'
#' @param ... Passed to [ggplot2::scale_fill_manual()].
#'
#' @return A ggplot2 scale.
#'
#' @export
#' @family theme
scale_fill_hse <- function(...) {
  ggplot2::scale_fill_manual(values = unname(hse_colours()), ...)
}

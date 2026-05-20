#' Generate dashboard specification as Excel workbook
#'
#' Creates a multi-sheet Excel workbook documenting every view, card, variable,
#' metric formula, filter, colour, and statistical method in the dashboard.
#' Intended for handoff to IIS for Power BI recreation.
#'
#' @param registry A dashboard registry list from [dashboard_registry()].
#' @param data A tibble of audit data (used for example counts/values).
#' @param output_path File path for the output .xlsx file.
#'
#' @return Invisibly returns the output path.
#'
#' @export
#' @family spec
generate_spec_excel <- function(registry, data, output_path = "ikdds_spec.xlsx") {
  if (!requireNamespace("openxlsx2", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg openxlsx2} is required. Install with: install.packages('openxlsx2')")
  }

  wb <- openxlsx2::wb_workbook()

  # ── Sheet 1: Views ──────────────────────────────────────────────────────
  views_df <- dplyr::bind_rows(lapply(registry, function(v) {
    tibble::tibble_row(
      view_id     = v$view_id,
      title       = v$view_title,
      description = v$description,
      filters     = paste(v$filters, collapse = ", "),
      n_cards     = length(v$cards)
    )
  }))

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Views") |>
    openxlsx2::wb_add_data(sheet = "Views", x = views_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Views", cols = 1:5,
                                  widths = c(15, 20, 60, 25, 10))

  # ── Sheet 2: Cards ─────────────────────────────────────────────────────
  cards_df <- registry_cards(registry)

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Cards") |>
    openxlsx2::wb_add_data(sheet = "Cards", x = cards_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Cards", cols = 1:8,
                                  widths = c(15, 25, 12, 22, 40, 30, 30, 50))

  # ── Sheet 3: Variables ─────────────────────────────────────────────────
  fields_df <- registry_fields(registry) |>
    dplyr::select(
      view_id, card_id,
      redcap_field = "redcap",
      clinical_label = "label",
      emed_table, emed_column = "emed_col",
      unit, target_lower, target_upper
    ) |>
    dplyr::distinct()

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Variables") |>
    openxlsx2::wb_add_data(sheet = "Variables", x = fields_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Variables", cols = 1:9,
                                  widths = c(15, 25, 15, 25, 15, 20, 10, 12, 12))

  # ── Sheet 4: Metric Logic ─────────────────────────────────────────────
  logic_rows <- list()
  for (view in registry) {
    for (card in view$cards) {
      logic_rows <- c(logic_rows, list(tibble::tibble_row(
        view_id    = view$view_id,
        card_id    = card$card_id,
        title      = card$title,
        pseudocode = card$pseudocode %||% NA_character_,
        r_code     = card$r_code %||% NA_character_
      )))
    }
  }
  logic_df <- dplyr::bind_rows(logic_rows)

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Metric Logic") |>
    openxlsx2::wb_add_data(sheet = "Metric Logic", x = logic_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Metric Logic", cols = 1:5,
                                  widths = c(15, 25, 40, 60, 60))

  # ── Sheet 5: Filters ──────────────────────────────────────────────────
  filters_df <- registry_filters(registry)

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Filters") |>
    openxlsx2::wb_add_data(sheet = "Filters", x = filters_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Filters", cols = 1:4,
                                  widths = c(20, 35, 20, 30))

  # ── Sheet 6: Colour Palette ───────────────────────────────────────────
  colours_df <- registry_colours()

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Colour Palette") |>
    openxlsx2::wb_add_data(sheet = "Colour Palette", x = colours_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Colour Palette", cols = 1:3,
                                  widths = c(15, 10, 50))

  # ── Sheet 7: Statistical Methods ──────────────────────────────────────
  methods_df <- registry_stat_methods(registry)

  wb <- wb |>
    openxlsx2::wb_add_worksheet("Statistical Methods") |>
    openxlsx2::wb_add_data(sheet = "Statistical Methods", x = methods_df) |>
    openxlsx2::wb_set_col_widths(sheet = "Statistical Methods", cols = 1:4,
                                  widths = c(30, 60, 40, 60))

  # ── Save ───────────────────────────────────────────────────────────────
  openxlsx2::wb_save(wb, output_path, overwrite = TRUE)
  cli::cli_alert_success("Spec Excel saved to {.file {output_path}}")

  invisible(output_path)
}


#' Generate dashboard specification as HTML document
#'
#' Renders the spec.qmd template with registry and data as parameters,
#' producing a rich HTML specification with embedded example figures.
#'
#' @param registry A dashboard registry list from [dashboard_registry()].
#' @param data A tibble of audit data.
#' @param output_dir Directory for the rendered HTML output.
#'
#' @return Invisibly returns the path to the rendered HTML file.
#'
#' @export
#' @family spec
generate_spec_html <- function(registry, data, output_dir = ".") {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg quarto} is required. Install with: install.packages('quarto')")
  }

  spec_qmd <- system.file("quarto", "spec.qmd",
                           package = "ikdds.dashboard", mustWork = FALSE)

  if (!nzchar(spec_qmd)) {
    # Fallback: look relative to package source
    spec_qmd <- file.path(
      system.file(package = "ikdds.dashboard"),
      "..", "inst", "quarto", "spec.qmd"
    )
    if (!file.exists(spec_qmd)) {
      cli::cli_abort("Cannot find spec.qmd template. Is the package installed?")
    }
  }

  # Save registry and data to temp files for Quarto to pick up
  tmp_dir <- tempdir()
  registry_path <- file.path(tmp_dir, "spec_registry.rds")
  data_path <- file.path(tmp_dir, "spec_data.rds")
  saveRDS(registry, registry_path)
  saveRDS(data, data_path)

  output_dir <- normalizePath(output_dir, mustWork = FALSE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  quarto::quarto_render(
    input = spec_qmd,
    output_file = "ikdds_spec.html",
    execute_params = list(
      registry_path = registry_path,
      data_path = data_path
    )
  )

  # Move output to desired location
  rendered <- sub("\\.qmd$", ".html", spec_qmd)
  output_file <- file.path(output_dir, "ikdds_spec.html")

  if (file.exists(rendered) && normalizePath(rendered) != normalizePath(output_file)) {
    file.copy(rendered, output_file, overwrite = TRUE)
  }

  cli::cli_alert_success("Spec HTML saved to {.file {output_file}}")
  invisible(output_file)
}

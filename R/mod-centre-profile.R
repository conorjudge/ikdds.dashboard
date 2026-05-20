#' Centre Profile module - UI
#'
#' Single-centre deep-dive with "vs national" comparisons.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_centre_profile_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "Select a single centre using the filter panel to see a detailed profile ",
      "comparing that centre's performance against national averages."
    ),
    shiny::uiOutput(ns("profile_content"))
  )
}

#' Centre Profile module - Server
#'
#' @param id Module namespace ID.
#' @param filtered_data Reactive tibble of filtered data.
#' @param audit_data Reactive tibble of unfiltered audit data (for national reference).
#' @param active_filters Reactive list of active filter values.
#'
#' @export
#' @family modules
mod_centre_profile_server <- function(id, filtered_data, audit_data,
                                       active_filters) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Determine selected centre
    selected_centre <- shiny::reactive({
      af <- active_filters()
      centres <- af$centre_code
      if (length(centres) == 1) centres else NULL
    })

    output$profile_content <- shiny::renderUI({
      centre <- selected_centre()

      if (is.null(centre)) {
        return(
          bslib::card(
            bslib::card_body(
              shiny::div(
                style = "text-align: center; padding: 3rem;",
                shiny::icon("building", style = "font-size: 3rem; color: #ccc;"),
                shiny::h4("Select a Single Centre", style = "color: #666;"),
                shiny::p(
                  class = "text-muted",
                  "Use the filter panel to select exactly one centre ",
                  "to view its detailed profile."
                )
              )
            )
          )
        )
      }

      # Centre is selected — render full profile
      shiny::tagList(
        # KPI row
        bslib::layout_columns(
          col_widths = c(4, 4, 4),
          bslib::value_box(
            title = "Patients",
            value = shiny::textOutput(ns("centre_patients")),
            showcase = shiny::icon("users"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Consultants",
            value = shiny::textOutput(ns("centre_consultants")),
            showcase = shiny::icon("user-doctor"),
            theme = "primary"
          ),
          bslib::value_box(
            title = "Data Completeness",
            value = shiny::textOutput(ns("centre_completeness")),
            showcase = shiny::icon("chart-pie"),
            theme = "info"
          )
        ),

        # Metric comparison cards by domain
        shiny::h4(paste0("Centre: ", centre, " vs National")),

        bslib::card(
          bslib::card_header("Key Metrics Comparison"),
          bslib::card_body(DT::DTOutput(ns("metrics_comparison")))
        ),

        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Centre Performance Bullet Chart"),
          bslib::card_body(
            plotly::plotlyOutput(ns("bullet_chart"), height = "500px")
          )
        ),

        bslib::card(
          bslib::card_header("Within-Centre Consultant Variation"),
          bslib::card_body(DT::DTOutput(ns("consultant_table")))
        ),

        # Temporal trends
        bslib::card(
          bslib::card_header("Centre Performance Over Time"),
          bslib::card_body(
            shiny::uiOutput(ns("centre_trends"))
          )
        ),

        # Download stub
        shiny::div(
          style = "margin-top: 1rem;",
          shiny::downloadButton(ns("download_pack"), "Download Audit Pack",
                                 class = "btn-outline-primary")
        ),
        source_footnote("Centre profile shows own performance vs national average. No other centre's individual results are displayed.")
      )
    })

    # --- KPI outputs ---

    output$centre_patients <- shiny::renderText({
      format(nrow(filtered_data()), big.mark = ",")
    })

    output$centre_consultants <- shiny::renderText({
      cons <- filtered_data()$consultant
      as.character(length(unique(cons[!is.na(cons)])))
    })

    output$centre_completeness <- shiny::renderText({
      df <- filtered_data()
      vars <- c("qblg9", "qblg3", "qblg4", "qblg6", "qblg7",
                 "qblb1", "qblb4", "qblb9", "qbla9", "qbla4",
                 "qble1", "qblf1", "qhd20")
      vars <- intersect(vars, names(df))
      if (length(vars) == 0 || nrow(df) == 0) return("N/A")
      total_cells <- nrow(df) * length(vars)
      missing_cells <- sum(sapply(vars, function(v) sum(is.na(df[[v]]))))
      pct <- round((1 - missing_cells / total_cells) * 100, 1)
      paste0(pct, "%")
    })

    # --- Metrics comparison table ---

    output$metrics_comparison <- DT::renderDT({
      centre <- selected_centre()
      if (is.null(centre)) return(NULL)

      df_centre <- filtered_data()
      df_national <- audit_data()

      # Define key proportion metrics for comparison
      metrics_info <- list(
        list(id = "urr_achievement", label = "URR >65%"),
        list(id = "pre_bp_achievement", label = "Pre-HD BP <140/90"),
        list(id = "post_bp_achievement", label = "Post-HD BP <130/80"),
        list(id = "phosphate_achievement", label = "PO4 1.1-1.7"),
        list(id = "calcium_achievement", label = "Ca 2.2-2.5"),
        list(id = "pth_achievement", label = "PTH 16-72"),
        list(id = "potassium_achievement", label = "K+ 4-6"),
        list(id = "bicarbonate_achievement", label = "HCO3 18-26"),
        list(id = "hb_achievement", label = "Hb 10-12"),
        list(id = "ferritin_achievement", label = "Ferritin >=200"),
        list(id = "avf_rate", label = "AVF Rate")
      )

      rows <- lapply(metrics_info, function(m) {
        # Centre value
        centre_metric <- tryCatch({
          fn <- match.fun(get_metric_config(m$id)$compute_fn)
          result <- fn(df_centre)
          if ("proportion" %in% names(result) && nrow(result) > 0) {
            round(national_average_proportion(result) * 100, 1)
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)

        # National value
        national_metric <- tryCatch({
          fn <- match.fun(get_metric_config(m$id)$compute_fn)
          result <- fn(df_national)
          if ("proportion" %in% names(result) && nrow(result) > 0) {
            round(national_average_proportion(result) * 100, 1)
          } else {
            NA_real_
          }
        }, error = function(e) NA_real_)

        diff_val <- if (!is.na(centre_metric) && !is.na(national_metric)) {
          round(centre_metric - national_metric, 1)
        } else {
          NA_real_
        }

        tibble::tibble(
          Metric = m$label,
          `Centre (%)` = centre_metric,
          `National (%)` = national_metric,
          `Difference (pp)` = diff_val
        )
      })

      comparison_df <- dplyr::bind_rows(rows)

      dt <- hse_datatable(comparison_df,
                            caption = paste("Performance comparison:", centre))

      # Colour the difference column
      if ("Difference (pp)" %in% names(comparison_df)) {
        dt <- DT::formatStyle(
          dt, "Difference (pp)",
          color = DT::styleInterval(
            c(-5, 5),
            c("#DC3545", "#212529", "#28A745")
          )
        )
      }

      dt
    })

    # --- Bullet chart ---

    output$bullet_chart <- plotly::renderPlotly({
      centre <- selected_centre()
      if (is.null(centre)) return(NULL)

      df_centre <- filtered_data()
      df_national <- audit_data()

      metrics_info <- list(
        list(id = "urr_achievement", label = "URR >65%"),
        list(id = "pre_bp_achievement", label = "Pre-BP <140/90"),
        list(id = "phosphate_achievement", label = "PO4 1.1-1.7"),
        list(id = "calcium_achievement", label = "Ca 2.2-2.5"),
        list(id = "pth_achievement", label = "PTH 16-72"),
        list(id = "hb_achievement", label = "Hb 10-12"),
        list(id = "ferritin_achievement", label = "Ferritin >=200"),
        list(id = "avf_rate", label = "AVF Rate")
      )

      chart_data <- purrr::map_dfr(metrics_info, function(m) {
        centre_val <- tryCatch({
          fn <- match.fun(get_metric_config(m$id)$compute_fn)
          r <- fn(df_centre)
          if ("proportion" %in% names(r) && nrow(r) > 0) {
            national_average_proportion(r) * 100
          } else NA_real_
        }, error = function(e) NA_real_)

        national_val <- tryCatch({
          fn <- match.fun(get_metric_config(m$id)$compute_fn)
          r <- fn(df_national)
          if ("proportion" %in% names(r) && nrow(r) > 0) {
            national_average_proportion(r) * 100
          } else NA_real_
        }, error = function(e) NA_real_)

        tibble::tibble(
          metric = m$label,
          centre = centre_val,
          national = national_val
        )
      })

      chart_data <- chart_data |>
        dplyr::filter(!is.na(.data$centre) & !is.na(.data$national)) |>
        dplyr::mutate(metric = factor(.data$metric, levels = rev(.data$metric)))

      if (nrow(chart_data) == 0) {
        return(plotly::ggplotly(
          ggplot2::ggplot() +
            ggplot2::labs(title = "Insufficient data for comparison")
        ))
      }

      p <- ggplot2::ggplot(chart_data) +
        # National reference bar (background)
        ggplot2::geom_col(
          ggplot2::aes(x = .data$metric, y = .data$national),
          fill = "#E9ECEF", width = 0.6
        ) +
        # Centre value bar (foreground, narrower)
        ggplot2::geom_col(
          ggplot2::aes(x = .data$metric, y = .data$centre),
          fill = hse_colours()[["teal"]], width = 0.3
        ) +
        # National marker line
        ggplot2::geom_point(
          ggplot2::aes(x = .data$metric, y = .data$national),
          shape = "|", size = 4, colour = "#DC3545"
        ) +
        ggplot2::coord_flip() +
        ggplot2::labs(
          title = paste(centre, "vs National"),
          x = NULL,
          y = "Achievement (%)"
        ) +
        ggplot2::theme_bw(base_size = 12) +
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 10, r = 15, b = 15, l = 15),
          plot.title = ggplot2::element_text(size = 13, face = "bold")
        )

      plotly::ggplotly(p) |>
        plotly::layout(margin = list(b = 50, t = 50, l = 120, r = 20))
    })

    # --- Consultant variation table ---

    output$consultant_table <- DT::renderDT({
      centre <- selected_centre()
      if (is.null(centre)) return(NULL)

      df <- filtered_data()
      consultants <- unique(df$consultant[!is.na(df$consultant)])

      if (length(consultants) <= 1) {
        return(hse_datatable(
          tibble::tibble(
            Message = "Only one consultant at this centre - no variation to show"
          )
        ))
      }

      # Compute key metrics by consultant
      rows <- lapply(consultants, function(con) {
        con_df <- df |> dplyr::filter(.data$consultant == con)
        n <- nrow(con_df)

        urr_val <- tryCatch({
          urr <- con_df$qblg9[!is.na(con_df$qblg9)]
          if (length(urr) > 0) round(sum(urr > 65) / length(urr) * 100, 1)
          else NA_real_
        }, error = function(e) NA_real_)

        hb_val <- tryCatch({
          hb <- con_df$qble1[!is.na(con_df$qble1)]
          if (length(hb) > 0) round(sum(hb >= 10 & hb <= 12) / length(hb) * 100, 1)
          else NA_real_
        }, error = function(e) NA_real_)

        tibble::tibble(
          Consultant = con,
          Patients = n,
          `URR >65% (%)` = urr_val,
          `Hb 10-12 (%)` = hb_val
        )
      })

      dplyr::bind_rows(rows) |>
        dplyr::arrange(dplyr::desc(.data$Patients)) |>
        hse_datatable(caption = "Consultant-level variation")
    })

    # --- Centre trends ---

    output$centre_trends <- shiny::renderUI({
      df <- filtered_data()
      if ("audit_period" %in% names(df) && length(unique(df$audit_period)) > 1) {
        # Multi-period data available — render trend charts
        plotly::plotlyOutput(ns("centre_trend_plot"), height = "400px")
      } else {
        trend_placeholder(
          "Temporal trends available when multi-period audit data is loaded."
        )
      }
    })

    # --- Download handler (stub) ---

    output$download_pack <- shiny::downloadHandler(
      filename = function() {
        centre <- selected_centre() %||% "national"
        paste0("ikdds_audit_pack_", centre, "_",
               format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        # TODO: Phase 2 — generate Quarto HTML audit pack
        df <- filtered_data()
        readr::write_csv(df, file)
      }
    )
  })
}

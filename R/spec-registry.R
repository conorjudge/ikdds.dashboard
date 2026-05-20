#' Dashboard view registry
#'
#' Single source of truth defining every dashboard view, card, and metric.
#' Drives both the Quarto dashboard layout and the specification document
#' generation for IIS Power BI translation.
#'
#' @return A list of view definitions, each containing cards with metric/plot
#'   function references, variable mappings, targets, and pseudocode.
#'
#' @export
#' @family spec
dashboard_registry <- function() {

  views <- list(

    # ── 1. Overview ──────────────────────────────────────────────────────────
    list(
      view_id     = "overview",
      view_title  = "Overview",
      description = "National summary landing page with KPI value boxes, key metrics summary, and missing data rates.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id   = "ovw_patient_count",
          card_type = "value_box",
          title     = "Total Patients",
          metric_fn = "compute_patient_counts",
          plot_fn   = NULL,
          chart_type = NULL,
          fields = list(
            list(redcap = "record_id", label = "Patient ID",
                 emed_table = "Patient", emed_col = "PatientID",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Count of distinct record IDs",
          r_code       = 'nrow(df)',
          pseudocode   = 'COUNT(DISTINCT PatientID)',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_centre_count",
          card_type = "value_box",
          title     = "Centres",
          metric_fn = NULL,
          plot_fn   = NULL,
          chart_type = NULL,
          fields = list(
            list(redcap = "centre_code", label = "Centre Code",
                 emed_table = "Centre", emed_col = "CentreCode",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Count of distinct centres",
          r_code       = 'length(unique(df$centre_code))',
          pseudocode   = 'COUNT(DISTINCT CentreCode)',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_urr_kpi",
          card_type = "value_box",
          title     = "URR >65%",
          metric_fn = NULL,
          plot_fn   = NULL,
          chart_type = NULL,
          fields = list(
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA)
          ),
          target       = 0.65,
          stat_method  = "Proportion of non-missing URR values >65%",
          r_code       = 'pct_in_range(df$qblg9, 65, Inf)$pct',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(Labs, URR > 65)), COUNTROWS(FILTER(Labs, NOT ISBLANK(URR)))) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_hb_kpi",
          card_type = "value_box",
          title     = "Hb 10-12 g/dL",
          metric_fn = NULL,
          plot_fn   = NULL,
          chart_type = NULL,
          fields = list(
            list(redcap = "qble1", label = "Haemoglobin (g/dL)",
                 emed_table = "LabResults", emed_col = "Haemoglobin",
                 unit = "g/dL", target_lower = 10, target_upper = 12)
          ),
          target       = NULL,
          stat_method  = "Proportion of non-missing Hb values in 10-12 g/dL",
          r_code       = 'pct_in_range(df$qble1, 10, 12)$pct',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(Labs, Hb >= 10 AND Hb <= 12)), COUNTROWS(FILTER(Labs, NOT ISBLANK(Hb)))) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_counts_table",
          card_type = "table",
          title     = "Patient Counts by Centre",
          metric_fn = "compute_patient_counts",
          plot_fn   = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "centre_code", label = "Centre Code",
                 emed_table = "Centre", emed_col = "CentreCode",
                 unit = NA_character_, target_lower = NA, target_upper = NA),
            list(redcap = "centre_name", label = "Centre Name",
                 emed_table = "Centre", emed_col = "CentreName",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "COUNT grouped by centre",
          r_code       = 'compute_patient_counts(df)',
          pseudocode   = 'GROUP BY CentreCode; COUNT(PatientID) AS n_patients',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_metrics_summary",
          card_type = "table",
          title     = "National Key Metrics Summary",
          metric_fn = NULL,
          plot_fn   = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA),
            list(redcap = "qblg3", label = "Pre-HD SBP",
                 emed_table = "Observations", emed_col = "PreHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 140),
            list(redcap = "qblg4", label = "Pre-HD DBP",
                 emed_table = "Observations", emed_col = "PreHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 90)
          ),
          target       = NULL,
          stat_method  = "pct_in_range() for each metric",
          r_code       = 'tibble(Metric = c("URR >65%", ...), `National %` = c(pct_in_range(df$qblg9, 65, Inf)$pct, ...))',
          pseudocode   = 'For each metric: DIVIDE(COUNT_IN_RANGE, COUNT_VALID) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id   = "ovw_missing",
          card_type = "table",
          title     = "Missing Data Overview",
          metric_fn = "compute_missing_national",
          plot_fn   = NULL,
          chart_type = "datatable",
          fields = list(),
          target       = NULL,
          stat_method  = "COUNTBLANK / COUNTROWS for each variable",
          r_code       = 'compute_missing_national(df)',
          pseudocode   = 'For each variable: DIVIDE(COUNTBLANK(Column), COUNTROWS(Table)) * 100',
          interactive_controls = NULL
        )
      )
    ),

    # ── 2. Demographics ──────────────────────────────────────────────────────
    list(
      view_id     = "demographics",
      view_title  = "Demographics",
      description = "Patient age and gender distributions by centre.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "demo_age_bar",
          card_type  = "chart",
          title      = "Age Distribution by Centre",
          metric_fn  = "compute_age_distribution",
          plot_fn    = "plot_bar",
          chart_type = "bar",
          fields = list(
            list(redcap = "age", label = "Age (years)",
                 emed_table = "Patient", emed_col = "DateOfBirth",
                 unit = "years", target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Patients binned into 7 age bands (<30, 30-39, ..., 80+); percentage within each centre",
          r_code       = 'compute_age_distribution(df)$bins |>\n  plot_bar(.data$centre_code, .data$pct, .data$age_band,\n           title = "Age Distribution", y_label = "Percentage (%)")',
          pseudocode   = 'SWITCH(Age, <30, "Under 30", 30-39, "30-39", ..., >=80, "80+"); GROUP BY CentreCode, AgeBand; DIVIDE(COUNT, SUM_CENTRE) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id    = "demo_gender_bar",
          card_type  = "chart",
          title      = "Gender Breakdown by Centre",
          metric_fn  = "compute_gender_breakdown",
          plot_fn    = "plot_bar",
          chart_type = "bar",
          fields = list(
            list(redcap = "gender", label = "Gender",
                 emed_table = "Patient", emed_col = "Gender",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Count and percentage of Male/Female per centre",
          r_code       = 'compute_gender_breakdown(df) |>\n  plot_bar(.data$centre_code, .data$pct, .data$gender,\n           title = "Gender Breakdown", y_label = "Percentage (%)")',
          pseudocode   = 'GROUP BY CentreCode, Gender; DIVIDE(COUNT, SUM_CENTRE) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id    = "demo_age_table",
          card_type  = "table",
          title      = "Age Summary Statistics",
          metric_fn  = "compute_age_distribution",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "age", label = "Age (years)",
                 emed_table = "Patient", emed_col = "DateOfBirth",
                 unit = "years", target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Mean, median, SD, min, max of age by centre",
          r_code       = 'compute_age_distribution(df)$summary',
          pseudocode   = 'GROUP BY CentreCode; AVERAGE(Age), MEDIAN(Age), STDEV.P(Age), MIN(Age), MAX(Age)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "demo_counts_table",
          card_type  = "table",
          title      = "Patient Counts",
          metric_fn  = "compute_patient_counts",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "record_id", label = "Patient ID",
                 emed_table = "Patient", emed_col = "PatientID",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "COUNT grouped by centre",
          r_code       = 'compute_patient_counts(df)',
          pseudocode   = 'GROUP BY CentreCode; COUNT(PatientID)',
          interactive_controls = NULL
        )
      )
    ),

    # ── 3. PRD ───────────────────────────────────────────────────────────────
    list(
      view_id     = "prd",
      view_title  = "PRD",
      description = "Primary Renal Diagnosis distribution using ERA-EDTA classification groups.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "prd_bar",
          card_type  = "chart",
          title      = "PRD Distribution by Centre",
          metric_fn  = "compute_prd_proportions",
          plot_fn    = "plot_bar",
          chart_type = "bar",
          fields = list(
            list(redcap = "dxs01", label = "Primary Renal Diagnosis Code",
                 emed_table = "Diagnosis", emed_col = "PRD_Code",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "ERA-EDTA code mapped to group; stacked bar of group % per centre",
          r_code       = 'compute_prd_proportions(df) |>\n  plot_bar(.data$centre_code, .data$pct, .data$prd_group,\n           title = "PRD Distribution by Centre", position = "stack")',
          pseudocode   = 'JOIN PRD_Codes ON code; GROUP BY CentreCode, PRD_Group; DIVIDE(COUNT, SUM_CENTRE) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id    = "prd_national_table",
          card_type  = "table",
          title      = "National PRD Summary",
          metric_fn  = "compute_prd_national",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "dxs01", label = "Primary Renal Diagnosis Code",
                 emed_table = "Diagnosis", emed_col = "PRD_Code",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "National count and percentage per PRD group",
          r_code       = 'compute_prd_national(df)',
          pseudocode   = 'GROUP BY PRD_Group; COUNT; DIVIDE(COUNT, TOTAL) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id    = "prd_centre_table",
          card_type  = "table",
          title      = "PRD by Centre",
          metric_fn  = "compute_prd_proportions",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "dxs01", label = "Primary Renal Diagnosis Code",
                 emed_table = "Diagnosis", emed_col = "PRD_Code",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Pivot wider: PRD group rows, centre columns, values = %",
          r_code       = 'compute_prd_proportions(df) |>\n  tidyr::pivot_wider(id_cols = "prd_group", names_from = "centre_code",\n                     values_from = "pct", values_fill = 0)',
          pseudocode   = 'PIVOT: Rows = PRD_Group, Columns = CentreCode, Values = Percentage',
          interactive_controls = NULL
        )
      )
    ),

    # ── 4. Dialysis ──────────────────────────────────────────────────────────
    list(
      view_id     = "dialysis",
      view_title  = "Dialysis",
      description = "Dialysis adequacy: URR median and achievement (>65%), session frequency and duration.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "dial_urr_median",
          card_type  = "chart",
          title      = "URR Median by Centre",
          metric_fn  = "compute_urr_median",
          plot_fn    = "plot_caterpillar_median",
          chart_type = "caterpillar_median",
          fields = list(
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA),
            list(redcap = "hdp01", label = "HD sessions/week",
                 emed_table = "Treatment", emed_col = "SessionsPerWeek",
                 unit = "sessions/week", target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Median with order-statistic 95% CI per centre; national weighted average",
          r_code       = 'compute_urr_median(df) |>\n  plot_caterpillar_median(x_label = "URR (%)")',
          pseudocode   = 'GROUP BY CentreCode; MEDIAN(URR); CI from order statistics',
          interactive_controls = list(
            list(control = "checkbox", label = "3x/week only",
                 param = "filter_3x", default = FALSE)
          )
        ),
        list(
          card_id    = "dial_urr_achieve",
          card_type  = "chart",
          title      = "URR >65% Achievement by Centre",
          metric_fn  = "compute_urr_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA)
          ),
          target       = 0.65,
          stat_method  = "Wilson score 95% CI for proportions; national weighted average",
          r_code       = 'compute_urr_achievement(df) |>\n  plot_caterpillar_proportion(target = 0.65)',
          pseudocode   = 'GROUP BY CentreCode; DIVIDE(COUNTROWS(FILTER(URR > 65)), COUNT_VALID); Wilson CI',
          interactive_controls = list(
            list(control = "checkbox", label = "3x/week only",
                 param = "filter_3x", default = FALSE)
          )
        ),
        list(
          card_id    = "dial_urr_funnel",
          card_type  = "chart",
          title      = "URR >65% - Funnel Plot",
          metric_fn  = "compute_urr_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA)
          ),
          target       = 0.65,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits (UKRR method)",
          r_code       = 'compute_urr_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; For each n: limits = p +/- z * sqrt(p*(1-p)/n); z = 1.96 (95%), 2.998 (99.7%)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "dial_freq_table",
          card_type  = "table",
          title      = "Session Frequency",
          metric_fn  = "compute_session_frequency",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "hdp01", label = "HD sessions/week",
                 emed_table = "Treatment", emed_col = "SessionsPerWeek",
                 unit = "sessions/week", target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Categorise into <3x, 3x, >3x per week; pivot wider by centre",
          r_code       = 'compute_session_frequency(df) |>\n  tidyr::pivot_wider(id_cols = c("centre_code", "centre_name"),\n                     names_from = "hd_freq", values_from = "pct", values_fill = 0)',
          pseudocode   = 'SWITCH(SessionsPerWeek, <3, "<3x", 3, "3x", >3, ">3x"); GROUP BY Centre, Band; DIVIDE(COUNT, SUM) * 100; PIVOT',
          interactive_controls = NULL
        ),
        list(
          card_id    = "dial_duration_table",
          card_type  = "table",
          title      = "Session Duration Distribution",
          metric_fn  = "compute_session_duration",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "hdp02", label = "HD session duration (mins)",
                 emed_table = "Treatment", emed_col = "SessionDuration",
                 unit = "minutes", target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Categorise into duration bands; pivot wider by centre",
          r_code       = 'compute_session_duration(df) |>\n  tidyr::pivot_wider(id_cols = c("centre_code", "centre_name"),\n                     names_from = "duration_band", values_from = "pct", values_fill = 0)',
          pseudocode   = 'SWITCH(Duration, <180, "<3h", 180-209, "3-3.5h", ...); GROUP BY Centre, Band; DIVIDE(COUNT, SUM) * 100; PIVOT',
          interactive_controls = NULL
        )
      )
    ),

    # ── 5. Blood Pressure ────────────────────────────────────────────────────
    list(
      view_id     = "bp",
      view_title  = "Blood Pressure",
      description = "Pre-HD and post-HD blood pressure achievement rates with caterpillar and funnel plots.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "bp_pre_caterpillar",
          card_type  = "chart",
          title      = "Pre-HD BP <140/90 - Caterpillar",
          metric_fn  = "compute_pre_bp_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblg3", label = "Pre-HD SBP",
                 emed_table = "Observations", emed_col = "PreHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 140),
            list(redcap = "qblg4", label = "Pre-HD DBP",
                 emed_table = "Observations", emed_col = "PreHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 90)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI; target = SBP < 140 AND DBP < 90",
          r_code       = 'compute_pre_bp_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'FILTER(NOT ISBLANK(SBP) AND NOT ISBLANK(DBP)); DIVIDE(COUNTROWS(SBP < 140 AND DBP < 90), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bp_post_caterpillar",
          card_type  = "chart",
          title      = "Post-HD BP <130/80 - Caterpillar",
          metric_fn  = "compute_post_bp_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblg6", label = "Post-HD SBP",
                 emed_table = "Observations", emed_col = "PostHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 130),
            list(redcap = "qblg7", label = "Post-HD DBP",
                 emed_table = "Observations", emed_col = "PostHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 80)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI; target = SBP < 130 AND DBP < 80",
          r_code       = 'compute_post_bp_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'FILTER(NOT ISBLANK(SBP) AND NOT ISBLANK(DBP)); DIVIDE(COUNTROWS(SBP < 130 AND DBP < 80), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bp_pre_funnel",
          card_type  = "chart",
          title      = "Pre-HD BP <140/90 - Funnel",
          metric_fn  = "compute_pre_bp_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblg3", label = "Pre-HD SBP",
                 emed_table = "Observations", emed_col = "PreHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 140),
            list(redcap = "qblg4", label = "Pre-HD DBP",
                 emed_table = "Observations", emed_col = "PreHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 90)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_pre_bp_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bp_post_funnel",
          card_type  = "chart",
          title      = "Post-HD BP <130/80 - Funnel",
          metric_fn  = "compute_post_bp_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblg6", label = "Post-HD SBP",
                 emed_table = "Observations", emed_col = "PostHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 130),
            list(redcap = "qblg7", label = "Post-HD DBP",
                 emed_table = "Observations", emed_col = "PostHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 80)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_post_bp_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bp_summary_table",
          card_type  = "table",
          title      = "Blood Pressure Summary Statistics",
          metric_fn  = "compute_bp_summary",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "qblg3", label = "Pre-HD SBP",
                 emed_table = "Observations", emed_col = "PreHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 140),
            list(redcap = "qblg4", label = "Pre-HD DBP",
                 emed_table = "Observations", emed_col = "PreHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 90),
            list(redcap = "qblg6", label = "Post-HD SBP",
                 emed_table = "Observations", emed_col = "PostHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 130),
            list(redcap = "qblg7", label = "Post-HD DBP",
                 emed_table = "Observations", emed_col = "PostHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 80)
          ),
          target       = NULL,
          stat_method  = "Mean and median SBP/DBP (pre and post) by centre",
          r_code       = 'compute_bp_summary(df)',
          pseudocode   = 'GROUP BY CentreCode; AVERAGE(SBP), MEDIAN(SBP), AVERAGE(DBP), MEDIAN(DBP) for pre and post',
          interactive_controls = NULL
        )
      )
    ),

    # ── 6. Biochemistry ──────────────────────────────────────────────────────
    list(
      view_id     = "biochemistry",
      view_title  = "Biochemistry",
      description = "CKD-MBD markers: phosphate, adjusted calcium, PTH achievement rates and simultaneous control.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "bio_po4_caterpillar",
          card_type  = "chart",
          title      = "Phosphate 1.1-1.7 mmol/L - Caterpillar",
          metric_fn  = "compute_phosphate_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblb1", label = "Phosphate (mmol/L)",
                 emed_table = "LabResults", emed_col = "Phosphate",
                 unit = "mmol/L", target_lower = 1.1, target_upper = 1.7)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion in range 1.1-1.7",
          r_code       = 'compute_phosphate_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(PO4 >= 1.1 AND PO4 <= 1.7)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_po4_funnel",
          card_type  = "chart",
          title      = "Phosphate 1.1-1.7 mmol/L - Funnel",
          metric_fn  = "compute_phosphate_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblb1", label = "Phosphate (mmol/L)",
                 emed_table = "LabResults", emed_col = "Phosphate",
                 unit = "mmol/L", target_lower = 1.1, target_upper = 1.7)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_phosphate_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_ca_caterpillar",
          card_type  = "chart",
          title      = "Adj. Calcium 2.2-2.5 mmol/L - Caterpillar",
          metric_fn  = "compute_calcium_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblb4", label = "Adjusted Calcium (mmol/L)",
                 emed_table = "LabResults", emed_col = "AdjCalcium",
                 unit = "mmol/L", target_lower = 2.2, target_upper = 2.5)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion in range 2.2-2.5",
          r_code       = 'compute_calcium_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(Ca >= 2.2 AND Ca <= 2.5)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_ca_funnel",
          card_type  = "chart",
          title      = "Adj. Calcium 2.2-2.5 mmol/L - Funnel",
          metric_fn  = "compute_calcium_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblb4", label = "Adjusted Calcium (mmol/L)",
                 emed_table = "LabResults", emed_col = "AdjCalcium",
                 unit = "mmol/L", target_lower = 2.2, target_upper = 2.5)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_calcium_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_pth_caterpillar",
          card_type  = "chart",
          title      = "PTH 16-72 pmol/L - Caterpillar",
          metric_fn  = "compute_pth_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblb9", label = "PTH (pmol/L)",
                 emed_table = "LabResults", emed_col = "PTH",
                 unit = "pmol/L", target_lower = 16, target_upper = 72)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion in range 16-72",
          r_code       = 'compute_pth_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(PTH >= 16 AND PTH <= 72)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_pth_funnel",
          card_type  = "chart",
          title      = "PTH 16-72 pmol/L - Funnel",
          metric_fn  = "compute_pth_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblb9", label = "PTH (pmol/L)",
                 emed_table = "LabResults", emed_col = "PTH",
                 unit = "pmol/L", target_lower = 16, target_upper = 72)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_pth_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bio_mbd_caterpillar",
          card_type  = "chart",
          title      = "Simultaneous CKD-MBD Control - Caterpillar",
          metric_fn  = "compute_ckd_mbd_simultaneous",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblb1", label = "Phosphate (mmol/L)",
                 emed_table = "LabResults", emed_col = "Phosphate",
                 unit = "mmol/L", target_lower = 1.1, target_upper = 1.7),
            list(redcap = "qblb4", label = "Adjusted Calcium (mmol/L)",
                 emed_table = "LabResults", emed_col = "AdjCalcium",
                 unit = "mmol/L", target_lower = 2.2, target_upper = 2.5),
            list(redcap = "qblb9", label = "PTH (pmol/L)",
                 emed_table = "LabResults", emed_col = "PTH",
                 unit = "pmol/L", target_lower = 16, target_upper = 72)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI; patient achieves ALL THREE targets simultaneously",
          r_code       = 'compute_ckd_mbd_simultaneous(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'FILTER(PO4 in range AND Ca in range AND PTH in range); DIVIDE(COUNT_ALL_THREE, COUNT_ALL_VALID)',
          interactive_controls = NULL
        )
      )
    ),

    # ── 7. Bicarb/K+ ────────────────────────────────────────────────────────
    list(
      view_id     = "bicarbonate",
      view_title  = "Bicarb/K+",
      description = "Pre-dialysis potassium and bicarbonate achievement rates.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "bk_k_caterpillar",
          card_type  = "chart",
          title      = "Potassium 4-6 mmol/L - Caterpillar",
          metric_fn  = "compute_potassium_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qbla9", label = "Potassium (mmol/L)",
                 emed_table = "LabResults", emed_col = "Potassium",
                 unit = "mmol/L", target_lower = 4, target_upper = 6)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion in range 4-6",
          r_code       = 'compute_potassium_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(K >= 4 AND K <= 6)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bk_k_funnel",
          card_type  = "chart",
          title      = "Potassium 4-6 mmol/L - Funnel",
          metric_fn  = "compute_potassium_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qbla9", label = "Potassium (mmol/L)",
                 emed_table = "LabResults", emed_col = "Potassium",
                 unit = "mmol/L", target_lower = 4, target_upper = 6)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_potassium_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bk_hco3_caterpillar",
          card_type  = "chart",
          title      = "Bicarbonate 18-26 mmol/L - Caterpillar",
          metric_fn  = "compute_bicarbonate_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qbla4", label = "Bicarbonate (mmol/L)",
                 emed_table = "LabResults", emed_col = "Bicarbonate",
                 unit = "mmol/L", target_lower = 18, target_upper = 26)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion in range 18-26",
          r_code       = 'compute_bicarbonate_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(HCO3 >= 18 AND HCO3 <= 26)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "bk_hco3_funnel",
          card_type  = "chart",
          title      = "Bicarbonate 18-26 mmol/L - Funnel",
          metric_fn  = "compute_bicarbonate_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qbla4", label = "Bicarbonate (mmol/L)",
                 emed_table = "LabResults", emed_col = "Bicarbonate",
                 unit = "mmol/L", target_lower = 18, target_upper = 26)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_bicarbonate_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        )
      )
    ),

    # ── 8. Anaemia ───────────────────────────────────────────────────────────
    list(
      view_id     = "anaemia",
      view_title  = "Anaemia",
      description = "Haemoglobin and ferritin metrics for anaemia management.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "ana_hb_median",
          card_type  = "chart",
          title      = "Haemoglobin Median - Caterpillar",
          metric_fn  = "compute_hb_median",
          plot_fn    = "plot_caterpillar_median",
          chart_type = "caterpillar_median",
          fields = list(
            list(redcap = "qble1", label = "Haemoglobin (g/dL)",
                 emed_table = "LabResults", emed_col = "Haemoglobin",
                 unit = "g/dL", target_lower = 10, target_upper = 12)
          ),
          target       = NULL,
          stat_method  = "Median with order-statistic 95% CI per centre",
          r_code       = 'compute_hb_median(df) |>\n  plot_caterpillar_median(x_label = "Hb (g/dL)")',
          pseudocode   = 'GROUP BY CentreCode; MEDIAN(Hb); CI from order statistics',
          interactive_controls = NULL
        ),
        list(
          card_id    = "ana_hb_funnel",
          card_type  = "chart",
          title      = "Hb 10-12 g/dL - Funnel",
          metric_fn  = "compute_hb_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qble1", label = "Haemoglobin (g/dL)",
                 emed_table = "LabResults", emed_col = "Haemoglobin",
                 unit = "g/dL", target_lower = 10, target_upper = 12)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_hb_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "ana_hb_dist",
          card_type  = "chart",
          title      = "Haemoglobin Distribution by Centre",
          metric_fn  = NULL,
          plot_fn    = "plot_distribution",
          chart_type = "distribution",
          fields = list(
            list(redcap = "qble1", label = "Haemoglobin (g/dL)",
                 emed_table = "LabResults", emed_col = "Haemoglobin",
                 unit = "g/dL", target_lower = 10, target_upper = 12)
          ),
          target       = NULL,
          stat_method  = "Histogram with 20 bins, faceted by centre; target range 10-12 g/dL shaded",
          r_code       = 'plot_distribution(df, .data$qble1,\n  title = "Haemoglobin Distribution",\n  x_label = "Hb (g/dL)",\n  target_lower = 10, target_upper = 12)',
          pseudocode   = 'HISTOGRAM(Hb, bins = 20); FACET BY CentreCode; SHADE region 10-12',
          interactive_controls = NULL
        ),
        list(
          card_id    = "ana_ferritin_caterpillar",
          card_type  = "chart",
          title      = "Ferritin >=200 ug/L - Caterpillar",
          metric_fn  = "compute_ferritin_achievement",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qblf1", label = "Ferritin (ug/L)",
                 emed_table = "LabResults", emed_col = "Ferritin",
                 unit = "ug/L", target_lower = 200, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion >= 200",
          r_code       = 'compute_ferritin_achievement(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(Ferritin >= 200)), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "ana_ferritin_funnel",
          card_type  = "chart",
          title      = "Ferritin >=200 ug/L - Funnel",
          metric_fn  = "compute_ferritin_achievement",
          plot_fn    = "plot_funnel",
          chart_type = "funnel",
          fields = list(
            list(redcap = "qblf1", label = "Ferritin (ug/L)",
                 emed_table = "LabResults", emed_col = "Ferritin",
                 unit = "ug/L", target_lower = 200, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Spiegelhalter funnel with 95% and 99.7% control limits",
          r_code       = 'compute_ferritin_achievement(df) |>\n  plot_funnel()',
          pseudocode   = 'National rate p; limits = p +/- z * sqrt(p*(1-p)/n)',
          interactive_controls = NULL
        )
      )
    ),

    # ── 9. Vascular Access ───────────────────────────────────────────────────
    list(
      view_id     = "access",
      view_title  = "Access",
      description = "Vascular access type distribution (AVF, AVG, catheter) and AVF rate by centre.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "acc_bar",
          card_type  = "chart",
          title      = "Vascular Access Distribution by Centre",
          metric_fn  = "compute_access_distribution",
          plot_fn    = "plot_bar",
          chart_type = "bar",
          fields = list(
            list(redcap = "qhd20", label = "Vascular Access Type",
                 emed_table = "Treatment", emed_col = "AccessType",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Stacked bar: AVF/AVG/Catheter count and % per centre",
          r_code       = 'compute_access_distribution(df) |>\n  plot_bar(.data$centre_code, .data$pct, .data$access_type,\n           title = "Vascular Access Type", position = "stack")',
          pseudocode   = 'GROUP BY CentreCode, AccessType; COUNT; DIVIDE(COUNT, SUM_CENTRE) * 100',
          interactive_controls = NULL
        ),
        list(
          card_id    = "acc_avf_caterpillar",
          card_type  = "chart",
          title      = "AVF Rate - Caterpillar",
          metric_fn  = "compute_avf_rate",
          plot_fn    = "plot_caterpillar_proportion",
          chart_type = "caterpillar_proportion",
          fields = list(
            list(redcap = "qhd20", label = "Vascular Access Type",
                 emed_table = "Treatment", emed_col = "AccessType",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Wilson score 95% CI for proportion using AVF",
          r_code       = 'compute_avf_rate(df) |>\n  plot_caterpillar_proportion()',
          pseudocode   = 'DIVIDE(COUNTROWS(FILTER(AccessType = "AVF")), COUNT_VALID)',
          interactive_controls = NULL
        ),
        list(
          card_id    = "acc_detail_table",
          card_type  = "table",
          title      = "Access Type - Detail Table",
          metric_fn  = "compute_access_distribution",
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "qhd20", label = "Vascular Access Type",
                 emed_table = "Treatment", emed_col = "AccessType",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Pivot wider: centres as rows, access types as columns (count + %)",
          r_code       = 'compute_access_distribution(df) |>\n  tidyr::pivot_wider(id_cols = c("centre_code", "centre_name"),\n                     names_from = "access_type",\n                     values_from = c("count", "pct"), values_fill = 0)',
          pseudocode   = 'PIVOT: Rows = Centre, Columns = AccessType, Values = Count + Percentage',
          interactive_controls = NULL
        )
      )
    ),

    # ── 10. Drill-down ───────────────────────────────────────────────────────
    list(
      view_id     = "drilldown",
      view_title  = "Drill-down",
      description = "Patient-level data table with all clinical variables for quality improvement review.",
      filters     = c("centre", "consultant"),
      cards       = list(
        list(
          card_id    = "dd_patient_table",
          card_type  = "table",
          title      = "Patient-Level Data",
          metric_fn  = NULL,
          plot_fn    = NULL,
          chart_type = "datatable",
          fields = list(
            list(redcap = "record_id", label = "Patient ID",
                 emed_table = "Patient", emed_col = "PatientID",
                 unit = NA_character_, target_lower = NA, target_upper = NA),
            list(redcap = "centre_code", label = "Centre",
                 emed_table = "Centre", emed_col = "CentreCode",
                 unit = NA_character_, target_lower = NA, target_upper = NA),
            list(redcap = "consultant", label = "Consultant",
                 emed_table = "Staff", emed_col = "ConsultantName",
                 unit = NA_character_, target_lower = NA, target_upper = NA),
            list(redcap = "age", label = "Age",
                 emed_table = "Patient", emed_col = "DateOfBirth",
                 unit = "years", target_lower = NA, target_upper = NA),
            list(redcap = "gender", label = "Gender",
                 emed_table = "Patient", emed_col = "Gender",
                 unit = NA_character_, target_lower = NA, target_upper = NA),
            list(redcap = "qblg9", label = "URR (%)",
                 emed_table = "LabResults", emed_col = "URR",
                 unit = "%", target_lower = 65, target_upper = NA),
            list(redcap = "qblg3", label = "Pre SBP",
                 emed_table = "Observations", emed_col = "PreHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 140),
            list(redcap = "qblg4", label = "Pre DBP",
                 emed_table = "Observations", emed_col = "PreHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 90),
            list(redcap = "qblg6", label = "Post SBP",
                 emed_table = "Observations", emed_col = "PostHD_SBP",
                 unit = "mmHg", target_lower = NA, target_upper = 130),
            list(redcap = "qblg7", label = "Post DBP",
                 emed_table = "Observations", emed_col = "PostHD_DBP",
                 unit = "mmHg", target_lower = NA, target_upper = 80),
            list(redcap = "qblb1", label = "PO4",
                 emed_table = "LabResults", emed_col = "Phosphate",
                 unit = "mmol/L", target_lower = 1.1, target_upper = 1.7),
            list(redcap = "qblb4", label = "Ca",
                 emed_table = "LabResults", emed_col = "AdjCalcium",
                 unit = "mmol/L", target_lower = 2.2, target_upper = 2.5),
            list(redcap = "qblb9", label = "PTH",
                 emed_table = "LabResults", emed_col = "PTH",
                 unit = "pmol/L", target_lower = 16, target_upper = 72),
            list(redcap = "qbla9", label = "K+",
                 emed_table = "LabResults", emed_col = "Potassium",
                 unit = "mmol/L", target_lower = 4, target_upper = 6),
            list(redcap = "qbla4", label = "HCO3",
                 emed_table = "LabResults", emed_col = "Bicarbonate",
                 unit = "mmol/L", target_lower = 18, target_upper = 26),
            list(redcap = "qble1", label = "Hb",
                 emed_table = "LabResults", emed_col = "Haemoglobin",
                 unit = "g/dL", target_lower = 10, target_upper = 12),
            list(redcap = "qblf1", label = "Ferritin",
                 emed_table = "LabResults", emed_col = "Ferritin",
                 unit = "ug/L", target_lower = 200, target_upper = NA),
            list(redcap = "qhd20", label = "Access",
                 emed_table = "Treatment", emed_col = "AccessType",
                 unit = NA_character_, target_lower = NA, target_upper = NA)
          ),
          target       = NULL,
          stat_method  = "Raw patient-level data; filterable by centre",
          r_code       = 'df |> dplyr::select(\n  ID = record_id, Centre = centre_code, Consultant = consultant,\n  Age = age, Gender = gender, `URR (%)` = qblg9,\n  `Pre SBP` = qblg3, `Pre DBP` = qblg4,\n  `Post SBP` = qblg6, `Post DBP` = qblg7,\n  PO4 = qblb1, Ca = qblb4, PTH = qblb9,\n  `K+` = qbla9, HCO3 = qbla4, Hb = qble1,\n  Ferritin = qblf1, Access = qhd20)',
          pseudocode   = 'SELECT PatientID, CentreCode, Consultant, Age, Gender, URR, PreSBP, PreDBP, PostSBP, PostDBP, PO4, Ca, PTH, K, HCO3, Hb, Ferritin, Access FROM PatientData WHERE CentreCode = [Selected]',
          interactive_controls = list(
            list(control = "dropdown", label = "Centre",
                 param = "centre_code", default = "first")
          )
        )
      )
    )
  )

  # Add class for method dispatch
  structure(views, class = c("dashboard_registry", "list"))
}


#' Extract all unique fields from registry
#'
#' @param registry A dashboard registry list.
#' @return A tibble of unique variable definitions.
#'
#' @export
#' @family spec
registry_fields <- function(registry) {
  fields <- list()
  for (view in registry) {
    for (card in view$cards) {
      for (field in card$fields) {
        field$card_id <- card$card_id
        field$view_id <- view$view_id
        fields <- c(fields, list(field))
      }
    }
  }
  dplyr::bind_rows(fields)
}


#' Extract all cards from registry
#'
#' @param registry A dashboard registry list.
#' @return A tibble of card definitions.
#'
#' @export
#' @family spec
registry_cards <- function(registry) {
  cards <- list()
  for (view in registry) {
    for (card in view$cards) {
      cards <- c(cards, list(tibble::tibble(
        view_id    = view$view_id,
        card_id    = card$card_id,
        card_type  = card$card_type,
        chart_type = card$chart_type %||% NA_character_,
        title      = card$title,
        metric_fn  = card$metric_fn %||% NA_character_,
        plot_fn    = card$plot_fn %||% NA_character_,
        stat_method = card$stat_method %||% NA_character_
      )))
    }
  }
  dplyr::bind_rows(cards)
}


#' Extract filter definitions from registry
#'
#' @param registry A dashboard registry list.
#' @return A tibble of filter-to-view mappings.
#'
#' @export
#' @family spec
registry_filters <- function(registry) {
  filters <- list()
  for (view in registry) {
    for (f in view$filters) {
      filter_def <- list(
        filter_name  = f,
        filter_type  = dplyr::case_when(
          f == "centre"     ~ "multi-select dropdown",
          f == "consultant" ~ "multi-select dropdown (cascades from centre)",
          TRUE              ~ "dropdown"
        ),
        source_field = dplyr::case_when(
          f == "centre"     ~ "centre_code",
          f == "consultant" ~ "consultant",
          TRUE              ~ f
        ),
        applies_to   = view$view_id
      )
      filters <- c(filters, list(tibble::tibble_row(!!!filter_def)))
    }
    # Add any card-level interactive controls
    for (card in view$cards) {
      if (!is.null(card$interactive_controls)) {
        for (ctrl in card$interactive_controls) {
          filters <- c(filters, list(tibble::tibble_row(
            filter_name  = ctrl$label,
            filter_type  = ctrl$control,
            source_field = ctrl$param,
            applies_to   = paste0(view$view_id, "/", card$card_id)
          )))
        }
      }
    }
  }
  dplyr::bind_rows(filters)
}


#' Extract colour palette for specification
#'
#' @return A tibble with colour_name, hex_code, and usage.
#'
#' @export
#' @family spec
registry_colours <- function() {
  cols <- hse_colours()
  tibble::tibble(
    colour_name = names(cols),
    hex_code    = unname(cols),
    usage       = c(
      "Primary brand colour; chart points, lines, bars",
      "Dark variant; axis text, headings",
      "Light variant; secondary elements, hover states",
      "Success/target achieved; target reference lines",
      "Warning; moderate values",
      "Danger/national average; reference lines",
      "Secondary text; axis labels, muted text",
      "Background; grid lines, card borders",
      "Dark text; plot titles, strong labels",
      "Info; value boxes, supplementary highlights",
      "Accent; rarely used, available for extra series"
    )
  )
}


#' Extract statistical methods from registry
#'
#' @param registry A dashboard registry list.
#' @return A tibble of unique statistical methods used.
#'
#' @export
#' @family spec
registry_stat_methods <- function(registry) {
  methods <- list()
  for (view in registry) {
    for (card in view$cards) {
      if (!is.null(card$stat_method) && !is.na(card$stat_method)) {
        methods <- c(methods, list(tibble::tibble_row(
          method_name = card$stat_method,
          card_id     = card$card_id,
          view_id     = view$view_id
        )))
      }
    }
  }
  method_df <- dplyr::bind_rows(methods)

  # Add standard method descriptions
  tibble::tibble(
    method_name = c(
      "Wilson score 95% CI for proportions",
      "Median with order-statistic 95% CI",
      "Spiegelhalter funnel with 95% and 99.7% control limits (UKRR method)"
    ),
    description = c(
      "Wilson score interval provides better coverage than Wald for small-sample proportions. Computed via wilson_ci(x, n).",
      "Non-parametric CI for the median based on order statistics of the sorted sample. Computed via median_ci(x).",
      "Control limits derived as p +/- z * sqrt(p*(1-p)/n) where p = national rate, z = 1.96 (95%) or 2.998 (99.7%). Standard UKRR method."
    ),
    r_function = c(
      "wilson_ci(x, n, conf_level = 0.95)",
      "median_ci(x, conf_level = 0.95)",
      "funnel_limits(target_rate, n_range)"
    ),
    formula = c(
      "p_hat = (x + z^2/2) / (n + z^2); CI = p_hat +/- z * sqrt(p*(1-p)/n + z^2/(4*n^2)) / (1 + z^2/n)",
      "j = qbinom(alpha/2, n, 0.5); k = n - j + 1; CI = (x[j], x[k]) where x is sorted",
      "lower = p - z * sqrt(p*(1-p)/n); upper = p + z * sqrt(p*(1-p)/n)"
    )
  )
}

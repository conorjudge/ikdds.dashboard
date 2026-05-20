# ikdds.dashboard 0.1.0

## New Features

* **Individual Patient QA page** (Page 12): Compare individual patient
  results against centre and national medians for 11 continuous metrics.
  Includes RAG status classification (in range / out of range / missing),
  KPI value boxes, DT comparison table, and plotly bullet chart.
  (`mod_individual_ui()`, `mod_individual_server()`)

* **14 dashboard pages** covering 7 clinical domains plus overview,
  demographics, data quality, centre profile, individual patient QA,
  working groups, methods, and drill-down.

* **Centre Profile page** (Page 11): Single-centre deep-dive with
  key metrics comparison table, bullet chart, consultant variation
  analysis, and download audit pack functionality.

* **Data Quality page** (Page 10): Completeness heatmap, national KPIs,
  flagged centres (>30% missing), and unmapped patient detection.

* **Working Groups page**: Displays working group membership and
  metric standards reference tables.

* **Methods page** (Page 13): Auto-generated from the metrics registry
  showing metric definitions, statistical methods, suppression policy,
  and data governance.

## Metrics & Statistics

* 23 clinical metrics across 7 domains with YAML-based registry
  (`inst/metrics/metrics.yml`).
* Wilson score confidence intervals for proportions (`wilson_ci()`).
* Order-statistic confidence intervals for medians (`median_ci()`).
* Spiegelhalter funnel plots with 95% and 99.7% control limits
  (`funnel_limits()`, `plot_funnel()`).
* Caterpillar plots for centre comparison (`plot_caterpillar_proportion()`,
  `plot_caterpillar_median()`).
* Small-number suppression engine (n < 10) with `apply_suppression()`.
* Completeness labelling with 70% threshold via `add_completeness_labels()`.

## Data Sources

* Synthetic data generator (`generate_synthetic_data()`) producing ~1,200
  realistic patients across 7 Irish renal centres.
* REDCap integration for live data via `IKDDS_REDCAP_URI` and
  `IKDDS_REDCAP_TOKEN` environment variables.
* Role-based access control with centre-level data filtering.

## Documentation

* 7 vignettes: dashboard overview, metric definitions, synthetic data,
  statistical methods, individual patient QA, data quality workflow,
  and deployment guide.
* Power BI handover specification (`docs/powerbi-handover-spec.md`)
  for HSE IIS team.
* pkgdown site with grouped reference and articles sections.
* 235 unit tests via testthat.

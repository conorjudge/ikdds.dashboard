# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`ikdds.dashboard` is an R package wrapping a Shiny dashboard for the Irish Kidney
Disease Data System (IKDDS) annual haemodialysis audit. It renders 23 clinical
metrics across 14 pages, fed by either synthetic demo data or live REDCap.

## Commands

All commands run from the package root in R (this is a standard R package, so the
usual `devtools`/`testthat` workflow applies):

```r
devtools::load_all()          # Load package for interactive development
devtools::test()              # Run all tests
devtools::test(filter = "metrics-bp")   # Run one test file (test-metrics-bp.R)
testthat::test_file("tests/testthat/test-metrics-bp.R")  # Alternative single-file
devtools::document()          # Regenerate man/*.Rd and NAMESPACE from roxygen
devtools::check()             # Full R CMD check
pkgdown::build_site()         # Build documentation site

run_app()                     # Launch the dashboard (synthetic data by default)
```

CI runs R-CMD-check, test-coverage, and pkgdown (see `.github/workflows/`).
After editing any roxygen comment, run `devtools::document()` — `man/` and
`NAMESPACE` are generated, never edit them by hand.

## Configuration

All runtime config comes from environment variables, read by `dashboard_config()`
(in `R/config.R`). Key vars: `IKDDS_DASH_DATA_SOURCE` (`synthetic` | `redcap`),
`IKDDS_REDCAP_URI`, `IKDDS_REDCAP_TOKEN`, `IKDDS_DASH_USER_ROLE`
(`admin` | `clinical` | `research`), `IKDDS_DASH_USER_CENTRE`. For tests, pass a
named list as the `env` argument instead of touching `Sys.getenv()`.

## Architecture

The whole app is data-driven from two registries plus a compute engine. Understand
these four layers before changing metric behaviour:

**1. Data source (`R/data-source.R`, `data-synthetic.R`, `data-redcap.R`).**
`get_audit_data(config)` is a factory returning a tibble with one row per patient.
Both synthetic and REDCap sources produce the *identical* column schema, defined by
`audit_data_columns()`. Column names are raw REDCap field codes (e.g. `qblg9` = URR,
`qble1` = Hb, `qhd20` = vascular access) — see the `get_audit_data` roxygen for the
full mapping. `validate_audit_data()` enforces the schema.

**2. Metrics registry (`inst/metrics/metrics.yml` → `R/metrics-registry.R`).**
YAML is the single source of truth for each metric's title, domain, target,
`value_field`, suppression thresholds, and — critically — `compute_fn`, the *name*
of the R function that computes it. `metrics_registry()` reads and caches the YAML
(call `reset_registry_cache()` after editing it in a session).

**3. Metrics engine (`R/metrics-engine.R`).** `compute_with_engine(data, metric_id,
group_col)` is the standard pipeline: it looks up the config, resolves `compute_fn`
by name from the package namespace, calls it, adds completeness labels, then applies
small-n suppression. **Suppression** (`apply_suppression`, default `min_n = 10`) sets
proportion/median/CI **and `n`** to `NA` for under-sized centres to prevent leaking
small counts. `group_col` is `"centre_code"`, `"consultant"`, or `"region"` — the
"compare by" toggle.

**4. Domain compute functions (`R/metrics-{domain}.R`).** One file per clinical
domain (dialysis, bp, biochemistry, anaemia, etc.). These return tidy tibbles via
shared builders in `R/metrics-helpers.R` (`compute_centre_proportion`,
`compute_centre_median`) which carry Wilson CIs / median CIs and funnel limits.
A function referenced by `compute_fn` in the YAML must accept `data` and optionally
`group_col`.

### Adding or changing a metric

1. Add/edit the entry in `inst/metrics/metrics.yml`.
2. Write the matching `compute_*` function in the relevant `R/metrics-{domain}.R`
   (or reuse an existing one) and `@export` it so the engine can resolve it by name.
3. If it appears on the spec/handover, also register it in `dashboard_registry()`
   (see below).

### Shiny layer

`R/ui.R` and `R/server.R` wire everything. Each page is a Shiny module pair
`mod_{domain}_ui` / `mod_{domain}_server` in `R/mod-{domain}.R`. `mod_filters` owns
the cascading centre/consultant filters and exposes reactives `filtered_data`,
`compare_by`, and `active_filters`, which `app_server` passes down to every module.
Role-based row filtering (`filter_by_role` in `R/access-control.R`) is applied once,
before data reaches any module.

### Spec / Power BI handover system

`R/spec-registry.R` (`dashboard_registry()`) is a *separate* declarative registry
describing every view, card, plot, variable mapping (REDCap ↔ eMed/IIS columns), and
pseudocode. `R/spec-generate.R` turns it into a multi-sheet Excel workbook for the
HSE IIS team to recreate the dashboard in Power BI. This registry is independent of
`metrics.yml` and must be kept in sync manually when views change.

## Conventions

- Errors and user-facing messages use `cli::cli_abort` / `cli::cli_inform`, not
  `stop`/`message`.
- Plotting uses ggplot2 wrapped in plotly; HSE brand colours and theme live in
  `R/theme.R` (`hse_colours`, `hse_ggplot_theme`).
- Tests use testthat edition 3 with fixtures in `tests/testthat/helper-fixtures.R`
  (`fixture_audit_data(n)`) and mocks in `helper-mocks.R` — prefer these over
  generating full synthetic data in tests.

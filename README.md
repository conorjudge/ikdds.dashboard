# ikdds.dashboard

Interactive Shiny dashboard for the **Irish Kidney Disease Data System (IKDDS)**
annual haemodialysis audit.

## Overview

Provides **14 dashboard pages** rendering 23 clinical metrics across 7 domains
as caterpillar plots, funnel plots, centre profiles, individual patient QA,
and drill-down tables. Features HSE teal theming, cascading centre/consultant
filters, Wilson CIs, and Spiegelhalter funnels.

### Domains

| Domain | Metrics |
|--------|---------|
| Dialysis Adequacy | URR median, URR >65%, session frequency/duration |
| Blood Pressure | Pre-HD <140/90, Post-HD <130/80, BP summary |
| Biochemistry | PO4, Adj Ca, PTH, simultaneous CKD-MBD control |
| Bicarb / K+ | Potassium 4-6, Bicarbonate 18-26 |
| Anaemia | Hb median, Hb 10-12, Hb distribution, Ferritin >=200 |
| Vascular Access | Access type distribution, AVF rate |
| Demographics/PRD | Age, gender, primary renal diagnosis |

### Dashboard Pages

| # | Page | Description |
|---|------|-------------|
| 1 | Overview | National summary KPIs, achievement rates, missing data |
| 2 | Demographics | Age/gender distributions by centre |
| 3 | PRD | Primary renal diagnosis breakdown |
| 4 | Dialysis | URR caterpillar + funnel, session frequency/duration |
| 5 | Blood Pressure | Pre/post-HD BP caterpillar + funnel |
| 6 | Biochemistry | PO4, Ca, PTH caterpillar + funnel |
| 7 | Bicarb / K+ | Bicarbonate and potassium caterpillar + funnel |
| 8 | Anaemia | Hb and ferritin caterpillar + funnel + distribution |
| 9 | Access | Vascular access type distribution |
| 10 | Data Quality | Completeness heatmap, flagged centres |
| 11 | Centre Profile | Single-centre deep-dive vs national averages |
| 12 | Individual | Patient-level QA: compare patient vs centre/national medians |
| 13 | Methods | Auto-generated metric definitions and statistical methods |
| 14 | Drill-down | Patient-level data table |

### Key Features

- **Metrics Registry** — Centralised YAML config (`inst/metrics/metrics.yml`)
  defining targets, suppression rules, and compute function mappings
- **Suppression Engine** — Centres with n < 10 automatically suppressed;
  values set to NA with visual indicators in plots and tables
- **Completeness Labelling** — Missingness percentages shown in centre labels
  (e.g., "Beaumont (12% missing)")
- **Compare-by Control** — Toggle between Centre and Consultant grouping
- **Data Quality Page** — Completeness heatmap, flagged centres, national KPIs
- **Centre Profile** — Single-centre deep-dive with vs-national comparisons
- **Individual Patient QA** — Compare individual patient results against centre
  and national medians with RAG status and bullet charts
- **Methods Page** — Auto-generated metric definitions, statistical methods,
  suppression policy, and data governance documentation

## Quick Start

```r
# Install from source
pak::pak("ikddsystem/ikdds-dashboard")

# Launch with synthetic demo data (default)
library(ikdds.dashboard)
run_app()
```

## Data Sources

- **Synthetic** (default) — `generate_synthetic_data()` creates ~1,200
  realistic patients across 7 Irish renal centres
- **REDCap** — Set `IKDDS_DATA_SOURCE=redcap` with `REDCAP_URI` and
  `REDCAP_TOKEN` environment variables

## Project Structure

```
R/
  config.R                 # App configuration
  data-source.R            # Data loading factory
  data-synthetic.R         # Synthetic data generator
  metrics-registry.R       # YAML config reader + cache
  metrics-engine.R         # Suppression, completeness, engine pipeline
  metrics-helpers.R        # Wilson CI, median CI, funnel limits
  metrics-{domain}.R       # Domain-specific compute functions
  metrics-missing.R        # Missing data analysis
  plot-caterpillar.R       # Caterpillar plot (proportion + median)
  plot-funnel.R            # Spiegelhalter funnel plot
  plot-table.R             # HSE-styled DT table wrapper
  mod-filters.R            # Centre/Consultant filter sidebar
  mod-overview.R           # National summary landing page
  mod-data-quality.R       # Data quality heatmap + flagging
  mod-centre-profile.R     # Single-centre deep-dive
  mod-individual.R         # Individual patient QA vs centre/national
  mod-methods.R            # Methods & definitions (auto-generated)
  mod-{domain}.R           # Domain-specific Shiny modules
  ui.R / server.R          # App UI and server wiring
  theme.R                  # HSE brand theme
inst/
  metrics/metrics.yml      # Centralised metric definitions
  app/www/custom.css       # Dashboard CSS
tests/testthat/            # Unit tests
vignettes/                 # Package vignettes (see below)
docs/
  design_decisions.md      # Architecture decision records
  powerbi-handover-spec.md # Power BI handover specification for HSE IIS
```

## Vignettes

| Vignette | Description |
|----------|-------------|
| `dashboard-overview` | Architecture overview and quick start guide |
| `metric-definitions` | All 23 clinical metrics with targets and functions |
| `synthetic-data` | Generating and exploring synthetic audit data |
| `statistical-methods` | Wilson CIs, funnel plots, and suppression worked examples |
| `individual-patient-qa` | Using the Individual Patient QA module |
| `data-quality-workflow` | Data quality assessment and completeness analysis |
| `deployment-guide` | Local, Posit Connect, and Docker deployment |

Browse vignettes with:

```r
browseVignettes("ikdds.dashboard")
```

## Testing

```r
# Run all tests (235 tests)
devtools::test()

# Full package check
devtools::check()
```

## Documentation

- `browseVignettes("ikdds.dashboard")` — Package vignettes
- [Design Decisions](docs/design_decisions.md) — Architecture and rationale
- [Power BI Handover Spec](docs/powerbi-handover-spec.md) — HSE IIS handover
- Methods page in the dashboard — Auto-generated metric definitions

## License

See [LICENSE](LICENSE) file.

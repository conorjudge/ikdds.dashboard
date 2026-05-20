#' Methods & Definitions module - UI
#'
#' Auto-generated metric definitions from the registry plus static
#' methodology text.
#'
#' @param id Module namespace ID.
#'
#' @return A [shiny::tagList()].
#'
#' @export
#' @family modules
mod_methods_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    page_description(
      shiny::tags$strong("About this page: "),
      "Definitions, statistical methods, and data governance policies ",
      "for the IKDDS Haemodialysis Audit Dashboard."
    ),

    bslib::card(
      bslib::card_header("Audit Inclusion Criteria"),
      bslib::card_body(
        shiny::HTML(audit_inclusion_text())
      )
    ),

    bslib::card(
      bslib::card_header("Metric Definitions"),
      bslib::card_body(DT::DTOutput(ns("metrics_table")))
    ),

    bslib::card(
      bslib::card_header("Statistical Methods"),
      bslib::card_body(
        shiny::HTML(statistical_methods_text())
      )
    ),

    bslib::card(
      bslib::card_header("Suppression & Completeness Policy"),
      bslib::card_body(
        shiny::HTML(suppression_policy_text())
      )
    ),

    bslib::card(
      bslib::card_header("Data Governance"),
      bslib::card_body(
        shiny::HTML(data_governance_text())
      )
    ),

    bslib::card(
      bslib::card_header("References"),
      bslib::card_body(
        shiny::HTML(references_text())
      )
    ),

    source_footnote(
      "Working groups review metric definitions and standards annually. See Working Groups tab for details."
    )
  )
}

#' Methods & Definitions module - Server
#'
#' @param id Module namespace ID.
#'
#' @export
#' @family modules
mod_methods_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    output$metrics_table <- DT::renderDT({
      reg <- metrics_registry()

      rows <- purrr::map_dfr(names(reg), function(metric_id) {
        cfg <- reg[[metric_id]]
        tibble::tibble(
          Metric = cfg$title %||% metric_id,
          Domain = cfg$domain %||% "",
          Type = cfg$type %||% "",
          Numerator = cfg$numerator_desc %||% "",
          Denominator = cfg$denominator_desc %||% "",
          Target = cfg$target_label %||% "",
          `Min N` = cfg$suppression$min_n %||% 10,
          `Min Completeness` = paste0(
            (cfg$suppression$min_completeness %||% 0.70) * 100, "%"
          )
        )
      })

      hse_datatable(rows, caption = "Metric definitions from metrics registry",
                      pageLength = 25)
    })
  })
}

# --- Static text helpers ---

#' @keywords internal
audit_inclusion_text <- function() {
  paste0(
    "<h5>Population</h5>",
    "<p>All adult patients (aged &ge;18 years) receiving maintenance ",
    "haemodialysis at participating Irish renal centres during the audit ",
    "period (calendar year).</p>",
    "<h5>Inclusion Criteria</h5>",
    "<ul>",
    "<li>Receiving in-centre or satellite haemodialysis</li>",
    "<li>On dialysis for &ge;90 days at the time of data collection</li>",
    "<li>Aged 18 years or older</li>",
    "</ul>",
    "<h5>Exclusion Criteria</h5>",
    "<ul>",
    "<li>Home haemodialysis patients (reported separately)</li>",
    "<li>Patients on dialysis for &lt;90 days (incident patients)</li>",
    "<li>Peritoneal dialysis patients</li>",
    "<li>Paediatric patients (aged &lt;18 years)</li>",
    "</ul>",
    "<h5>Acute Patient Handling</h5>",
    "<p>Acute patients (identified by admission type or dialysis duration &lt;90 days) ",
    "can skew KPI achievement rates. The dashboard provides an 'Exclude acute patients' ",
    "filter to remove these patients from all calculations. When this filter is active, ",
    "only prevalent (maintenance) HD patients are included in metric computations.</p>",
    "<h5>Data Collection</h5>",
    "<p>Data collected via REDCap electronic data capture system. ",
    "Each centre enters data for all prevalent HD patients. ",
    "Laboratory values represent the most recent result within the audit period. ",
    "Blood pressure values are the mean of the last three pre- and post-HD readings.</p>"
  )
}

#' @keywords internal
statistical_methods_text <- function() {
  paste0(
    "<h5>Caterpillar Plots</h5>",
    "<p>Centre-level proportions (or medians) are displayed as ordered ",
    "dot-and-whisker plots. Each point represents a centre's achievement rate, ",
    "with 95% confidence intervals shown as error bars.</p>",
    "<ul>",
    "<li><strong>Proportions:</strong> Wilson score confidence intervals, which ",
    "have better coverage properties than the normal approximation for small ",
    "samples and proportions near 0 or 1.</li>",
    "<li><strong>Medians:</strong> Order statistic method using exact binomial ",
    "CIs for the position of the median in the ordered sample.</li>",
    "<li><strong>National mean:</strong> Shown as a red horizontal line. ",
    "Computed as the weighted average across all centres (total events / ",
    "total patients for proportions; sample-size-weighted mean for medians).</li>",
    "</ul>",
    "<h5>Funnel Plots</h5>",
    "<p>Spiegelhalter-style funnel plots are used for centre comparison, ",
    "following the methodology of the UK Renal Registry. Each centre is plotted ",
    "as sample size (x-axis) against achievement rate (y-axis).</p>",
    "<ul>",
    "<li><strong>Control limits:</strong> 95% (solid) and 99.7% (dashed) ",
    "limits based on exact binomial variation around the national mean.</li>",
    "<li><strong>Interpretation:</strong> Centres outside the 95% limits ",
    "show more variation than expected by chance; centres outside the 99.7% ",
    "limits are considered outliers warranting investigation.</li>",
    "<li><strong>Over-dispersion:</strong> Not currently adjusted for. ",
    "A multiplicative over-dispersion factor may be applied in future versions.</li>",
    "</ul>",
    "<h5>Confidence Intervals</h5>",
    "<p>All confidence intervals are 95% unless otherwise stated. ",
    "Wilson score intervals are used for proportions; order statistic ",
    "intervals for medians.</p>"
  )
}

#' @keywords internal
suppression_policy_text <- function() {
  paste0(
    "<h5>Small Number Suppression</h5>",
    "<p>To protect patient confidentiality and prevent unreliable estimates, ",
    "centres with fewer than 10 patients with data for a given metric are ",
    "suppressed:</p>",
    "<ul>",
    "<li>Caterpillar plots: suppressed centres shown as grey open circles ",
    "labelled 'Suppressed'</li>",
    "<li>Funnel plots: suppressed centres excluded from the plot (but ",
    "included in the national mean calculation)</li>",
    "<li>Tables: suppressed rows shown in grey italic text</li>",
    "</ul>",
    "<h5>Completeness Threshold</h5>",
    "<p>Centres where a metric has &gt;30% missing data (i.e., &lt;70% ",
    "completeness) are flagged on the Data Quality page. Missingness ",
    "percentages are shown in centre labels on caterpillar plots, e.g., ",
    "'Beaumont (12% missing)'.</p>",
    "<p>The 70% threshold is based on European Renal Best Practice (ERBP) ",
    "recommendations for registry data quality.</p>"
  )
}

#' @keywords internal
references_text <- function() {
  paste0(
    "<h5>Clinical Guidelines</h5>",
    "<ol>",
    "<li><strong>KDIGO 2024</strong>: Kidney Disease: Improving Global Outcomes. ",
    "Clinical Practice Guidelines for CKD-MBD, Anaemia in CKD, and Blood Pressure ",
    "in CKD. <em>Kidney International Supplements</em>, 2024.</li>",
    "<li><strong>KDOQI</strong>: Kidney Disease Outcomes Quality Initiative. ",
    "Clinical Practice Guidelines for Hemodialysis Adequacy. ",
    "<em>American Journal of Kidney Diseases</em>.</li>",
    "<li><strong>ERA-EDTA</strong>: European Renal Association / European Dialysis and ",
    "Transplant Association. Best Practice Guidelines. ",
    "<em>Nephrology Dialysis Transplantation</em>.</li>",
    "<li><strong>NICE</strong>: National Institute for Health and Care Excellence. ",
    "Chronic kidney disease: managing anaemia (NG8). Updated 2021.</li>",
    "<li><strong>UK Renal Registry</strong>: Annual Report and Clinical Audit ",
    "Standards. <em>ukrr.org</em>.</li>",
    "<li><strong>Irish Renal Association</strong>: National guidelines for ",
    "haemodialysis practice in Ireland.</li>",
    "</ol>",
    "<h5>Statistical References</h5>",
    "<ol>",
    "<li>Spiegelhalter DJ. Funnel plots for comparing institutional performance. ",
    "<em>Statistics in Medicine</em>, 2005; 24: 1185-1202.</li>",
    "<li>Wilson EB. Probable inference, the law of succession, and statistical inference. ",
    "<em>JASA</em>, 1927; 22: 209-212.</li>",
    "</ol>"
  )
}

#' @keywords internal
data_governance_text <- function() {
  paste0(
    "<h5>Data Controller</h5>",
    "<p>The Health Service Executive (HSE) is the data controller for the ",
    "IKDDS audit data.</p>",
    "<h5>Privacy & Confidentiality</h5>",
    "<ul>",
    "<li>No patient-level data is displayed in this dashboard</li>",
    "<li>All metrics are aggregated at centre or consultant level</li>",
    "<li>Small number suppression (n&lt;10) prevents re-identification</li>",
    "<li>The drill-down tab shows aggregated summary data only</li>",
    "</ul>",
    "<h5>Data Access</h5>",
    "<p>Access to the dashboard is restricted to authorised members of the ",
    "IKDDS audit team and participating centre leads. Data is transmitted ",
    "via encrypted channels and stored in accordance with HSE data ",
    "protection policies.</p>"
  )
}

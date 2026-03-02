#' Generate synthetic audit data
#'
#' Creates a realistic synthetic dataset of ~1,200 in-centre haemodialysis
#' patients across 7 Irish renal centres. Values are drawn from distributions
#' calibrated to published UKRR/ERA-EDTA registry data with deliberate
#' between-centre variation.
#'
#' @param seed Random seed for reproducibility. Default `42`.
#' @param n_total Approximate total number of patients. Default `1200`.
#'
#' @return A tibble with one row per patient and columns matching REDCap
#'   field names used by the audit metrics.
#'
#' @export
#' @family data
#'
#' @examples
#' df <- generate_synthetic_data(seed = 42, n_total = 100)
#' nrow(df)
generate_synthetic_data <- function(seed = 42, n_total = 1200) {
  set.seed(seed)

  centres <- load_centres()
  n_centres <- nrow(centres)

  # Distribute patients unevenly across centres (larger units = more patients)
  centre_weights <- c(0.22, 0.16, 0.14, 0.13, 0.15, 0.10, 0.10)
  centre_ns <- as.integer(round(n_total * centre_weights))
  centre_ns[1] <- centre_ns[1] + (n_total - sum(centre_ns))  # adjust rounding

  patients <- list()
  record_counter <- 0L

  for (i in seq_len(n_centres)) {
    ni <- centre_ns[i]
    centre_code <- centres$centre_code[i]
    centre_name <- centres$centre_name[i]

    # Centre-specific offsets for realistic variation
    urr_offset   <- stats::rnorm(1, 0, 2)
    bp_offset    <- stats::rnorm(1, 0, 3)
    hb_offset    <- stats::rnorm(1, 0, 0.3)
    po4_offset   <- stats::rnorm(1, 0, 0.05)
    avf_rate     <- stats::rbeta(1, 12, 8)  # ~60% AVF

    # Demographics
    age <- pmax(18, pmin(95, stats::rnorm(ni, mean = 65, sd = 14)))
    gender <- sample(c("Male", "Female"), ni, replace = TRUE, prob = c(0.6, 0.4))

    # Consultants (2-4 per centre)
    n_consultants <- sample(2:4, 1)
    consultant_names <- paste0("Dr. ", centre_code, "_", LETTERS[seq_len(n_consultants)])
    consultant <- sample(consultant_names, ni, replace = TRUE)

    # PRD codes (weighted towards common diagnoses)
    prd_codes <- c(40, 72, 64, 11, 81, 00, 10, 17, 60, 65, 84, 99)
    prd_weights <- c(0.15, 0.18, 0.08, 0.10, 0.12, 0.10, 0.05, 0.05,
                     0.04, 0.03, 0.05, 0.05)
    dxs01 <- sample(prd_codes, ni, replace = TRUE, prob = prd_weights)

    # Dialysis adequacy
    urr <- pmax(30, pmin(95, stats::rnorm(ni, mean = 72 + urr_offset, sd = 8)))
    hd_times_per_week <- sample(c(2, 3, 4), ni, replace = TRUE,
                                 prob = c(0.05, 0.90, 0.05))
    hd_duration_mins <- sample(c(180, 210, 240, 270, 300), ni, replace = TRUE,
                                prob = c(0.05, 0.15, 0.55, 0.15, 0.10))

    # Blood pressure
    pre_sbp  <- pmax(90, pmin(220, stats::rnorm(ni, 145 + bp_offset, 22)))
    pre_dbp  <- pmax(40, pmin(120, stats::rnorm(ni, 78 + bp_offset * 0.5, 12)))
    post_sbp <- pmax(80, pmin(200, stats::rnorm(ni, 132 + bp_offset, 20)))
    post_dbp <- pmax(35, pmin(110, stats::rnorm(ni, 72 + bp_offset * 0.5, 11)))

    # Biochemistry
    phosphate <- pmax(0.4, pmin(3.5, stats::rnorm(ni, 1.45 + po4_offset, 0.35)))
    adj_calcium <- pmax(1.5, pmin(3.2, stats::rnorm(ni, 2.35, 0.15)))
    pth <- pmax(1, pmin(300, stats::rlnorm(ni, log(35), 0.7)))
    potassium <- pmax(2.5, pmin(7.5, stats::rnorm(ni, 5.0, 0.7)))
    bicarbonate <- pmax(10, pmin(35, stats::rnorm(ni, 22, 3)))

    # Anaemia
    hb <- pmax(6, pmin(16, stats::rnorm(ni, 10.8 + hb_offset, 1.4)))
    ferritin <- pmax(10, pmin(2000, stats::rlnorm(ni, log(350), 0.6)))

    # Vascular access
    access_probs <- c(avf_rate, 0.08, 1 - avf_rate - 0.08)
    access_type <- sample(c("AVF", "AVG", "Catheter"), ni, replace = TRUE,
                          prob = pmax(0.01, access_probs))

    # Introduce some missing data (5-15% per variable)
    add_missing <- function(x, rate = 0.08) {
      miss_idx <- sample(seq_along(x), size = ceiling(length(x) * rate))
      x[miss_idx] <- NA
      x
    }

    record_ids <- seq(record_counter + 1L, record_counter + ni)
    record_counter <- record_counter + ni

    centre_df <- tibble::tibble(
      record_id       = as.character(record_ids),
      centre_code     = centre_code,
      centre_name     = centre_name,
      consultant      = consultant,
      age             = round(age, 1),
      gender          = gender,
      dxs01           = as.character(dxs01),
      qblg9           = round(add_missing(urr), 1),
      hdp01           = hd_times_per_week,
      hdp02           = hd_duration_mins,
      qblg3           = round(add_missing(pre_sbp)),
      qblg4           = round(add_missing(pre_dbp)),
      qblg6           = round(add_missing(post_sbp)),
      qblg7           = round(add_missing(post_dbp)),
      qblb1           = round(add_missing(phosphate), 2),
      qblb4           = round(add_missing(adj_calcium), 2),
      qblb9           = round(add_missing(pth), 1),
      qbla9           = round(add_missing(potassium), 1),
      qbla4           = round(add_missing(bicarbonate), 1),
      qble1           = round(add_missing(hb), 1),
      qblf1           = round(add_missing(ferritin)),
      qhd20           = add_missing(access_type, rate = 0.03)
    )

    patients[[i]] <- centre_df
  }

  dplyr::bind_rows(patients)
}

#' Load Irish renal centres reference data
#'
#' @return A tibble with centre_code, centre_name, region.
#'
#' @keywords internal
load_centres <- function() {
  path <- system.file("extdata", "irish_renal_centres.csv",
                      package = "ikdds.dashboard", mustWork = FALSE)
  if (nzchar(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    # Fallback for when package is not installed
    tibble::tibble(
      centre_code = c("BEA", "MAT", "TAL", "STJ", "CUH", "UHL", "GUH"),
      centre_name = c("Beaumont Hospital",
                      "Mater Misericordiae University Hospital",
                      "Tallaght University Hospital",
                      "St. James's Hospital",
                      "Cork University Hospital",
                      "University Hospital Limerick",
                      "Galway University Hospital"),
      region = c("Dublin", "Dublin", "Dublin", "Dublin",
                 "Munster", "Midwest", "West")
    )
  }
}

#' Load ERA PRD code lookup
#'
#' @return A tibble with code, group, description.
#'
#' @keywords internal
load_era_prd_codes <- function() {
  path <- system.file("extdata", "era_prd_codes.csv",
                      package = "ikdds.dashboard", mustWork = FALSE)
  if (nzchar(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    tibble::tibble(
      code = character(),
      group = character(),
      description = character()
    )
  }
}

#' Load metric thresholds
#'
#' @return A tibble with metric_id, metric_name, domain, lower, upper, unit.
#'
#' @keywords internal
load_metric_thresholds <- function() {
  path <- system.file("extdata", "metric_thresholds.csv",
                      package = "ikdds.dashboard", mustWork = FALSE)
  if (nzchar(path)) {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    tibble::tibble(
      metric_id = character(),
      metric_name = character(),
      domain = character(),
      lower = double(),
      upper = double(),
      unit = character(),
      description = character()
    )
  }
}

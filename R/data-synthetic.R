#' Generate synthetic audit data
#'
#' Creates a realistic synthetic dataset of ~2,000 in-centre haemodialysis
#' patients across 23 Irish dialysis centres (14 main renal centres and
#' 9 satellite units). Values are drawn from distributions calibrated to
#' published UKRR/ERA-EDTA registry data with deliberate between-centre
#' variation. Satellite units have fewer consultants, lower acute admission
#' rates, and slightly better clinical parameters reflecting more stable
#' patient populations.
#'
#' @param seed Random seed for reproducibility. Default `42`.
#' @param n_total Approximate total number of patients. Default `2000`.
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
generate_synthetic_data <- function(seed = 42, n_total = 2000) {
  set.seed(seed)

  centres <- load_centres()
  n_centres <- nrow(centres)

  # Distribute patients across 23 centres (weights sum to 1.0)
  centre_weights <- c(
    0.115, 0.070, 0.060, 0.090, 0.100,   # BEA MAT SVH TAL CUH
    0.030, 0.050, 0.045, 0.050, 0.035,   # UHK UHL WAT GUH MAY
    0.030, 0.025, 0.030, 0.035,           # SLI LET CAV TUL
    0.030, 0.025, 0.035, 0.025, 0.020,   # NCR DRO BCN CLN POR
    0.025, 0.020, 0.020, 0.035            # FKK WEX LSU WEL
  )
  centre_ns <- as.integer(round(n_total * centre_weights))
  centre_ns[1] <- centre_ns[1] + (n_total - sum(centre_ns))  # adjust rounding

  patients <- list()
  record_counter <- 0L

  for (i in seq_len(n_centres)) {
    ni <- centre_ns[i]
    centre_code <- centres$centre_code[i]
    centre_name <- centres$centre_name[i]
    is_satellite <- if ("unit_type" %in% names(centres)) {
      centres$unit_type[i] == "Satellite"
    } else {
      FALSE
    }

    # Centre-specific offsets for realistic variation
    # Satellites have slightly better (more stable) clinical values
    urr_offset   <- stats::rnorm(1, ifelse(is_satellite, 1, 0), 2)
    bp_offset    <- stats::rnorm(1, ifelse(is_satellite, -2, 0), 3)
    hb_offset    <- stats::rnorm(1, ifelse(is_satellite, 0.2, 0), 0.3)
    po4_offset   <- stats::rnorm(1, ifelse(is_satellite, -0.03, 0), 0.05)
    avf_rate     <- stats::rbeta(1, ifelse(is_satellite, 14, 12), 8)

    # Demographics
    age <- pmax(18, pmin(95, stats::rnorm(ni, mean = 65, sd = 14)))
    gender <- sample(c("Male", "Female"), ni, replace = TRUE, prob = c(0.6, 0.4))

    # Consultants: satellites have 1-2, main centres have 2-4
    if (is_satellite) {
      n_consultants <- sample(1:2, 1)
    } else {
      n_consultants <- sample(2:4, 1)
    }
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

    # Acute patient flag: ~4% for satellites, ~8% for main centres
    acute_rate <- ifelse(is_satellite, 0.04, 0.08)
    is_acute <- sample(c(TRUE, FALSE), ni, replace = TRUE,
                       prob = c(acute_rate, 1 - acute_rate))

    # Ethnicity (realistic Irish demographics with high missing rate)
    ethnicity_raw <- sample(
      c("White Irish", "White Other", "Asian", "Black African",
        "Other", "Not Stated"),
      ni, replace = TRUE,
      prob = c(0.70, 0.12, 0.05, 0.04, 0.04, 0.05)
    )

    # Consultant mapping gaps (~5% unmapped)
    consultant_with_gaps <- consultant
    unmapped_idx <- sample(seq_len(ni), size = ceiling(ni * 0.05))
    consultant_with_gaps[unmapped_idx] <- NA_character_

    # Introduce some missing data (5-15% per variable)
    add_missing <- function(x, rate = 0.08) {
      miss_idx <- sample(seq_along(x), size = ceiling(length(x) * rate))
      x[miss_idx] <- NA
      x
    }

    record_ids <- seq(record_counter + 1L, record_counter + ni)
    record_counter <- record_counter + ni

    # Unit and region info from centres reference data
    unit_code_val <- if ("unit_code" %in% names(centres)) {
      centres$unit_code[i]
    } else {
      centre_code
    }
    unit_name_val <- if ("unit_name" %in% names(centres)) {
      centres$unit_name[i]
    } else {
      centre_name
    }
    unit_type_val <- if ("unit_type" %in% names(centres)) {
      centres$unit_type[i]
    } else {
      "Renal"
    }
    region_val <- if ("region" %in% names(centres)) {
      centres$region[i]
    } else {
      NA_character_
    }

    centre_df <- tibble::tibble(
      record_id       = as.character(record_ids),
      centre_code     = centre_code,
      centre_name     = centre_name,
      region          = region_val,
      unit_code       = unit_code_val,
      unit_name       = unit_name_val,
      unit_type       = unit_type_val,
      consultant      = consultant_with_gaps,
      is_acute        = is_acute,
      age             = round(age),
      gender          = gender,
      ethnicity       = add_missing(ethnicity_raw, rate = 0.30),
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
      centre_code = c("BEA", "MAT", "SVH", "TAL",
                       "CUH", "UHK", "UHL", "WAT", "GUH",
                       "MAY", "SLI", "LET", "CAV", "TUL",
                       "NCR", "DRO", "BCN", "CLN", "POR",
                       "FKK", "WEX", "LSU", "WEL"),
      centre_name = c("Beaumont Hospital",
                      "Mater Misericordiae University Hospital",
                      "St Vincent's University Hospital",
                      "Tallaght University Hospital",
                      "Cork University Hospital",
                      "University Hospital Kerry",
                      "University Hospital Limerick",
                      "University Hospital Waterford",
                      "Merlin Park University Hospital",
                      "Mayo University Hospital",
                      "Sligo University Hospital",
                      "Letterkenny University Hospital",
                      "Cavan General Hospital",
                      "Midland Regional Hospital Tullamore",
                      "Northern Cross Dialysis Unit",
                      "Drogheda Dialysis Unit",
                      "Beacon Renal Sandyford",
                      "Clondalkin Dialysis Unit",
                      "Portlaoise Dialysis Unit",
                      "Kilkenny Dialysis Unit",
                      "Wexford Dialysis Unit",
                      "Limerick Satellite Dialysis Unit",
                      "Wellstone Renal Dialysis Clinic"),
      region = c("Dublin and North East", "Dublin and Midlands",
                 "Dublin and Midlands", "Dublin and Midlands",
                 "South", "South West", "Mid West", "South",
                 "West and North West", "West and North West",
                 "West and North West", "West and North West",
                 "West and North West", "Dublin and Midlands",
                 "Dublin and North East", "Dublin and North East",
                 "Dublin and Midlands", "Dublin and Midlands",
                 "Dublin and Midlands", "South", "South",
                 "Mid West", "West and North West"),
      unit_code = c("BEA", "MAT", "SVH", "TAL",
                    "CUH", "UHK", "UHL", "WAT", "GUH",
                    "MAY", "SLI", "LET", "CAV", "TUL",
                    "BEA", "BEA", "MAT", "TAL", "TAL",
                    "CUH", "CUH", "UHL", "GUH"),
      unit_name = c("Beaumont Hospital",
                    "Mater Misericordiae University Hospital",
                    "St Vincent's University Hospital",
                    "Tallaght University Hospital / St James's Hospital",
                    "Cork University Hospital",
                    "University Hospital Kerry",
                    "University Hospital Limerick",
                    "University Hospital Waterford",
                    "Galway University Hospitals",
                    "Mayo University Hospital",
                    "Sligo University Hospital",
                    "Letterkenny University Hospital",
                    "Cavan General Hospital",
                    "Midland Regional Hospital Tullamore",
                    "Beaumont Hospital",
                    "Beaumont Hospital",
                    "Mater Misericordiae University Hospital",
                    "Tallaght University Hospital / St James's Hospital",
                    "Tallaght University Hospital / St James's Hospital",
                    "Cork University Hospital",
                    "Cork University Hospital",
                    "University Hospital Limerick",
                    "Galway University Hospitals"),
      unit_type = c(rep("Renal", 14), rep("Satellite", 9))
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

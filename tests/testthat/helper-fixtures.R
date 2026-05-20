# Synthetic test data frames for dashboard testing.
# These fixtures provide small, deterministic datasets.

#' Create a small test audit dataset
#' @param n Number of patients per centre (default 10).
#' @keywords internal
fixture_audit_data <- function(n = 10) {
  centres <- c("BEA", "MAT", "CUH")
  centre_names <- c("Beaumont Hospital",
                     "Mater Misericordiae University Hospital",
                     "Cork University Hospital")

  rows <- list()
  id <- 0L

  for (i in seq_along(centres)) {
    ids <- seq(id + 1L, id + n)
    id <- id + n

    rows[[i]] <- tibble::tibble(
      record_id   = as.character(ids),
      centre_code = centres[i],
      centre_name = centre_names[i],
      region      = c("Dublin and North East", "Dublin and Midlands", "South")[i],
      unit_code   = centres[i],
      unit_name   = paste0(centre_names[i], " Renal Unit"),
      unit_type   = "Renal",
      consultant  = paste0("Dr. ", centres[i], "_A"),
      is_acute    = rep(FALSE, n),
      age         = seq(40, 40 + n - 1),
      gender      = rep(c("Male", "Female"), length.out = n),
      ethnicity   = rep(c("White Irish", "White Other", NA), length.out = n),
      dxs01       = as.character(rep(c(40, 72, 11), length.out = n)),
      qblg9       = seq(60, 60 + n - 1, length.out = n),
      hdp01       = rep(3L, n),
      hdp02       = rep(240L, n),
      qblg3       = seq(130, 130 + n - 1, length.out = n),
      qblg4       = rep(75, n),
      qblg6       = seq(120, 120 + n - 1, length.out = n),
      qblg7       = rep(70, n),
      qblb1       = seq(1.0, 1.0 + (n - 1) * 0.1, length.out = n),
      qblb4       = rep(2.35, n),
      qblb9       = seq(20, 20 + (n - 1) * 5, length.out = n),
      qbla9       = seq(4.0, 4.0 + (n - 1) * 0.2, length.out = n),
      qbla4       = seq(18, 18 + (n - 1) * 1, length.out = n),
      qble1       = seq(9.0, 9.0 + (n - 1) * 0.3, length.out = n),
      qblf1       = seq(100, 100 + (n - 1) * 30, length.out = n),
      qhd20       = rep(c("AVF", "AVG", "Catheter"), length.out = n)
    )
  }

  dplyr::bind_rows(rows)
}

#' Create minimal audit data with known values for precise testing
#' @keywords internal
fixture_audit_data_minimal <- function() {
  tibble::tibble(
    record_id   = as.character(1:6),
    centre_code = rep(c("BEA", "MAT"), each = 3),
    centre_name = rep(c("Beaumont Hospital", "Mater Hospital"), each = 3),
    region      = rep(c("Dublin and North East", "Dublin and Midlands"), each = 3),
    unit_code   = rep(c("BEA", "MAT"), each = 3),
    unit_name   = rep(c("Beaumont Renal Unit", "Mater Renal Unit"), each = 3),
    unit_type   = rep("Renal", 6),
    consultant  = rep(c("Dr. A", "Dr. B"), each = 3),
    is_acute    = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    age         = c(50, 60, 70, 45, 55, 65),
    gender      = c("Male", "Female", "Male", "Female", "Male", "Female"),
    ethnicity   = c("White Irish", "White Other", NA, "White Irish", "Asian", "White Irish"),
    dxs01       = c("40", "72", "11", "40", "72", "81"),
    qblg9       = c(70, 68, 62, 75, 55, 80),
    hdp01       = c(3L, 3L, 3L, 3L, 2L, 3L),
    hdp02       = c(240L, 240L, 210L, 240L, 180L, 270L),
    qblg3       = c(135, 150, 128, 142, 160, 130),
    qblg4       = c(80, 95, 75, 88, 100, 78),
    qblg6       = c(125, 138, 118, 132, 145, 120),
    qblg7       = c(72, 85, 68, 80, 92, 70),
    qblb1       = c(1.3, 1.9, 1.5, 1.1, 2.1, 1.4),
    qblb4       = c(2.3, 2.6, 2.4, 2.2, 2.1, 2.45),
    qblb9       = c(25, 80, 45, 15, 100, 50),
    qbla9       = c(4.5, 6.2, 5.0, 3.8, 5.5, 4.8),
    qbla4       = c(22, 17, 24, 20, 28, 21),
    qble1       = c(10.5, 9.0, 11.5, 10.0, 12.5, 10.8),
    qblf1       = c(250, 150, 400, 180, 500, 300),
    qhd20       = c("AVF", "Catheter", "AVF", "AVG", "Catheter", "AVF")
  )
}

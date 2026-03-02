# Generate synthetic audit data and save as .rda
# Run this script with: Rscript data-raw/generate-synthetic-data.R

library(ikdds.dashboard)

synthetic_audit_data <- generate_synthetic_data(seed = 42, n_total = 1200)

usethis::use_data(synthetic_audit_data, overwrite = TRUE)

cli::cli_inform(c(

  "v" = "Saved synthetic_audit_data.rda: {nrow(synthetic_audit_data)} patients, {length(unique(synthetic_audit_data$centre_code))} centres."
))

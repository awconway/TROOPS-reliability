library(targets)
library(tarchetypes)

# pak::pkg_install(".")
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c(
  "tidyverse",
  "agreement",
  "TROOPSreliability"
),
imports = c("TROOPSreliability"))

list(
  tar_target(
    ratings,
    readxl::read_xlsx("data/TROOPS-ratings.xlsx")
  ),
  tar_target(
    baseline_data,
    readxl::read_xlsx("data/TROOPS-baseline-data.xlsx")
  ),
  tar_target(icc_total, icc_total(ratings)),
  tar_target(icc_total_tidy, filter(
    tidy(icc_total),
    term == "Inter-Rater ICC"
  )),
  tar_target(icc_total_plot, plot(icc_total,
    intra = FALSE,
    inter = TRUE
  )),
  tar_target(icc_airway, icc_airway(ratings)),
  tar_target(icc_airway_tidy, filter(
    tidy(icc_airway),
    term == "Inter-Rater ICC"
  )),
  tar_target(icc_airway_plot, plot(icc_airway,
    intra = FALSE,
    inter = TRUE
  )),
  tar_target(icc_quality, icc_quality(ratings)),
  tar_target(icc_quality_tidy, filter(
    tidy(icc_quality),
    term == "Inter-Rater ICC"
  )),
  tar_target(icc_quality_plot, plot(icc_quality,
    intra = FALSE,
    inter = TRUE
  ))
)

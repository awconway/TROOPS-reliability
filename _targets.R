library(targets)
library(tarchetypes)

devtools::load_all()
options(tidyverse.quiet = TRUE)

tar_option_set(packages = c(
  "tidyverse"
))

list(
    tar_target(ratings, 
    readxl::read_xlsx('data/TROOPS-ratings.xlsx')),
    tar_target(baseline_data, 
    readxl::read_xlsx('data/TROOPS-baseline-data.xlsx'))

)
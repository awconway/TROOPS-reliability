library(targets)
library(tarchetypes)


tar_option_set(
  packages = c(
    "tidyverse",
    "agreement", # https://github.com/jmgirard/agreement
    "TROOPSreliability"
  ),
  imports = c("TROOPSreliability")
)

list(
  tar_target(
    ratings,
    readxl::read_xlsx("data/TROOPS-ratings.xlsx")
  ),
  tar_target(
    baseline_data,
    readxl::read_xlsx("data/TROOPS-baseline-data.xlsx")
  ),
  tar_target(summary_table, create_summary_table(baseline_data)),
  tar_target(icc_total_results, icc_total(ratings)),
  tar_target(icc_total_tidy, filter(
    tidy(icc_total_results),
    term == "Inter-Rater ICC"
  )),
  tar_target(icc_total_plot, plot(icc_total_results,
    intra = FALSE,
    inter = TRUE
  )),
  tar_target(icc_airway_results, icc_airway(ratings)),
  tar_target(icc_airway_tidy, filter(
    tidy(icc_airway_results),
    term == "Inter-Rater ICC"
  )),
  tar_target(icc_airway_plot, plot(icc_airway_results,
    intra = FALSE,
    inter = TRUE
  )),
  tar_target(time, ratings %>%
    filter(!is.na(`Completion time`)) %>%
    select(ID, Rater, `Completion time`) %>%
    count(`Completion time`)),
  tar_target(events, ratings %>%
    group_by(ID, Rater) %>%
    filter(!is.na(Rating), !is.na(Rater)) %>%
    select(ID, Rater, Rating, Complication) %>%
    pivot_wider(names_from = "Complication", values_from = "Rating") %>%
    unite("total", Airway:`Sedation quality`) %>%
    ungroup() %>%
    mutate(ratings = case_when(
      `total` == "None_None_None_None" ~ 0,
      `total` == "Minor_None_None_None" ~ 1,
      `total` == "None_None_None_Intermediate" ~ 2,
      `total` == "Minor_None_None_Intermediate" ~ 2
    )) %>%
    select(-total) %>%
    pivot_wider(names_from = "Rater", values_from = "ratings") %>%
    mutate(event = ifelse(`1` != 0 | `2` != 0, "event", "no event")) %>%
    count(event)),

  # manuscript
  tar_render(manuscript, "manuscript.Rmd")
)

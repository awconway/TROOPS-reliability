#' ICC for total TROOPS
#' @export
#' @importFrom dplyr group_by filter select ungroup mutate
#' @importFrom tidyr pivot_wider unite
#' @importFrom agreement dim_icc

icc_total <- function(ratings) {
  ratings %>%
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
      `total` == "Minor_None_None_Intermediate" ~ 3
    )) %>%
    select(-total) %>%
    dim_icc(
      model = "1A", type = "agreement", unit = "average",
      object = ID, rater = Rater, score = ratings, warnings = FALSE
    )
}

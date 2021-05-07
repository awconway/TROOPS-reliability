#' ICC for airway domain of TROOPS
#' @export
#' @importFrom dplyr group_by filter select ungroup mutate
#' @importFrom tidyr pivot_wider
#' @importFrom agreement dim_icc

icc_airway <- function(ratings) {
ratings %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
group_by(ID, Rater) %>%
select(ID,Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
  ungroup() %>%
select(ID, Rater, Airway) %>%
  mutate(ratings = ifelse(Airway == "Minor", 1, 0)) %>%
dim_icc(model = "1A", type = "agreement", unit = "average",
                     object = ID, rater = Rater, score = ratings, warnings = FALSE)
}

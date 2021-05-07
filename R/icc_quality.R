#' ICC for quality domain of TROOPS
#' @export
#' @importFrom dplyr group_by filter select ungroup mutate
#' @importFrom tidyr pivot_wider
#' @importFrom agreement dim_icc

icc_quality <- function(ratings) {
ratings %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
group_by(ID, Rater) %>%
select(ID,Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
  ungroup() %>%
select(ID, Rater, `Sedation quality`) %>%
  mutate(ratings = ifelse(`Sedation quality` == "Intermediate", 1, 0)) %>%
dim_icc(model = "1A", type = "agreement", unit = "average",
                     object = ID, rater = Rater, score = ratings)
}

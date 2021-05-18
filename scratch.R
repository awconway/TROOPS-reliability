library(tidyverse)
targets::tar_load(ratings)
ratings %>%
filter(Rating != "None") %>%
group_by(Rater) %>%
count()

library(irr)
# https://github.com/jmgirard/agreement
ratings %>%
group_by(ID, Rater) %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
select(ID, Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
unite("total", Airway:`Sedation quality`) %>%
select(Rater, total) %>%
pivot_wider(names_from = "Rater", values_from = "total") %>%view
ungroup() %>%
mutate(agree = ifelse(`1` == `2`, 1, 0)) %>% 
summarise(total = sum(agree, na.rm = T)/n())

icc_total <- ratings %>%
group_by(ID, Rater) %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
select(ID, Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
unite("total", Airway:`Sedation quality`) %>%
  ungroup() %>%
# select(Rater, total) %>%
mutate(ratings = case_when(`total` == "None_None_None_None" ~ 0,
                           `total` == "Minor_None_None_None" ~ 1,
                           `total` == "None_None_None_Intermediate" ~ 2,
                           `total` == "Minor_None_None_Intermediate" ~ 3 )) %>%
  select(-total) %>%
  agreement::dim_icc(model = "1A", type = "agreement", unit = "average",
          object = ID, rater = Rater, score = ratings, warnings = FALSE)

summary(icc_total)
filter(agreement::tidy(icc_total), term == "Inter-Rater ICC")
agreement::plot(icc_total, intra = FALSE, inter = TRUE)


ratings %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
group_by(ID, Rater) %>%
select(ID,Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
  ungroup() %>%
select(ID, Rater, Airway) %>%
  mutate(ratings = ifelse(Airway == "Minor", 1, 0)) %>%
agreement::dim_icc(model = "1A", type = "agreement", unit = "average",
                     object = ID, rater = Rater, score = ratings, warnings = FALSE) %>%
  agreement::tidy()


ratings %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
group_by(ID, Rater) %>%
select(Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
select(Rater, `Sedation quality`) %>%
pivot_wider(names_from = "Rater", values_from = `Sedation quality`) %>%
ungroup() %>%
transmute(rater1 = ifelse(`1`=="None", 0, 1),
rater2 = ifelse(`2`=='None', 0,1)) %>%
icc( model = "oneway", 
    type = "consistency"
   )


ratings %>%
  filter(!is.na(Rating), !is.na(Rater)) %>%
  group_by(ID, Rater) %>%
  select(Rater, Rating, Complication) %>%
  pivot_wider(names_from = "Complication", values_from = "Rating") %>%
  select(Rater, Airway) %>%
  pivot_wider(names_from = "Rater", values_from = "Airway") %>%
  ungroup() %>%
  transmute(rater1 = ifelse(`1`=="None", 0, 1),
            rater2 = ifelse(`2`=='None', 0,1)) %>%
  mutate(agreement = ifelse(rater1 == rater2, 1,0)) %>%
  summarise(total = sum(agreement)/n())

ratings %>%
  filter(!is.na(Rating), !is.na(Rater)) %>%
  group_by(ID, Rater) %>%
  select(Rater, Rating, Complication) %>%
  pivot_wider(names_from = "Complication", values_from = "Rating") %>%
  select(Rater, `Sedation quality`) %>%
  pivot_wider(names_from = "Rater", values_from = `Sedation quality`) %>%
  ungroup() %>%
  transmute(rater1 = ifelse(`1`=="None", 0, 1),
            rater2 = ifelse(`2`=='None', 0,1)) %>%
  mutate(agreement = ifelse(rater1 == rater2, 1,0)) %>%
  summarise(total = sum(agreement, na.rm = T)/n())


ratings  %>%
  filter(!is.na(`Completion time`)) %>%
  select(ID, Rater, `Completion time`) %>%
  count(`Completion time`)

targets::tar_load(baseline_data)

baseline_data %>%view


targets::tar_load(ratings)
ratings %>%
  filter(!is.na(Rating), !is.na(Rater)) %>%
filter(Rating == "Minor" | Rating == "Intermediate") %>%
summarise(etiology_present = sum(!is.na(Etiology))/n())

targets::tar_load(ratings)
ratings %>%
  filter(!is.na(Rating), !is.na(Rater)) %>%
  filter(Complication == "Airway") %>%
filter(Rating == "Minor" | Rating == "Intermediate") %>%
summarise(
  desat = sum(!is.na(Description1))/n(),
  oxygen_inc = sum(!is.na(Description2))/n(),
  airway = sum(!is.na(Description3))/n(),
  tactile_stim = sum(!is.na(Description4))/n(),
)

ratings %>%
  filter(!is.na(Rating), !is.na(Rater)) %>%
filter(Rating == "Minor" | Rating == "Intermediate") %>%
  filter(Complication == "Airway") %>%
summarise(
  
)


# either nurse selected at least one event
events <- ratings %>%
group_by(ID, Rater) %>%
filter(!is.na(Rating), !is.na(Rater)) %>%
select(ID, Rater, Rating, Complication) %>%
pivot_wider(names_from = "Complication", values_from = "Rating") %>%
unite("total", Airway:`Sedation quality`) %>%
  ungroup() %>%
# select(Rater, total) %>%
mutate(ratings = case_when(`total` == "None_None_None_None" ~ 0,
                           `total` == "Minor_None_None_None" ~ 1,
                           `total` == "None_None_None_Intermediate" ~ 2,
                           `total` == "Minor_None_None_Intermediate" ~ 3 )) %>%
  select(-total) %>%
  pivot_wider(names_from = "Rater", values_from = "ratings") %>%
  mutate(event = ifelse(`1`!=0 | `2` !=0, "event", "no event")) %>%
  count(event) 

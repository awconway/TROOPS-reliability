library(tidyverse)
ratings %>%
filter(Rating != "None") %>%
group_by(Rater) %>%
count()


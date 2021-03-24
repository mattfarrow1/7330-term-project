# Load library
library(tidyverse)
library(stringi)
library(data.table)

# Load data
load(here::here("data - output", "jeopardy.RData"))

# Join together
doubles_has_scores <- left_join(players_has_episodes, doubles_score, by=c("first" = "name","gameid"))

#remove unnecessary columns
doubles_has_scores <- doubles_has_scores %>%
  select("playerid", "clueid", "score")

#check how many NA's
summary(doubles_has_scores)
summary(doubles)

#drop na's
doubles_has_scores <- drop_na(doubles_has_scores)

# Save file
write_csv(doubles_has_scores, here::here("data - output", "doubles_has_scores.csv"))

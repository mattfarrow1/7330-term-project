
# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidytext)

# Load data
load(here::here("data - output", "jeopardy.RData"))

# Data Preparation --------------------------------------------------------

# Break out categories
categories <- df_board %>% 
  select(category, uid) %>% 
  distinct()

# Break out clues
clues <- df_board %>% 
  select(clue, uid) %>% 
  distinct()

# Break out answers
answers <- df_board %>% 
  select(answer, uid) %>% 
  distinct()

# Break out occupations
occupations <- as_tibble(unlist(strsplit(df_players$occupation, "\n|,|, ")))
occupations <- as_tibble(unlist(strsplit(occupations$value, " And ")))
occupations <- as_tibble(str_remove_all(occupations$value, "An "))

# Break out locations
locations <- str_split_fixed(df_players$from, ", ", 2)
locations <- tibble("City" = locations[,1],
                    "State" = locations[,2])
locations <- locations %>%
  mutate("ST" = state.abb[match(locations$State, state.name)])
locations$ST[is.na(locations$ST)] <- "DC"

# Text Analysis -----------------------------------------------------------

# Top 10 Categories
categories %>% 
  count(category) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(games = n)

# Top 10 Answers
answers %>% 
  count(answer) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(games = n)

# Top 10 Occupations
occupations %>% 
  count(value) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(players = n)

# Top 10 States
locations %>% 
  count(ST) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(players = n)

# Top 10 City/ST Combos
locations %>% 
  count(City, ST) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(players = n)

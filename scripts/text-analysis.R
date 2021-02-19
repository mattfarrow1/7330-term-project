
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

# Text Analysis -----------------------------------------------------------

# Top 10 categories
categories %>% 
  count(category) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(games = n)

# Bottom 10 categories
categories %>% 
  count(category) %>% 
  arrange(desc(n)) %>% 
  slice_tail(n = 10) %>% 
  rename(games = n)

# Top 10 answers
answers %>% 
  count(answer) %>% 
  arrange(desc(n)) %>% 
  slice_head(n = 10) %>% 
  rename(games = n)

# Bottom 10 answers
answers %>% 
  count(answer) %>% 
  arrange(desc(n)) %>% 
  slice_tail(n = 10) %>% 
  rename(games = n)

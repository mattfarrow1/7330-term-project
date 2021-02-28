# Load library
library(tidyverse)

# Load data
load(here::here("data - output", "jeopardy.RData"))

# Define $ values of clues
clue_values <- tibble(round = c(rep(1, 5), rep(2, 5)),
                      row = c(rep(1:5, 2)),
                      value = c(100, 200, 300, 400, 500 ,200, 400, 600, 800, 1000))

# Merge into board
board_clean <- left_join(board, clue_values, by = c("round" = "round", "row" = "row"))

# Find games and clues that are daily doubles
doubles <- doubles %>% 
  select(uid, i) %>% 
  mutate(double = 1)

# Merge into board & replace blanks with 0
board_clean <- left_join(board_clean, doubles, by = c("uid", "i"))
board_clean$double[is.na(board_clean$double)] <- 0

# Rename columns
board_clean <- board_clean %>% 
  rename("gameid" = uid,
         "chosen" = i)

# Define location as counting 1:30 as reading left-right, top-bottom
location <- tibble(row = c(rep(1, 6),
                           rep(2, 6),
                           rep(3, 6),
                           rep(4, 6),
                           rep(5, 6)),
                   col = c(rep(1:6, 5)),
                   location = c(1:30))

# Merge into board
board_clean <- left_join(board_clean, location, by = c("row" = "row", "col" = "col"))

# Save file
write_csv(board_clean, here::here("data - output", "board_for_sql.csv"))

# Load library
library(tidyverse)
library(stringi)
library(data.table)

# Load data
load(here::here("data - output", "jeopardy.RData"))

# Define $ values of clues
clue_values <- tibble(round = c(rep(1, 5), rep(2, 5)),
                      row = c(rep(1:5, 2)),
                      value = c(100, 200, 300, 400, 500 ,200, 400, 600, 800, 1000))

# Merge into board
board_clean <- left_join(board, clue_values, by = c("round" = "round", "row" = "row"))

#create table for just doubles including score
doubles_score <- doubles %>%
  select(uid, i, name, score) %>%
  mutate(double = 1)

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

doubles_score <- doubles_score %>%
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

# Create clueid
board_clean <- board_clean %>% 
  mutate(clueid = row_number())

#join clueid to doubles table
doubles_score <- merge(doubles_score,board_clean,by=c("gameid","chosen","double"))

#remove unnecessary columns
doubles_score <- doubles_score %>%
  select("clueid","gameid","name", "score")


# Organize columns
board_clean <- board_clean %>%
  select(clueid,
         round,
         chosen,
         category,
         clue,
         answer,
         value,
         double,
         gameid,
         row,
         col,
         location)

#update to ASCII (remove accents)
board_clean <- as.data.table(board_clean)
board_clean[, answer := iconv(answer,to = 'ASCII//TRANSLIT')]

#check that it worked
board_clean[54]

board_clean[, category := iconv(category,to = 'ASCII//TRANSLIT')]
board_clean[, clue := iconv(clue,to = 'ASCII//TRANSLIT')]

# Save file
write_csv(board_clean, here::here("data - output", "board_for_sql.csv"))

#save file without location
board_sql2 <- board_clean[,0:9]

# Save file
write_csv(board_sql2, here::here("data - output", "board_for_sql2.csv"))

# Save file
write_csv(doubles_score, here::here("data - output", "doubles_score.csv"))

#see doubles_has_scores.R for double scores join to playerid

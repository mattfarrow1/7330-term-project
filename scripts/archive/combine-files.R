
# Setup -------------------------------------------------------------------

library(tidyverse)

# Read CSV Files ----------------------------------------------------------

# Get names of all CSV files
csv_file_names <- here::here("data - raw", "games") %>% 
  list.files() %>% 
  .[str_detect(., ".csv")]

# Read into R
csv_file_names %>%
  purrr::map(function(file_name) {
    # iterate through each file name
    assign(
      x = str_remove(file_name, ".csv"),
      # Remove file extension ".csv"
      value = read_csv(paste0(
        here::here("data - raw", "games"), "/", file_name
      )),
      envir = .GlobalEnv
    )
  })

# Read Excel file ---------------------------------------------------------

library(readxl)

board_1501_6000 <- read_excel("data - raw/games/board_1501_6000.xlsx")

# Combine Files Together --------------------------------------------------

airdate <- bind_rows(airdate,
                        airdate_501_1500,
                        airdate_1501_6000,
                        airdate_6001_6948)

board <- bind_rows(board,
                      board_501_1500,
                      board_1501_6000,
                      board_6001_6948)

doubles <- bind_rows(doubles,
                        doubles_501_1500,
                        doubles_1501_6000,
                        doubles_6001_6948)

players <- bind_rows(players,
                        players_501_1500,
                        players_1501_6000)

scores <- bind_rows(scores,
                       scores_501_1500,
                       scores_1501_6000,
                       scores_6001_6948)

synopsis <- bind_rows(synopsis,
                         synopsis_501_1500,
                         synopsis_1501_6000,
                         synopsis_6001_6948)

# Save Files --------------------------------------------------------------

write_csv(airdate, here::here("data - output", "airdate_0001_6948.csv"))
write_csv(board, here::here("data - output", "board_0001_6948.csv"))
write_csv(doubles, here::here("data - output", "doubles_0001_6948.csv"))
write_csv(scores, here::here("data - output", "scores_0001_6948.csv"))
write_csv(synopsis, here::here("data - output", "synopsis_0001_6948.csv"))

# Save RData --------------------------------------------------------------

load(here::here("data - output", "game_contestants.RData"))
load(here::here("data - output", "player_ids.RData"))

# Everything
save(airdate,
     board,
     doubles,
     game_contestants,
     player_ids,
     scores,
     synopsis,
     file = here::here("data - output", "jeopardy.RData"))

# Individual Files
save(airdate, file = here::here("data - output", "airdate.RData"))
save(board, file = here::here("data - output", "board.RData"))
save(doubles, file = here::here("data - output", "doubles.RData"))
save(scores, file = here::here("data - output", "scores.RData"))
save(synopsis, file = here::here("data - output", "synopsis.RData"))

# Load Data ---------------------------------------------------------------

load(here::here("data - output", "jeopardy.RData"))

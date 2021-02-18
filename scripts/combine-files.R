
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

df_airdate <- bind_rows(airdate,
                        airdate_501_1500,
                        airdate_1501_6000)

df_board <- bind_rows(board,
                      board_501_1500,
                      board_1501_6000)

df_doubles <- bind_rows(doubles,
                        doubles_501_1500,
                        doubles_1501_6000)

df_players <- bind_rows(players,
                        players_501_1500,
                        players_1501_6000)

df_scores <- bind_rows(scores,
                       scores_501_1500,
                       scores_1501_6000)

df_synopsis <- bind_rows(synopsis,
                         synopsis_501_1500,
                         synopsis_1501_6000)

# Save Files --------------------------------------------------------------

write_csv(df_airdate, here::here("data - output", "airdate_0001_6000.csv"))
write_csv(df_board, here::here("data - output", "board_0001_6000.csv"))
write_csv(df_doubles, here::here("data - output", "doubles_0001_6000.csv"))
write_csv(df_scores, here::here("data - output", "scores_0001_6000.csv"))
write_csv(df_synopsis, here::here("data - output", "synopsis_0001_6000.csv"))

# Save RData --------------------------------------------------------------

# Everything
save(df_airdate,
     df_board,
     df_doubles,
     df_players,
     df_scores,
     df_synopsis,
     file = here::here("data - output", "jeopardy.RData"))

# Individual Files
save(df_airdate, file = here::here("data - output", "airdate.RData"))
save(df_board, file = here::here("data - output", "board.RData"))
save(df_doubles, file = here::here("data - output", "doubles.RData"))
save(df_players, file = here::here("data - output", "players.RData"))
save(df_scores, file = here::here("data - output", "scores.RData"))
save(df_synopsis, file = here::here("data - output", "synopsis.RData"))

# Load Data ---------------------------------------------------------------

load(here::here("data - output", "jeopardy.RData"))

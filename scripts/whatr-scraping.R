library(tidyverse)
library(whatr)


get_game_details <- function(id) {
  game_details <- lst(
    # whatr_plot(game = id),
    whatr::whatr_airdate(game = id),
    whatr::whatr_board(game = id),
    whatr::whatr_scores(game = id),
    whatr::whatr_players(game = id),
    whatr::whatr_synopsis(game = id),
    whatr_doubles(game = id)
  )
}

df <- map(6000:6010, get_game_details)



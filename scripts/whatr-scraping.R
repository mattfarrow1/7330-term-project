library(tidyverse)
library(whatr)


get_game_details <- function(id) {
  game_details <- lst(
    # plot = whatr_plot(game = id),
    airdate = whatr::whatr_airdate(game = id),
    board = whatr::whatr_board(game = id),
    scores = whatr::whatr_scores(game = id),
    players = whatr::whatr_players(game = id),
    synopsis = whatr::whatr_synopsis(game = id),
    doubles = whatr_doubles(game = id)
  )
}

df <- map(6001:6010, get_game_details)



library(tidyverse)
library(whatr)
library(data.table)


get_game_details <- function(id) { 
  game_details <- lst(
    airdate = whatr::whatr_airdate(game = id),
    board = whatr::whatr_board(game = id),
    scores = whatr::whatr_scores(game = id),
    players = whatr::whatr_players(game = id),
    synopsis = whatr::whatr_synopsis(game = id),
    doubles = whatr_doubles(game = id)
  )
}

df <- map(1:1000, get_game_details)

stupid_function <- function(game_list) {
  airdate_df <- NULL
  board_df <- NULL
  scores_df <- NULL
  players_df <- NULL
  synopsis_df <- NULL
  doubles_df <- NULL
  for(i in 1:length(game_list)) {
    uid <- game_list[[i]][['airdate']]$game
    airdate <- as.data.frame(game_list[[i]][['airdate']])
    airdate_df <- rbind(airdate_df, airdate)
    board <- as.data.frame(game_list[[i]][['board']])
    board$uid <- uid
    board_df <- rbind(board_df, board)
    scores <- as.data.frame(game_list[[i]][['scores']])
    scores$uid <- uid
    scores_df <- rbind(scores_df, scores)
    players <- as.data.frame(game_list[[i]][['players']])
    players$uid <- uid
    players_df <- rbind(players_df, players)
    synopsis <- as.data.frame(game_list[[i]][['synopsis']])
    synopsis$uid <- uid
    synopsis_df <- rbind(synopsis_df, synopsis)
    doubles <- as.data.frame(game_list[[i]][['doubles']])
    doubles$uid <- uid
    doubles_df <- rbind(doubles_df, doubles)
  }
  return(lst(airdate_df, board_df, scores_df, players_df, synopsis_df, doubles_df))
}

tables <- stupid_function(df)

write.csv(tables[[1]], 'airdate.csv', row.names = FALSE)
write.csv(tables[[2]], 'board.csv', row.names = FALSE)
write.csv(tables[[3]], 'scores.csv', row.names = FALSE)
write.csv(tables[[4]], 'players.csv', row.names = FALSE)
write.csv(tables[[5]], 'synopsis.csv', row.names = FALSE)
write.csv(tables[[6]], 'doubles.csv', row.names = FALSE)
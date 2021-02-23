packageurl <- "https://cran.r-project.org/src/contrib/Archive/whatr/whatr_1.0.0.tar.gz"
install.packages(packageurl, repos=NULL, type="source")
library(whatr)
library(data.table)
library(tidyverse)

get_game_details <- function(id) { 
  tryCatch({
    game_details <- lst(
      airdate = whatr::whatr_airdate(game = id),
      board = whatr::whatr_board(game = id),
      scores = whatr::whatr_scores(game = id),
      players = whatr::whatr_players(game = id),
      synopsis = whatr::whatr_synopsis(game = id),
      doubles = whatr_doubles(game = id)
    )}, error=function(e){})
}

df <- map(1:500, get_game_details)

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

airdateData <- data.frame(tables[1])
boardData <- data.frame(tables[2])

colnames(airdateData) <- c("gameid", "showid", "air_date")
colnames(boardData) <- c("round", "clueid", "dollarAmountid", "when_chosen", "category_desc", "clue_desc", "answer", "gameid")


write.csv(airdateData, 'airdate.csv', row.names = FALSE)
write.csv(boardData, 'board2.csv', row.names = FALSE)
write.csv(tables[[3]], 'scores.csv', row.names = FALSE)
write.csv(tables[[4]], 'players.csv', row.names = FALSE)
write.csv(tables[[5]], 'synopsis.csv', row.names = FALSE)
write.csv(tables[[6]], 'doubles.csv', row.names = FALSE)


install.packages("RMySQL")
install.packages("RODBC")
library(RMySQL)
library(RODBC)

#Establishing the Connection to the Database
databaseConnection <- dbConnect(MySQL(),user = 'root', password = 'Dannysboy092016!', host = 'localhost')

#Creating the Schema in mySQL Workbench
queryCreateDatabase <- "CREATE DATABASE jeopardy"
results <- dbSendQuery(databaseConnection, queryCreateDatabase)
dbClearResult(results)

#Establishing connection to the jeopardy database
databaseConnection <- dbConnect(MySQL(),user = 'root', password = 'Dannysboy092016!', host = 'localhost', dbname = 'jeopardy')

#Creating the Tables based off the csv files
#Airdates Table
queryCreateTableAirdates <- "CREATE TABLE airdates(
  gameid INT,
  showid INT,
  air_date DATE)"

resultsAirdates <- dbSendQuery(databaseConnection, queryCreateTableAirdates)
dbClearResult(resultsAirdates)

dbSendQuery( databaseConnection, "LOAD DATA LOCAL INFILE '/path/to/airdates.csv'
                                              INTO TABLE episode
                                              FIELDS TERMINATED by ','
                                              ENCLOSED BY '"'
                                              LINES TERMINATED BY '\\n'")

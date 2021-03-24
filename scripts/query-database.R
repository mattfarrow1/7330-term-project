
# Setup -------------------------------------------------------------------

library(tidyverse)
library(RMariaDB)
library(DBI)
library(dbplyr)


# Create connection
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "jeopardy",
                 user = "root",
                 password = ""
                 )

# Examples ----------------------------------------------------------------

# List tables
dbListTables(con)

# List columns
dbListFields(con, "board")

# Read table
dbReadTable(con, "episode") %>% 
  head()

# SQL Query: Categories ---------------------------------------------------

# Get the top 10 categories
res <- dbSendQuery(con, "SELECT category, count(*) AS games
FROM board
GROUP BY category
ORDER BY games DESC
LIMIT 10;")
top_cat <- dbFetch(res, n = Inf)
dbClearResult(res)

# SQL Query: Double Jeopardy ----------------------------------------------

# Double Jeopardy locations
res <- dbSendQuery(
  con,
  "SELECT subquery.rowcat, subquery.colcat, count(*)
  FROM
    (
      SELECT board.clueid, board.doublejeop, location.rowcat, location.colcat
      FROM location
      LEFT JOIN board
      ON board.clueid = location.board_clueid
      WHERE board.doublejeop = 1
    ) AS subquery
  GROUP BY subquery.rowcat, subquery.colcat
  ORDER BY count(*) DESC;"
)
dj_loc <- dbFetch(res, n = Inf)
dbClearResult(res)

# Everyone who won Double Jeopardy
res <- dbSendQuery(
  con,
  "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1;"
)
dj_who <- dbFetch(res, n = Inf)
dbClearResult(res)

# Top Double Jeopardy contestants
res <- dbSendQuery(
  con,
  "select count(board.clueid) as double_jeop_count, players.playerid, players.firstname, players.lastname
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1
  GROUP BY playerid
  order by double_jeop_count desc
  limit 10;"
)
dj_top <- dbFetch(res, n = Inf)
dbClearResult(res)

# SQL Query: Players ------------------------------------------------------

# Top 10 notable player stats
res <- dbSendQuery(
  con,
  "select players.firstname, players.lastname, round(avg(finalscore),1) AS average, max(finalscore) as max_score, round(avg(ansRight)) as avg_correct, round(avg(ansWrong)) as avg_incorrect, sum(ansRight) as total_correct, sum(ansWrong) as total_incorrect, count(*) as total_games
  from synopsis_has_players
  INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
  INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
  group by playerid
  order by total_correct desc
  limit 10;"
)
top10_stats <- dbFetch(res, n = Inf)
dbClearResult(res)

# All player info for text analysis
res <- dbSendQuery(
  con,
  "select * from players;"
)
players <- dbFetch(res, n = Inf)
dbClearResult(res)

# All game player info to perform winner analysis in R
res <- dbSendQuery(
  con,
  "select finalscore, episode_gameid, finalscoreid, ansRight, ansWrong, players.firstname, players.lastname, players_playerid
  from synopsis
  inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
  inner join players on players.playerid = synopsis_has_players.players_playerid;"
)
winners <- dbFetch(res, n = Inf)
dbClearResult(res)

# SQL Query: Daily Doubles ------------------------------------------------

# Daily Double clue info for text analysis joined to who answered it
res <- dbSendQuery(
  con,
  "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1;"
)
daily_doubles <- dbFetch(res, n = Inf)
dbClearResult(res)

# Cleanup -----------------------------------------------------------------

# Disconnect from the database
dbDisconnect(con)

# Alternate Connection Method: dbplyr -------------------------------------

# df <- tbl(con, "board")
# df
# 
# summary <- df %>%
#   count(category) %>% 
#   rename("games" = n) %>% 
#   arrange(desc(games))
# 
# # see query
# summary %>% show_query()
# 
# # execute query and retrieve results
# summary %>% collect()
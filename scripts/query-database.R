
# Setup -------------------------------------------------------------------

library(tidyverse)
library(RMariaDB)
library(DBI)
library(dbplyr)


# Create connection
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "jeopardy",
                 user = "root",
                 password = "***"
                 )
#

# Examples ----------------------------------------------------------------

# List tables
dbListTables(con)

# List columns
dbListFields(con, "board")

# Read table
dbReadTable(con, "episode") %>% 
  head()

# Run Queries -------------------------------------------------------------

# Get the top 10 categories
res <- dbSendQuery(con, "SELECT category, count(*) AS games
FROM board
GROUP BY category
ORDER BY games DESC
LIMIT 10;")
dbFetch(res)
dbClearResult(res)

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

#List of everyone who won double jeopardy
who_db <- dbSendQuery(
  con,
  "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1;"
)

#top double jeopardy contestants
top <- dbSendQuery(
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

#top 10 notable player stats
top10_stats <- dbSendQuery(
  con,
  "select players.firstname, players.lastname, round(avg(finalscore),1) AS average, max(finalscore) as max_score, round(avg(ansRight)) as avg_correct, round(avg(ansWrong)) as avg_incorrect, sum(ansRight) as total_correct, sum(ansWrong) as total_incorrect, count(*) as total_games
  from synopsis_has_players
  INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
  INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
  group by playerid
  order by total_correct desc
  limit 10;"
)

#pull all player info for text analysis
players <- dbSendQuery(
  con,
  "select * from players;"
)

#daily double clue info for text analysis joined to who answered it
daily_doubles <- dbSendQuery(
  con,
  "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1;"
)

#get all game player info to perform winner analysis in R
winners <- dbSendQuery(
  con,
  "select finalscore, episode_gameid, finalscoreid, ansRight, ansWrong, players.firstname, players.lastname, players_playerid
  from synopsis
  inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
  inner join players on players.playerid = synopsis_has_players.players_playerid;"
)

# Save results
doubles <- dbFetch(res, n = Inf)
who_db <- dbFetch(who_db, n = Inf)
top <- dbFetch(top, n = Inf)
top10_stats <- dbFetch(top10_stats, n=Inf)
all_players <- dbFetch(players, n=Inf)
daily_d <- dbFetch(daily_doubles, n=Inf)
winners <- dbFetch(winners, n=Inf)


# Clear results
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)

# Using dbplyr ------------------------------------------------------------

df <- tbl(con, "board")
df

summary <- df %>%
  count(category) %>% 
  rename("games" = n) %>% 
  arrange(desc(games))

# see query
summary %>% show_query()

# execute query and retrieve results
summary %>% collect()

# Doubles Locations -------------------------------------------------------

# Convert doubles to tibble
doubles <- as_tibble(doubles)

# Define location as counting 1:30 as reading left-right, top-bottom
location <- tibble(row = c(rep(1, 6),
                           rep(2, 6),
                           rep(3, 6),
                           rep(4, 6),
                           rep(5, 6)),
                   col = c(rep(1:6, 5)),
                   location = c(1:30))

# Merge into board
doubles <- left_join(doubles, location, by = c("rowcat" = "row", "colcat" = "col"))

# Clean up
doubles <- doubles %>% 
  rename("row" = rowcat,
         "column" = colcat,
         "instances" = `count(*)`) %>% 
  select(location, instances, row, column) %>% 
  arrange(location)

doubles %>% 
  ggplot(aes(as.character(location), instances)) +
  geom_col()

# Top 10 Daily Double count ----------

#create new column combining first and last name
top <- top %>%
  mutate(name = paste(firstname,lastname))

#change double_jeop_count to numeric
top$double_jeop_count <- as.numeric(top$double_jeop_count)

top %>%
  arrange(double_jeop_count) %>%
  ggplot(aes(x = reorder(name, double_jeop_count), y = double_jeop_count)) +
  geom_col(fill="darkblue") +
  coord_flip() +
  ylab("Career Daily Double Count") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        panel.background = element_blank(),
        plot.title = element_text(size = 15)) +
  ggtitle("Who Got the Most Daily Double Clues?")

# Daily Double Wagers ----------


# Top Daily Double Categories --------

# Determine who won each game -----



  


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

#for each top 10 player, where did they select the double jeopardy the most?
top10_dj <- dbSendQuery(
  con,
  "select count(board.clueid) as loc_count, players.playerid, players.firstname, players.lastname, location.rowcat, location.colcat
  from board
  INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
  INNER JOIN location on board.clueid = location.board_clueid
  INNER JOIN players on players.playerid = doubles_has_scores.playerid
  where doublejeop = 1 and players.playerid in (1, 12600, 861, 12824, 9037, 10171, 11663, 8885, 10911, 8522)
  group by playerid, rowcat, colcat
  order by playerid;"
)

# Save results
doubles <- dbFetch(res, n = Inf)
who_db <- dbFetch(who_db, n = Inf)
top <- dbFetch(top, n = Inf)
top10_dj <- dbFetch(top10_dj, n = Inf)

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

# Top 10 Double Jeopardy count ----------

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
  xlab("Contestant") +
  ylab("Career Double Jeopardy Count") +
  theme_light() +
  ggtitle("Who Got the Most Double Jeopardy Clues?")


  

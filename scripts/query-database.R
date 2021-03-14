
# Setup -------------------------------------------------------------------

library(tidyverse)
library(RMariaDB)
library(DBI)
library(dbplyr)

# Create connection
con <- dbConnect(RMariaDB::MariaDB(), dbname = "jeopardy")

# Examples ----------------------------------------------------------------

# List tables
dbListTables(con)

# List columns
dbListFields(con, "board")

# Read table
dbReadTable(con, "episode")

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
dbFetch(res)
dbClearResult(res)

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

# End Session -------------------------------------------------------------

# Clear the result
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)

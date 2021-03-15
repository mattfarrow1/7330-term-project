
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

# Save results
doubles <- dbFetch(res, n = Inf)

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

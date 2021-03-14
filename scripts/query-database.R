
# Setup -------------------------------------------------------------------

library(tidyverse)
library(RMariaDB)
library(DBI)

# Create connection
con <- dbConnect(RMariaDB::MariaDB(), dbname = "jeopardy")

# Examples ----------------------------------------------------------------

# List tables
dbListTables(con)

# List columns
dbListFields(con, "board")

# Read table
dbReadTable(con, "episode")

# Run Query ---------------------------------------------------------------

res <- dbSendQuery(con, 
                   "SELECT category, count(*) AS games
                   FROM board
                   GROUP BY category
                   ORDER BY games DESC
                   LIMIT 10;"
                   )
dbFetch(res)
dbClearResult(res)

# End Session -------------------------------------------------------------

# Clear the result
dbClearResult(res)

# Disconnect from the database
dbDisconnect(con)

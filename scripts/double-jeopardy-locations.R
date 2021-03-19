
# Setup -------------------------------------------------------------------

library(tidyverse)
library(RMariaDB)
library(DBI)
library(dbplyr)
library(hrbrthemes)
library(scales)

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

# Run Query ---------------------------------------------------------------

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

# Doubles Locations -------------------------------------------------------

# Convert doubles to tibble
doubles <- as_tibble(doubles)

# Clean up
doubles <- doubles %>% 
  rename("row" = rowcat,
         "category" = colcat,
         "instances" = `count(*)`) %>% 
  # mutate(pct = instances / 395350)  # number of clues in rounds 1 & 2
  # mutate(pct = instances / 6775)    # number of games
  mutate(pct = instances / sum(instances))

# Plot --------------------------------------------------------------------

doubles %>%
  # ggplot(aes(category, row, fill = pct, label = percent(round(pct, digits = 1)))) +
  ggplot(aes(category, row, fill = pct, label = percent(pct, accuracy = 0.01))) +
  geom_tile() +
  geom_text(color = "white", size = rel(3.5)) +
  labs(title = "Where are you likely to find a Daily Double?",
       x = "",
       y = "",
       fill = "") +
  scale_x_continuous(
    breaks = c(1:6),
    labels = c(
      "Category 1",
      "Category 2",
      "Category 3",
      "Category 4",
      "Category 5",
      "Category 6"
    ),
    position = "top"
  ) +
  scale_y_reverse(
    breaks = c(1:5),
    labels = c(
      "$100/$200",
      "$200/$400",
      "$300/$600",
      "$400/$800",
      "$500/$1000"
    )
  ) +
  theme_ipsum() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    
    axis.text = element_text(face = "bold"),
    legend.position = ""
  )
library(shiny)
library(shinydashboard)
library(ggplot2)
library(magrittr)
library(tidyverse)
library(RMariaDB)
library(DBI)
library(dbplyr)
library(RODBC)
library(dplyr)
library(DT)


ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "JEOPARDY! Insights"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text = "Notable Player Analysis", tabName = "Players"),
                        menuItem(text = "Categories and Clues", tabName = "CE"),
                        menuItem(text = "Daily Double Exploration", tabName = "DDE")
                        #menuItem(text = "The Best and the Worst", tabName = "TBTW")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Players",
                                box(plotOutput("playerstatsBar"), width = 8),
                                box(
                                  selectInput("features", "Features:",
                                              c("average", "max_score", "avg_correct", "total_correct", "total_incorrect", "total_games")), width = 4),
                                box(plotOutput("playerdailydouble"), width = 6)),
                        tabItem(tabName = "CE",
                                box(tags$img(src = "category_wordplot.png", height = '400', width = '650'), width = 6),
                                box(tags$img(src = "clue_worldcloud.png", height = '400', width = '650'), width = 6),
                                box(tags$img(src = "top-10-categories.png", height = '400', width = '650'), width = 6),
                                box(tags$img(src = "answer_wordcloud.png", height = '400', width = '650'), width = 6)),
                        tabItem(tabName = "DDE",
                                box(tags$img(src = "daily-double-answers.png"), width = 6),
                                box(tags$img(src = "daily-double-locations.png", height = '400', width = '500'), width = 6),
                                box(tags$img(src = "daily-double-wordcloud.png"), width = 6)
                                
                      )
                    )
))

#Establish connection to DB
con <- dbConnect(RMariaDB::MariaDB(),
                 dbname = "jeopardy",
                 user = "",
                 password = ""
)

#Running Queries and Storing OUtput into data frames

#### NOTABLE PLAYERS ANALYSIS #####
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
            top10_stats_df <- dbFetch(top10_stats)

            
#### CATEGORY EVALUATION #########
            #Get the top 10 Categories
            
            #Common values first chosen in a game?
            top_placement <- dbSendQuery(
              con, 
              "SELECT score, count(*) AS games
              FROM board
              WHERE chosen = 1
              GROUP BY score
              ORDER BY score
              LIMIT 10;"
            )
            top_placement_df <- dbFetch(top_placement)
          
            
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

top <- dbFetch(top)
# Top 10 Daily Double count ----------

#create new column combining first and last name
top <- top %>%
  mutate(name = paste(firstname,lastname))

#change double_jeop_count to numeric
top$double_jeop_count <- as.numeric(top$double_jeop_count)

#DOUBLE JEOPARDY

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
doubles <- dbFetch(res, n = Inf)

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





server <-  function(input,output) {
  
  output$playerstatsBar <- renderPlot({
    ggplot(data = top10_stats_df, aes(x = lastname, y = input$features)) + geom_bar(stat = "identity") + ggtitle("Top 10 Players Stats")
  })
  output$playerdailydouble <- renderPlot({
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
      ggtitle("Who Has the Most Daily Double Clues?")
  })
  output$doublelocation <- renderPlot({
    doubles %>% 
      ggplot(aes(as.character(location), instances)) +
      geom_col()
  })
}

shinyApp(ui, server)

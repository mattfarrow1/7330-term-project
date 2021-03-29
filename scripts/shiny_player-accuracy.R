# Setup -------------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinydashboard)
library(RMariaDB)
library(DBI)
library(dbplyr)
library(scales)

ui <- fluidPage(
  # Page title
  titlePanel("Top 10 Player Accuracy"),
  
  # Define the page with a sidebar and main panel
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      # Create drop-down to select variable
      checkboxGroupInput(inputId = "player", label = h3("Choose a Player:"),
                  choices = c("Arthur Chu"        = "Chu",
                              "Julia Collins"     = "Collins",
                              "James Holzhauer"   = "Holzhauer",
                              "Matt Jackson"      = "Jackson",
                              "Ken Jennings"      = "Jennings",
                              "Jason Keller"      = "Keller",
                              "David Madden"      = "Madden",
                              "Austin Rogers"     = "Rogers",
                              "Seth Wilson"       = "Wilson",
                              "Jason Zuffranieri" = "Zuffranieri"
                  )
      )
    ),
    mainPanel(plotOutput(outputId = "playerplot", width = "500px", height = "500px"))
  )
)

server <- function(input, output) {
  
  # Create connection
  con <- dbConnect(RMariaDB::MariaDB(),
                   dbname = "jeopardy",
                   user = "root",
                   password = ""
  )
  
  # # Set SQL Session Options
  # dbSendQuery(
  #   con,
  #   "SET SESSION sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''));")
  # dbClearResult()
  # 
  # Top 10 players' game accuracy
    res <- dbSendQuery(
    con,
    "SELECT 	players.playerid,
		players.firstname, 
		players.lastname,
        episode.gameid,
        episode.airdate,
        sum(ansRight) as total_correct, 
        sum(ansWrong) as total_incorrect, 
        (ansRight + ansWrong) as total_answers,
        (ansRight / (ansRight + ansWrong)) as total_accuracy
  FROM synopsis_has_players
  INNER JOIN synopsis on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
  INNER JOIN players on synopsis_has_players.players_playerid = players.playerid
  INNER JOIN episode on synopsis.episode_gameid = episode.gameid
  WHERE players.playerid in (1, 12600, 12824, 9037, 861, 10171, 11663, 8885, 10911, 7606)
  GROUP BY gameid;"
  )
  player_accuracy <- dbFetch(res, n = Inf)
  dbClearResult(res)
  
  # Disconnect from the database
  dbDisconnect(con)
  
  # Add ID Column
  player_accuracy <- player_accuracy %>% 
    group_by(lastname) %>% 
    mutate(id = row_number()) %>% 
    ungroup()
  
  # Make Reactive & Filter Based on Input
  data <- reactive({
    req(input$player)
    df <- player_accuracy %>%
      filter(lastname %in% input$player)
  })
  
  # Create Plot
  output$playerplot <- renderPlot({
    
      ggplot(data(), aes(id, total_accuracy, color = lastname)) +
      geom_point(alpha = 0.5) +
      labs(x = "Game Appearance",
           y = "Accuracy",
           color = "Player") +
      scale_y_continuous(labels = percent) +
      theme_minimal()
    
  })
}

shinyApp(ui, server)
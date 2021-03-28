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
    titlePanel("Top 10 Player Stats"),
    
    # Define the page with a sidebar and main panel
    sidebarLayout(
        # Sidebar panel
        sidebarPanel(
            # Create drop-down to select variable
            selectInput(inputId = "stat", label = h3("Stat"),
                        choices = c("Average" = "average",
                                    "Max Score" = "max_score",
                                    "Average Correct" = "avg_correct",
                                    "Average Incorrect" = "avg_incorrect",
                                    "Total Correct" = "total_correct",
                                    "Total Incorrect" = "total_incorrect",
                                    "Total Games" = "total_games"
                                    )
                        )
        ),
        mainPanel(plotOutput(outputId = "statplot", width = "500px", height = "500px"))
    )
)

server <- function(input, output) {
    
    # Create connection
    con <- dbConnect(RMariaDB::MariaDB(),
                     dbname = "jeopardy",
                     user = "root",
                     password = ""
    )
    
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
    
    # Disconnect from the database
    dbDisconnect(con)
    
    # Pivot Longer
    top10_stats <- top10_stats %>% 
        select(-firstname) %>% 
        mutate(across(c(2:8), as.numeric)) %>% 
        pivot_longer(cols = c(2:8), names_to = "stat")
    
    # Make Reactive & Filter Based on Input
    data <- reactive({
        req(input$stat)
        df <-  top10_stats %>% 
            filter(stat %in% input$stat)
    })
    
    # Create Plot
    output$statplot <- renderPlot({

        ggplot(data(), aes(lastname, value)) +
            geom_col(fill = "steelblue") +
            labs(x = "",
                 y = "") +
            scale_y_continuous(labels = comma) +
            theme_minimal()
    })
}

shinyApp(ui, server)
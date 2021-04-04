#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(scales)

# Define UI for application
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Jeopardy! Insights"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Player Accuracy", tabName = "Players"),
            menuItem(text = "Player Stats", tabName = "Players2"),
            menuItem(text = "100 Club", tabname = "Players3")
            )
        ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "Players",
                sidebarLayout(
                    # Sidebar panel
                    sidebarPanel(# Create drop-down to select variable
                        checkboxGroupInput(
                            inputId = "player",
                            label = h3("Choose a Player:"),
                            choices = c(
                                "Arthur Chu" = "Chu",
                                "Julia Collins" = "Collins",
                                "James Holzhauer" = "Holzhauer",
                                "Matt Jackson" = "Jackson",
                                "Ken Jennings" = "Jennings",
                                "Jason Keller" = "Keller",
                                "David Madden" = "Madden",
                                "Austin Rogers" = "Rogers",
                                "Seth Wilson" = "Wilson",
                                "Jason Zuffranieri" = "Zuffranieri"
                                )
                            )
                        ),
                    mainPanel(
                        plotOutput(
                            outputId = "playerplot",
                            width = "800px",
                            height = "500px"
                        )
                    )
                )),
        tabItem(tabName = "Players2",
                sidebarLayout(
                    # Sidebar panel
                    sidebarPanel(# Create drop-down to select variable
                        selectInput(
                            inputId = "stat",
                            label = h3("Stat"),
                            choices = c(
                                "Average Winnings" = "average",
                                "Max Single Game Score" = "max_score",
                                "Average Correct Per Game" = "avg_correct",
                                "Average Incorrect Per Game" = "avg_incorrect",
                                # "Total Correct" = "total_correct",
                                # "Total Incorrect" = "total_incorrect",
                                "Total Games Played" = "total_games"
                            )
                        )),
                    box(plotOutput(outputId = "statplot"))
                ))
         #,
         #tabItem(tabName = "Players3",
        #         sidebarLayout(
        #         sidebarPanel(
        #           selectInput())),
        #         mainPanel(
        #           box(tableOutput(outputId = "hundredclub"))))
    ))
)
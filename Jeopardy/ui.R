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

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "JEOPARDY! Insights"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(text = "Notable Player Analysis 1", tabName = "Players"),
            menuItem(text = "Notable Player Analysis 2", tabName = "Players2")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "Players",
                sidebarLayout(
                    # Sidebar panel
                    sidebarPanel(
                        # Create drop-down to select variable
                        checkboxGroupInput(
                            inputId = "player", label = h3("Choose a Player:"),
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
                    mainPanel(plotOutput(outputId = "playerplot", width = "500px", height = "500px"))
                ),
                box(plotOutput("playerdailydouble"), width = 6)
            ),
            tabItem(
                tabName = "Players2",
                sidebarLayout(
                    # Sidebar panel
                    sidebarPanel(
                        # Create drop-down to select variable
                        selectInput(
                            inputId = "stat", label = h3("Stat"),
                            choices = c(
                                "Average" = "average",
                                "Max Score" = "max_score",
                                "Average Correct" = "avg_correct",
                                "Average Incorrect" = "avg_incorrect",
                                "Total Correct" = "total_correct",
                                "Total Incorrect" = "total_incorrect",
                                "Total Games" = "total_games"
                            )
                        )
                    ),
                    box(plotOutput(outputId = "statplot"))
                )
            )
        )
    )
)
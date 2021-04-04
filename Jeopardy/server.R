#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Load Data (locally)
    # load(here::here("Jeopardy", "jeopardy_app_data.RData"))
    
    # Load Data (for Shiny)
    load("jeopardy_app_data.RData")
    
    # ┌────────────────────────────────────────┐
    # │         Top 10 Player Accuracy         │
    # └────────────────────────────────────────┘
    
    # Make Reactive & Filter Based on Input
    data_accuracy <- reactive({
        req(input$player)
        df <- player_accuracy %>%
            filter(lastname %in% input$player)
    })
    
    filtereddata <- reactive({
        player_accuracy[player_accuracy$player %in% input$inputplayers]
    })
    
    # ┌────────────────────────────────────────┐
    # │          Top 10 Player Stats           │
    # └────────────────────────────────────────┘
    
    # Pivot Longer
    top10_stats <- top10_stats %>%
        dplyr::select(-firstname) %>%
        mutate(across(c(2:8), as.numeric)) %>%
        pivot_longer(cols = c(2:8), names_to = "stat")
    
    # Make Reactive & Filter Based on Input
    data_stats <- reactive({
        req(input$stat)
        df <- top10_stats %>%
            filter(stat %in% input$stat)
    })
    
    # ┌────────────────────────────────────────┐
    # │           100% Accuracy Club           │
    # └────────────────────────────────────────┘
    
    # Add Accuracy to winners
    winners <- winners %>%
        mutate(accuracy = round(as.numeric(ansRight) / (as.numeric(ansRight) + as.numeric(ansWrong)) * 100))
    
    # Who is in the 100% accuracy club
    onehundredclub <- filter(winners, accuracy == 100) %>%
        dplyr::select(firstname, lastname) %>%
        distinct()
    
    # ┌────────────────────────────────────────┐
    # │            Create the Plots            │
    # └────────────────────────────────────────┘
    
    # Top 10 Player Accuracy

    # Add ID Column
    player_accuracy <- player_accuracy %>%
        group_by(lastname) %>%
        mutate(id = row_number()) %>%
        ungroup()
    
    output$playerplot <- renderPlot({
        ggplot(data_accuracy(), aes(id, total_accuracy, color = lastname)) +
            geom_line(alpha = 0.5, size = 1) +
            labs(
                title = "How Accurate are the Most Notable Players?",
                x = "Game Appearance",
                y = "Accuracy",
                color = "Player"
            ) +
            scale_y_continuous(labels = percent) +
            theme_minimal()
    })
    
    # Top 10 Player Stats
    output$statplot <- renderPlot({
        ggplot(data_stats(), aes(lastname, value)) +
            geom_col(fill = "steelblue") +
            labs(x = "",
                 y = "") +
            scale_y_continuous(labels = comma) +
            theme_minimal()
    })
    # output$accuracyplot <- renderPlot({
    #     onehundredclub %>% 
    #         arrange(lastname) %>% 
    #         ggplot(aes(playerid, total_accuracy, color = lastname)) +
    #         geom_line(alpha = 0.5, size = 1) +
    #         labs(
    #             title = "How Accurate are the Most Notable Players?",
    #             x = "Game Appearance",
    #             y = "Accuracy",
    #             color = "Player"
    #         ) +
    #         scale_y_continuous(labels = percent) +
    #         theme_minimal()
    # })
})
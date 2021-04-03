#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Load Data
    load("jeopardy_app_data.RData")
    
    # ┌────────────────────────────────────────┐
    # │          Top 10 Player Stats           │
    # └────────────────────────────────────────┘
    
    # Pivot Longer
    top10_stats <- top10_stats %>%
        dplyr::select(-firstname) %>%
        mutate(across(c(2:8), as.numeric)) %>%
        pivot_longer(cols = c(2:8), names_to = "stat")
    
    # Make Reactive & Filter Based on Input
    data <- reactive({
        req(input$stat)
        df <- top10_stats %>%
            filter(stat %in% input$stat)
    })
    
    # ┌────────────────────────────────────────┐
    # │         Top 10 Player Accuracy         │
    # └────────────────────────────────────────┘
    
    # Add ID Column
    player_accuracy <- player_accuracy %>%
        group_by(lastname) %>%
        mutate(id = row_number()) %>%
        ungroup()
    
    # Make Reactive & Filter Based on Input
    data1 <- reactive({
        req(input$player)
        df <- player_accuracy %>%
            filter(lastname %in% input$player)
    })
    
    filtereddata <- reactive({
        player_accuracy[player_accuracy$player %in% input$inputplayers]
    })
    
    # ┌────────────────────────────────────────┐
    # │       Double Jeopardy Locations        │
    # └────────────────────────────────────────┘
    
    # Convert doubles to tibble
    doubles <- as_tibble(dj_loc)
    
    # Clean up
    doubles <- doubles %>%
        rename("row" = rowcat,
               "category" = colcat,
               "instances" = `count(*)`) %>%
        # mutate(pct = instances / 395350)  # number of clues in rounds 1 & 2
        # mutate(pct = instances / 6775)    # number of games
        mutate(pct = instances / sum(instances))
    
    # Add Accuracy to winners
    winners <- winners %>%
        mutate(accuracy = round(as.numeric(ansRight) / (as.numeric(ansRight) + as.numeric(ansWrong)) * 100))
    
    # who is in the 100% accuracy club
    onehundredclub <- filter(winners, accuracy == 100) %>%
        dplyr::select(firstname, lastname) %>%
        distinct()
    
    # summarize career stats
    averages <- winners %>%
        group_by(players_playerid, firstname, lastname) %>%
        summarize(
            game_count = n(),
            avg_correct = mean(ansRight),
            avg_incorrect = mean(ansWrong),
            total_correct = sum(ansRight),
            total_incorrect = sum(ansWrong),
            max_score = max(finalscore),
            avg_acc = mean(accuracy),
            avg_score = mean(finalscore)
        )
    
    top <- dj_top %>%
        mutate(name = paste(firstname, lastname))
    
    # Change double_jeop_count to numeric
    top$double_jeop_count <- as.numeric(top$double_jeop_count)
    
    # ┌────────────────────────────────────────┐
    # │            Create the Plots            │
    # └────────────────────────────────────────┘
    
    output$statplot <- renderPlot({
        ggplot(datatopdata, aes(lastname, value)) +
            geom_col(fill = "steelblue") +
            labs(x = "",
                 y = "") +
            scale_y_continuous(labels = comma) +
            theme_minimal()
    })
    output$doublelocation <- renderPlot({
        doubles %>%
            ggplot(aes(
                category,
                row,
                fill = pct,
                label = percent(pct, accuracy = 0.01)
            )) +
            geom_tile() +
            geom_text(color = "white", size = rel(3.5)) +
            labs(
                title = "Where are you likely to find a Daily Double?",
                x = "",
                y = "",
                fill = ""
            ) +
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
            theme_minimal() +
            theme(
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                axis.line = element_blank(),
                axis.ticks = element_blank(),
                axis.text = element_text(face = "bold"),
                legend.position = ""
            )
    })
    output$topcategories <- renderPlot({
        top_cat %>%
            mutate(games = as.numeric(games)) %>%
            ggplot(aes(
                games,
                reorder(category, games),
                label = comma(games, accuracy = 1)
            )) +
            geom_col(fill = "steelblue") +
            geom_text(hjust = 1.5,
                      color = "white",
                      size = 4) +
            labs(title = "Top 10 Most Common Categories",
                 x = "Games",
                 y = "") +
            theme_minimal()
    })
    
    output$statplot <- renderPlot({
        ggplot(data(), aes(lastname, value)) +
            geom_col(fill = "steelblue") +
            labs(x = "",
                 y = "") +
            scale_y_continuous(labels = comma) +
            theme_minimal()
    })
    
    output$playerplot <- renderPlot({
        ggplot(data1(), aes(id, total_accuracy, color = lastname)) +
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
    
    
    output$cluewordcloud <- renderPlot({
        wordcloud(
            words = clue_50$term,
            freq = clue_50$count,
            min.freq = 1,
            max.words = 100,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(6, "Blues")
        )
    })
    
    output$catwordcloud <- renderPlot({
        wordcloud(
            words = cat_50$term,
            freq = cat_50$count,
            min.freq = 1,
            max.words = 100,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(6, "Blues")
        )
    })
    
    output$answordcloud <- renderPlot({
        wordcloud(
            words = ans_50$term,
            freq = ans_50$count,
            min.freq = 1,
            max.words = 100,
            random.order = FALSE,
            rot.per = 0.35,
            colors = brewer.pal(6, "Blues")
        )
    })
})
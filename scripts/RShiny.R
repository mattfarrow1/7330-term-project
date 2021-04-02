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
library(scales)
#for text analysis
library(tm)
library(wordcloud)
library(plotly)



ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "JEOPARDY! Insights"),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem(text = "Notable Player Analysis 1", tabName = "Players"),
                        menuItem(text = "Notable Player Analysis 2", tabName = "Players2"),
                        menuItem(text = "Categories and Clues", tabName = "CE"),
                        menuItem(text = "Daily Double Exploration", tabName = "DDE")
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Players",
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
                                  mainPanel(plotOutput(outputId = "playerplot", width = "500px", height = "500px"))),
                                box(plotOutput("playerdailydouble"), width = 6)),
                        tabItem(tabName = "Players2", 
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
                                                            "Total Games" = "total_games")
                                                
                                    )
                                  ),
                                  box(plotOutput(outputId = "statplot"))
                                )),
                        tabItem(tabName = "CE",
                                box(title= "Category Word Cloud", plotOutput("catwordcloud"), width = 6),
                                box(title = "Clue Word Cloud", plotOutput("cluewordcloud")),
                                box(plotOutput("topcategories"), width = 8)),
                        tabItem(tabName = "DDE",
                                box(plotOutput("doublelocation"), width = 8),
                                box(title = "Daily Double Answers Word Cloud", plotOutput("answordcloud"), width = 6))
                        
                        
                      )
                    )               
)

server <-  function(input,output){

#Establish connection to DB
          con <- dbConnect(RMariaDB::MariaDB(),
                           dbname = "jeopardy",
                           user = "root",
                           password = "Dannysboy092016!"
          )

# SQL Query: Categories ---------------------------------------------------

        # Get the top 10 categories
        res <- dbSendQuery(con, "SELECT category, count(*) AS games
                           FROM board
                           GROUP BY category
                           ORDER BY games DESC
                           LIMIT 10;")
        top_cat <- dbFetch(res, n = Inf)
        dbClearResult(res)

# SQL Query: Double Jeopardy ----------------------------------------------

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
      dj_loc <- dbFetch(res, n = Inf)
      dbClearResult(res)
      
      # Everyone who won Double Jeopardy
      res <- dbSendQuery(
        con,
        "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
        from board
        INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
        INNER JOIN players on players.playerid = doubles_has_scores.playerid
        where doublejeop = 1;"
      )
      dj_who <- dbFetch(res, n = Inf)
      dbClearResult(res)
      
      # Top Double Jeopardy contestants
      res <- dbSendQuery(
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
      dj_top <- dbFetch(res, n = Inf)
      dbClearResult(res)

# SQL Query: Players ------------------------------------------------------

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
      #dbDisconnect(con)
      
      # Pivot Longer
      top10_stats <- top10_stats %>% 
        dplyr::select(-firstname) %>% 
        mutate(across(c(2:8), as.numeric)) %>% 
        pivot_longer(cols = c(2:8), names_to = "stat")
      
      # Make Reactive & Filter Based on Input
      data <- reactive({
        req(input$stat)
        df <-  top10_stats %>% 
          filter(stat %in% input$stat)
      })
        
        # All player info for text analysis
        res <- dbSendQuery(
          con,
          "select * from players;"
        )
        players <- dbFetch(res, n = Inf)
        dbClearResult(res)
        
        # All game player info to perform winner analysis in R
        res <- dbSendQuery(
          con,
          "select finalscore, episode_gameid, finalscoreid, ansRight, ansWrong, players.firstname, players.lastname, players_playerid
          from synopsis
          inner join synopsis_has_players on synopsis_has_players.synopsis_finalscoreid = synopsis.finalscoreid
          inner join players on players.playerid = synopsis_has_players.players_playerid;"
        )
        winners <- dbFetch(res, n = Inf)
        dbClearResult(res)
        
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

# SQL Query: Daily Doubles ------------------------------------------------

        # Daily Double clue info for text analysis joined to who answered it
        res <- dbSendQuery(
          con,
          "select board.clueid, category, clue, answer, players.playerid, players.firstname, players.lastname
          from board
          INNER JOIN doubles_has_scores on doubles_has_scores.clueid = board.clueid
          INNER JOIN players on players.playerid = doubles_has_scores.playerid
          where doublejeop = 1;"
        )
        daily_doubles <- dbFetch(res, n = Inf)
        dbClearResult(res)


# Double Jeopardy Locations -----------------------------------------------

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
        
        # Add Accuracy to winners ----------
        winners <- winners %>%
          mutate(accuracy = round(ansRight / (ansRight + ansWrong) *100))
        
        #who is in the 100% accuracy club
        onehundredclub <- filter(winners, accuracy == 100) %>%
          dplyr::select(firstname, lastname) %>%
          distinct()
        
        #summarize career stats
        averages <- winners %>%
          group_by(players_playerid, firstname, lastname) %>%
          summarize(game_count = n(), avg_correct = mean(ansRight), avg_incorrect = mean(ansWrong),
                    total_correct = sum(ansRight), total_incorrect = sum(ansWrong),
                    max_score = max(finalscore),avg_acc = mean(accuracy), avg_score = mean(finalscore))
        
        top <- dj_top %>%
          mutate(name = paste(firstname,lastname))
        
        # Change double_jeop_count to numeric
        top$double_jeop_count <- as.numeric(top$double_jeop_count)

# Top Daily Double Key Words ---------------------------------------------
          #create corpus from the category, clue & answer information
          clue_corpus <- SimpleCorpus(VectorSource(daily_doubles$clue))
          category_corpus <- SimpleCorpus(VectorSource(daily_doubles$category))
          answer_corpus <- SimpleCorpus(VectorSource(daily_doubles$answer))
          
          # 1. Stripping any extra white space:
          clue_corpus <- tm_map(clue_corpus, stripWhitespace)
          category_corpus <- tm_map(category_corpus, stripWhitespace)
          answer_corpus <- tm_map(answer_corpus, stripWhitespace)
          
          # 2. Transforming everything to lowercase
          clue_corpus <- tm_map(clue_corpus, content_transformer(tolower))
          category_corpus <- tm_map(category_corpus, content_transformer(tolower))
          answer_corpus <- tm_map(answer_corpus, content_transformer(tolower))
          
          # 3. Removing numbers 
          clue_corpus <- tm_map(clue_corpus, removeNumbers)
          category_corpus <- tm_map(category_corpus, removeNumbers)
          answer_corpus <- tm_map(answer_corpus, removeNumbers)
          
          # 4. Removing punctuation
          clue_corpus <- tm_map(clue_corpus, removePunctuation)
          category_corpus <- tm_map(category_corpus, removePunctuation)
          answer_corpus <- tm_map(answer_corpus, removePunctuation)
          
          # 5. Removing stop words
          clue_corpus <- tm_map(clue_corpus, removeWords, stopwords("english"))
          category_corpus <- tm_map(category_corpus, removeWords, stopwords("english"))
          answer_corpus <- tm_map(answer_corpus, removeWords, stopwords("english"))
          
          DTM_clue <- DocumentTermMatrix(clue_corpus)
          DTM_category <- DocumentTermMatrix(category_corpus)
          DTM_answer <- DocumentTermMatrix(answer_corpus)
          
          #clue wordcloud - you may have to increase memory limit to run
          sums <- as.data.frame(colSums(as.matrix(DTM_clue)))
          sums <- rownames_to_column(sums)
          colnames(sums) <- c("term", "count")
          sums <- arrange(sums, desc(count))
          
          #what are top 10 words
          sums[1:10,]
          
          #make word cloud with top 50 words
          clue_50 <- sums[1:50,]
          
          #category wordcloud
          sums2 <- as.data.frame(colSums(as.matrix(DTM_category)))
          sums2 <- rownames_to_column(sums2)
          colnames(sums2) <- c("term", "count")
          sums2 <- arrange(sums2, desc(count))
          
          #what are top 10 words
          sums2[1:10,]
          
          #make word cloud with top 50 words
          cat_50 <- sums2[1:50,]
          
          #answer wordcloud
          sums3 <- as.data.frame(colSums(as.matrix(DTM_answer)))
          sums3 <- rownames_to_column(sums3)
          colnames(sums3) <- c("term", "count")
          sums3 <- arrange(sums3, desc(count))
          
          #what are top 10 words
          sums3[1:10,]
          
          #make word cloud with top 50 words
          ans_50 <- sums3[1:50,]
          
dbDisconnect(con)
    
    
#CREATING THE PLOTS
          
          output$statplot <- renderPlot({
            ggplot(datatopdata, aes(lastname, value)) +
              geom_col(fill = "steelblue") +
              labs(x = "",
                   y = "") +
              scale_y_continuous(labels = comma) +
              theme_minimal()
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
          output$topcategories <- renderPlot({top_cat %>% 
              mutate(games = as.numeric(games)) %>% 
              ggplot(aes(games, reorder(category, games), label = comma(games, accuracy = 1))) +
              geom_col(fill = "steelblue") +
              geom_text(hjust =  1.5, color = "white", size = 4) +
              labs(title = "Top 10 Most Common Categories",
                   x = "Games",
                   y = "") +
              theme_minimal()}) 
          
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
                geom_point(alpha = 0.5) +
                labs(x = "Game Appearance",
                     y = "Accuracy",
                     color = "Player") +
                scale_y_continuous(labels = percent) +
                theme_minimal()
              
            })
            
            
          output$cluewordcloud <- renderPlot({wordcloud(words = clue_50$term, freq = clue_50$count,min.freq = 1,
                                                        max.words=100, random.order=FALSE, rot.per=0.35, 
                                                        colors=brewer.pal(6, "Blues"))})
          
          output$catwordcloud <- renderPlot({wordcloud(words = cat_50$term, freq = cat_50$count,min.freq = 1,
                                                       max.words=100, random.order=FALSE, rot.per=0.35, 
                                                       colors=brewer.pal(6, "Blues"))})
          
          output$answordcloud <- renderPlot({wordcloud(words = ans_50$term, freq = ans_50$count,min.freq = 1,
                                                       max.words=100, random.order=FALSE, rot.per=0.35, 
                                                       colors=brewer.pal(6, "Blues"))})


}

shinyApp(ui, server)


# Setup -------------------------------------------------------------------

source(here::here("scripts", "query-database.R"))
library(scales)
#for text analysis
library(tm)
library(wordcloud)

# Top Categories ----------------------------------------------------------

# Create plot
top_cat %>% 
  mutate(games = as.numeric(games)) %>% 
  ggplot(aes(games, reorder(category, games), label = comma(games, accuracy = 1))) +
  geom_col(fill = "steelblue") +
  geom_text(hjust =  1.5, color = "white", size = 4) +
  labs(title = "Top 10 Most Common Categories",
       x = "Games",
       y = "") +
  theme_minimal()

ggsave(here::here("images", "top-10-categories.png"), dpi = "retina")

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

# Create plot
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

ggsave(here::here("images", "daily-double-locations.png"), dpi = "retina")

# Top 10 Daily Double Count -----------------------------------------------

# Create new column combining first and last name
top <- dj_top %>%
  mutate(name = paste(firstname,lastname))

# Change double_jeop_count to numeric
top$double_jeop_count <- as.numeric(top$double_jeop_count)

# Create plot
top %>%
  arrange(double_jeop_count) %>%
  ggplot(aes(x = reorder(name, double_jeop_count), y = double_jeop_count)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  ylab("Career Daily Double Count") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    panel.background = element_blank(),
    plot.title = element_text(size = 15)
  ) +
  ggtitle("Who Got the Most Daily Double Clues?")

ggsave(here::here("images", "top-daily-double-clues.png"), dpi = "retina")



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

wordcloud(words = clue_50$term, freq = clue_50$count,min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Blues"))

#category wordcloud
sums2 <- as.data.frame(colSums(as.matrix(DTM_category)))
sums2 <- rownames_to_column(sums2)
colnames(sums2) <- c("term", "count")
sums2 <- arrange(sums2, desc(count))

#what are top 10 words
sums2[1:10,]

#make word cloud with top 50 words
cat_50 <- sums2[1:50,]

wordcloud(words = cat_50$term, freq = cat_50$count,min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Blues"))


#answer wordcloud
sums3 <- as.data.frame(colSums(as.matrix(DTM_answer)))
sums3 <- rownames_to_column(sums3)
colnames(sums3) <- c("term", "count")
sums3 <- arrange(sums3, desc(count))

#what are top 10 words
sums3[1:10,]

#make word cloud with top 50 words
ans_50 <- sums3[1:50,]

wordcloud(words = ans_50$term, freq = ans_50$count,min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(6, "Blues"))

# Add Accuracy to winners ----------
winners <- winners %>%
  mutate(accuracy = round(ansRight / (ansRight + ansWrong) *100))

#who is in the 100% accuracy club
onehundredclub <- filter(winners, accuracy == 100) %>%
  select(firstname, lastname) %>%
  distinct()

#summarize career stats
averages <- winners %>%
  group_by(players_playerid, firstname, lastname) %>%
  summarize(game_count = n(), avg_correct = mean(ansRight), avg_incorrect = mean(ansWrong),
            total_correct = sum(ansRight), total_incorrect = sum(ansWrong),
            max_score = max(finalscore),avg_acc = mean(accuracy), avg_score = mean(finalscore))

ggplot(averages, aes(x = avg_correct, y = avg_incorrect, color = avg_score)) +
  geom_point() +
  xlab("Average Correct Answers") +
  ylab("Average Incorrect Answers") +
  ggtitle("Average Player Accuracy") +
  theme(
    axis.title.y = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    plot.title = element_text(size = 15)
  )


# Who Won Each Game -------------------------------------------------------
champ <- winners %>% 
  group_by(episode_gameid) %>% 
  arrange(desc(finalscore)) %>% 
  filter(row_number() == 1)

#total games won
games_won <- champ %>% 
  group_by(players_playerid, firstname, lastname) %>%
  summarise(games_won = n())



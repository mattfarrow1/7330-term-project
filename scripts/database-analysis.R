
# Setup -------------------------------------------------------------------

source(here::here("scripts", "query-database.R"))
library(scales)

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

#create new column combining first and last name
top <- top %>%
  mutate(name = paste(firstname,lastname))

#change double_jeop_count to numeric
top$double_jeop_count <- as.numeric(top$double_jeop_count)

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
  ggtitle("Who Got the Most Daily Double Clues?")

# Daily Double Wagers -----------------------------------------------------


# Top Daily Double Categories ---------------------------------------------


# Who Won Each Game -------------------------------------------------------



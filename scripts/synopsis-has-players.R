# Load library
library(tidyverse)

# Load data
load(here::here("data - output", "jeopardy.RData"))

# Start with players_has_episodes
players_has_episodes <- game_contestants %>% 
  pivot_longer(cols = starts_with("player_"), values_to = "playerid") %>% 
  select(-name) %>% 
  filter(!is.na(playerid)) %>% 
  rename("gameid" = uid)

# Get player ids & first name
x <- player_ids %>% 
  select(contestant_id, first)

# Join together
players_has_episodes <- left_join(players_has_episodes, x, by = c("playerid" = "contestant_id"))

# Load the synopsis table data from mysql
# X4g <- read_csv("~/Desktop/4g.csv")
glimpse(X4g)

# Create synopsis_has_players
synopsis_has_players <-
  left_join(X4g,
            players_has_episodes,
            by = c("episode_gameid" = "gameid", "name" = "first")) %>% 
  select(1, 8)
glimpse(synopsis_has_players)

# Save to disk
write_csv(synopsis_has_players, here::here("data - output", "synopsis_has_players.csv"))

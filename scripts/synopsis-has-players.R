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

# Look at the data
glimpse(players_has_episodes)
glimpse(synopsis)

# Create synopsis_has_players
synopsis_has_players <-
  left_join(synopsis,
            players_has_episodes,
            by = c("uid" = "gameid", "name" = "first"))
glimpse(synopsis_has_players)

# Save to disk
write_csv(synopsis_has_players, here::here("data - output", "synopsis_has_players.csv"))

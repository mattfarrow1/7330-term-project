players_has_episodes <- game_contestants %>% 
  pivot_longer(cols = starts_with("player_"), values_to = "playerid") %>% 
  select(-name) %>% 
  filter(!is.na(playerid)) %>% 
  rename("gameid" = uid)

write_csv(players_has_episodes, "players_has_episodes.csv")
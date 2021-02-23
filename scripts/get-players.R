library(tidyverse)
library(whatr)
library(rvest) 

# Function to scrape the data
get_players <- function(id) {
  # Get IDs
  ids <- whatr::whatr_html(id) %>%
    rvest::html_nodes(".contestants") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    str_sub(start = 52) %>%
    tibble::enframe() %>%
    rename(contestant_id = value) %>%
    mutate(contestant = paste('player', row_number())) %>%
    select(-1)
  # Get names and bios
  names <- whatr::whatr_players(id) %>%
    mutate(contestant = paste('player', row_number()))
  # Merge together
  ids %>% 
    left_join(names, by = "contestant") %>% 
    select(-contestant) %>% 
    mutate(uid = id)
}

# Scrape all 6,498 games
players <- map(1:6948, get_players)

# Save the data as a backup
save(players, file = here::here("data - raw", "raw_player_data.RData"))

# Unlist to a tibble
players_df <- map_dfr(players, `[`, c("contestant_id", "first", "last", "occupation", "from", "uid"))

# Create datasets for contestant IDs and UIDs
player_ids <- players_df %>% 
  select(contestant_id, first, last, occupation, from) %>% 
  distinct() %>% 
  arrange(contestant_id)

game_contestants <- players_df %>% 
  select(uid, contestant_id) %>% 
  arrange(uid, contestant_id) %>% 
  group_by(uid) %>% 
  mutate(key = paste('player', row_number())) %>%
  ungroup() %>% 
  pivot_wider(names_from = "key", values_from = contestant_id)
game_contestants <- janitor::clean_names(game_contestants)

# Save data
write_csv(player_ids, here::here("data - output", "player_ids.csv"))
save(player_ids, file = here::here("data - output", "player_ids.RData"))

write_csv(game_contestants, here::here("data - output", "game_contestants.csv"))
save(game_contestants, file = here::here("data - output", "game_contestants.RData"))

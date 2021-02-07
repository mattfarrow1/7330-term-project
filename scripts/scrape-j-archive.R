#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Scrape J! Archive
# Megan Ball, Matt Farrow, Jake Harrison
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setup -------------------------------------------------------------------

# Load libraries
library(tidyverse)  # general data analysis
library(polite)     # respectful web scraping
library(rvest)      # web scraping

# Define directories
game_dir  <- here::here("data - raw", "games")
score_dir <- here::here("data - raw", "scores")

# Check Scraping Restrictions ---------------------------------------------

# Make our intentions known to the website and see what we're allowed to do
session <- bow(
  url = "http://www.j-archive.com/listseasons.php",  # base URL
  user_agent = "Matt Farrow"  # identify ourselves
)

session

# There aren't any restrictions other than a 20 sec crawl delay.

# Get Game Data -----------------------------------------------------------

get_game_data <- function(id, verbose = FALSE) {
  
  # Define URL for game data
  url <-
    paste0("http://www.j-archive.com/showgame.php?game_id=", id)
  
  # Define the game ID
  game_id <- url %>% 
    str_sub(start = 47)
  
  # Politely scrape the data, using a 20 second delay between attempts
  html <- url %>%
    polite::bow(delay = 25) %>%
    polite::scrape()
  
  # Get the date of the game
  date <- html %>%
    rvest::html_node("#game_title") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\w+ \\d+, \\d+$")
  
  if (verbose) {
    message("Scraping game ", id, " (", date, ")")
  }
  
  rounds <- rvest::html_nodes(html, ".round")
  
  if (!length(rounds)) {
    if (verbose) {
      message("Skipping game ", id, " (", date, ")")
    }
    
    return(
      tibble::tibble(
        date     = character(0),
        round    = character(0),
        category = character(0),
        comment  = character(0),
        value    = character(0),
        clue     = character(0),
        response = character(0),
        link     = character(0),
        contestants = character(0),
        comments = character(0)
      )
    )
  }
  
  `%xmod%` <- function(lhs, rhs) {
    res           <- lhs %% rhs
    res[res == 0] <- rhs
    res
  }
  
  comments <- html %>%
    rvest::html_nodes("#game_comments") %>%
    rvest::html_text() %>%
    tibble::enframe() %>%
    select(value) %>%
    rename(comments = value)
  
  contestant_ids <- html %>%
    rvest::html_nodes(".contestants") %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href") %>%
    str_sub(start = 52) %>%
    tibble::enframe() %>%
    rename(contestant_id = value) %>%
    mutate(key = paste('player', row_number())) %>%
    select(-1)
  
  contestants <- html %>%
    rvest::html_nodes(".contestants") %>%
    rvest::html_text() %>%
    tibble::enframe() %>%
    select(value) %>%
    tidyr::separate(
      value,
      into = c("name", 'bio'),
      sep = ", ",
      extra = "drop",
      fill = "right"
    ) %>%
    tidyr::separate(
      name,
      into = c("first", "last"),
      sep = " ",
      remove = FALSE,
      extra = "drop",
      fill = "right"
    ) %>%
    mutate(key = paste('player', row_number()))
  
  # Get the table for the scores prior to final jeopardy
  scores_dj <- html %>% 
    rvest::html_nodes("#double_jeopardy_round > table:nth-child(4)") %>% 
    rvest::html_table() %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    pivot_longer(cols = -1,
                 names_to = "name",
                 values_to = "value") %>%
    pivot_wider(names_from = rowname, values_from = value) %>%
    select(c(2:3)) %>%
    rename(first = `1`,
           score_dj = `2`) %>%
    mutate(score_dj = str_replace(score_dj, "^-\\$(.*)$", "$-\\1"),
           score_dj = parse_number(score_dj))
  
  # Get the table for the scores after final jeopardy
  scores_fj <- html %>% 
    rvest::html_nodes("#final_jeopardy_round > table:nth-child(4)") %>% 
    rvest::html_table(fill = TRUE) %>% 
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    pivot_longer(cols = -1,
                 names_to = "name",
                 values_to = "value") %>%
    pivot_wider(names_from = rowname, values_from = value) %>%
    select(c(2:3)) %>%
    rename(first = `1`,
           score_fj = `2`) %>%
    mutate(score_dj = str_replace(score_dj, "^-\\$(.*)$", "$-\\1"),
           score_dj = parse_number(score_dj))
  
  # Merge scores and contestants
  contestants <- left_join(contestants, scores_dj, by = "first")
  contestants <- left_join(contestants, scores_fj, by = "first")
  
  # Merge contestant ID into contestants
  contestants <- full_join(contestant_ids, contestants, by = "key")
  
  categories <- rounds %>%
    rvest::html_nodes(".category_name") %>%
    rvest::html_text() %>%
    tibble::enframe(name = "id", value = "category") %>%
    dplyr::mutate(
      round = incase::switch_case((id + 5) %/% 6, 1 ~ "J", 2 ~ "DJ", 3 ~ "FJ"),
      cat_id  = as.character(id %xmod% 6),
      id      = NULL,
      comment = rvest::html_text(rvest::html_nodes(rounds, ".category_comments"))
    )
  
  round_clues <- rounds %>%
    rvest::html_nodes(".clue") %>%
    purrr::map_dfr(
      ~ tibble::tibble(
        id       = rvest::html_attr(rvest::html_node(., ".clue_text"), "id"),
        value    = rvest::html_text(rvest::html_node(., ".clue_value")),
        clue     = rvest::html_text(rvest::html_node(., ".clue_text")),
        response = rvest::html_attr(rvest::html_node(., "div"), "onmouseover"),
        link     = rvest::html_attr(rvest::html_node(., ".clue_text > a:last-child"), "href")
      )
    ) %>%
    dplyr::mutate(
      cat_id = stringr::str_match(id, "clue_(.+)_(.+)_(.+)"),
      round  = cat_id[, 2],
      cat_id = cat_id[, 3]
    ) %>%
    dplyr::left_join(categories, by = c("cat_id", "round")) %>%
    dplyr::select(-cat_id)
  
  
  final_jeopardy <- html %>%
    rvest::html_nodes(".final_round") %>%
    purrr::map_dfr(
      ~ tibble::tibble(
        round = rvest::html_node(., ".clue_text") %>%
          rvest::html_attr("id") %>%
          stringr::str_extract("[A-Z]{2}"),
        category = rvest::html_text(rvest::html_node(., ".category_name")),
        comment  = rvest::html_text(rvest::html_node(., ".category_comments")),
        value    = rvest::html_text(rvest::html_node(., ".clue_value")),
        clue     = rvest::html_text(rvest::html_node(., ".clue_text")),
        response = rvest::html_attr(rvest::html_node(., "div"), "onmouseover"),
        link     = rvest::html_attr(rvest::html_node(., ".clue_text > a:last-child"), "href")
      )
    )
  
  clues <- dplyr::bind_rows(round_clues, final_jeopardy) %>%
    dplyr::mutate(
      response = response %>%
        stringr::str_match(".*<em class=.*correct_response.*?>(.*)</em>.*") %>%
        magrittr::extract(, 2) %>%
        stringr::str_replace_all("</?i>", ""),
      dplyr::across(c(clue, response), dplyr::na_if, "="),
      round = round,
      value = readr::parse_number(value),
      date  = lubridate::mdy(date),
      game_comments = comments$comments,
      player1_id = contestants$contestant_id[1],
      player1_name = contestants$name[1],
      player1_bio = contestants$bio[1],
      player1_score_dj = contestants$score_dj[1],
      player1_score_fj = contestants$score_fj[1],
      player2_id = contestants$contestant_id[2],
      player2_name = contestants$name[2],
      player2_bio = contestants$bio[2],
      player2_score_dj = contestants$score_dj[2],
      player2_score_fj = contestants$score_fj[2],
      player3_id = contestants$contestant_id[3],
      player3_name = contestants$name[3],
      player3_bio = contestants$bio[3],
      player3_score_dj = contestants$score_dj[3],
      player3_score_fj = contestants$score_fj[3],
      game_id = game_id
    )
  
}

# Scrape Data -------------------------------------------------------------

# Megan's Games
game_data_megan <- map(1:2000, get_game_data)
game_details_megan <- as_tibble(do.call(rbind, game_data_megan)) 
write_csv(game_details_megan, here::here("data - output", "game_details_1_2000.csv"))

game_data_jake <- map(2001:4000, get_game_data)
game_details_jake <- as_tibble(do.call(rbind, game_data_jake)) 
write_csv(game_details_jake, here::here("data - output", "game_details_20001_4000.csv"))

game_data_matt <- map(4001:6932, get_game_data)
game_details_matt <- as_tibble(do.call(rbind, game_data_matt)) 
write_csv(game_details_matt, here::here("data - output", "game_details_4001_6932.csv"))

# Split into data sets ----------------------------------------------------

# games <- game_details %>% 
#   select(game_id, date, id, value, clue, response, link, round, category, comment)
# 
# players <- game_details %>% 
#   select(game_id, date, 10:25) %>% 
#   distinct()
# 
# # Save
# write_csv(games,   here::here("data - output", "games.csv"))
# write_csv(players, here::here("data - output", "scores.csv"))
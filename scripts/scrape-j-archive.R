#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# Scrape J! Archive
# Matt Farrow
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

game_ids <- 1:6924

get_game_data <- function(id, verbose = FALSE) {
 
  # Define URL for game data
  url <-
    paste0("http://www.j-archive.com/showgame.php?game_id=", id)
  
  # Politely scrape the data, using a 20 second delay between attempts
  html <- url %>%
    polite::bow(delay = 20) %>%
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
        # contestants = character(0),
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
  
  contestants <- html %>%
    rvest::html_nodes(".contestants") %>%
    rvest::html_text() %>%
    tibble::enframe() %>%
    select(value) %>%
    mutate(key = paste('player', row_number())) %>%
    pivot_wider(names_from = key, values_from = value)

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
      game_comments = comments,
      player1 = contestants[, 1],
      player2 = contestants[, 2],
      player3 = contestants[, 3]
    ) %>%
    dplyr::select(date, round, category, comment, value, clue, response, link, game_comments, player1, player2, player3)
  
}

# Get Player Info ---------------------------------------------------------

get_player_name <- function(id) {
  read_html(paste0(
    "https://www.j-archive.com/showplayerstats.php?player_id=",
    id
  )) %>%
    html_node(".player_full_name") %>%
    html_text()
}

# Scrape Data -------------------------------------------------------------

# Enter game IDs into `map` function to get the data from those games
game_data <- map(6895:6900, get_game_data)

# Convert list generated in the previous line into a single tibble
games <- as_tibble(do.call(rbind, game_data))

# Enter player IDs into `map` function
player_names <- map(1:10, get_player_name)

player_names %>% 
  unlist()

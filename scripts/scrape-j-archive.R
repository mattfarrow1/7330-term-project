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

# Set Up Bow --------------------------------------------------------------

# Make our intentions known to the website and see what we're allowed to do
session <- bow(
  url = "http://www.j-archive.com/listseasons.php",  # base URL
  user_agent = "Matt Farrow"  # identify ourselves
)

session

# Get Data from J! Archive ------------------------------------------------

id <- 6895

url <- paste0("http://www.j-archive.com/showgame.php?game_id=", id)
  
html <- url %>%
  polite::bow() %>%
  polite::scrape()
  
date <- html %>%
  rvest::html_node("#game_title") %>%
  rvest::html_text() %>%
  stringr::str_extract("\\w+ \\d+, \\d+$")

rounds <- rvest::html_nodes(html, ".round")
  
`%xmod%` <- function(lhs, rhs) {
  res           <- lhs %% rhs
  res[res == 0] <- rhs
  res
}
  
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
    date  = lubridate::mdy(date)
  ) %>%
  dplyr::select(date, round, category, comment, value, clue, response, link)

contestants <- html %>% 
  rvest::html_nodes(".contestants") %>% 
  rvest::html_text() %>%
  tibble::enframe()

game_comments <- html %>% 
  rvest::html_nodes("#game_comments") %>% 
  rvest::html_text() %>% 
  tibble::enframe()

# Player Information ------------------------------------------------------

get_player_info <- function(id){
  url <- paste0("https://www.j-archive.com/showplayerstats.php?player_id=", id)
  
  html <- url %>%
    polite::bow() %>%
    polite::scrape()
  
  player_full_name <- html %>% 
    rvest::html_nodes(".player_full_name") %>% 
    rvest::html_text() %>%
    tibble::enframe()
}

players <- map(13720, get_player_info)

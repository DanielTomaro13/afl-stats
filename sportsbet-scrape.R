library(rvest)
library(httr)
library(dplyr)

url <- "https://www.sportsbet.com.au/betting/australian-rules/afl/geelong-cats-v-hawthorn-9107388"

headers <- c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36',
  'Accept-Language' = 'en-US,en;q=0.9'
)

page <- GET(url, add_headers(headers)) %>%
  content("text") %>%
  read_html()

all_text <- page %>% html_nodes("span") %>% html_text()

disposal_markers <- which(grepl("Disposal Markets|To Get 20|To Get 25|First Disposal", all_text))
goal_markers <- which(grepl("Goal Scorer|To Score 2|To Score 3|1st Goal Scorer", all_text))

players_and_odds <- all_text[grepl("^[A-Z][a-z]+\\s[A-Z][a-z]+$", all_text) | grepl("^\\d+\\.\\d{2}$", all_text)]

player_indices <- which(grepl("^[A-Z][a-z]+\\s[A-Z][a-z]+$", players_and_odds))

player_odds_df <- data.frame(
  Player = character(),
  Odds = numeric(),
  Market = character(),
  stringsAsFactors = FALSE
)

disposal_start <- which(players_and_odds == "Bailey Smith")[1]
goal_start <- which(players_and_odds == "Jeremy Cameron")[1]

for (i in player_indices) {
  name <- players_and_odds[i]
  
  odds_candidate <- players_and_odds[(i+1):(i+3)]
  odds_vals <- odds_candidate[grepl("^\\d+\\.\\d{2}$", odds_candidate)]
  
  if (length(odds_vals) >= 1) {
    market_type <- ifelse(i >= disposal_start, "Disposal", "Goal")
    
    player_odds_df <- rbind(player_odds_df, data.frame(
      Player = name,
      Odds = as.numeric(odds_vals[1]),
      Market = market_type,
      stringsAsFactors = FALSE
    ))
  }
}

player_odds_df <- player_odds_df %>%
  arrange(Market, Player)

library(rvest)
library(httr)
library(dplyr)
library(stringr)

url <- "https://www.sportsbet.com.au/betting/australian-rules/afl/geelong-cats-v-hawthorn-9107388"

headers <- c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36',
  'Accept-Language' = 'en-US,en;q=0.9'
)

page <- GET(url, add_headers(headers)) %>%
  content("text") %>%
  read_html()

all_text <- page %>% html_nodes("span") %>% html_text()

player_odds_df <- data.frame(
  Player = character(),
  Odds = numeric(),
  Market = character(),
  Market_Subtype = character(),
  stringsAsFactors = FALSE
)

top_market <- NA
sub_market <- NA

i <- 1
while (i <= length(all_text)) {
  text <- all_text[i]
  
  # Detect top-level market
  if (grepl("Disposal Markets", text, ignore.case = TRUE)) {
    top_market <- "Disposal"
  } else if (grepl("Goal Scorer", text, ignore.case = TRUE)) {
    top_market <- "Goal"
  }
  
  # Detect sub-market
  if (grepl("To Get \\d+", text, ignore.case = TRUE)) {
    number <- str_extract(text, "\\d+")
    sub_market <- paste0(number, "+")
  } else if (grepl("To Score \\d+", text, ignore.case = TRUE)) {
    number <- str_extract(text, "\\d+")
    sub_market <- paste0(number, "+")
  }
  
  # Detect player name
  if (grepl("^[A-Z][a-z]+\\s[A-Z][a-z]+$", text)) {
    player <- text
    j <- i + 1
    
    while (j <= length(all_text) && grepl("^\\d+\\.\\d{2}$", all_text[j])) {
      odds <- as.numeric(all_text[j])
      
      player_odds_df <- rbind(player_odds_df, data.frame(
        Player = player,
        Odds = odds,
        Market = top_market,
        Market_Subtype = sub_market,
        stringsAsFactors = FALSE
      ))
      
      j <- j + 1
    }
    
    i <- j - 1
  }
  
  i <- i + 1
}

# Clean + sort
player_odds_df <- player_odds_df %>%
  arrange(Market, Market_Subtype, Player)

View(player_odds_df)

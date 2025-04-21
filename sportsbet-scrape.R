library(rvest)
library(httr)
library(dplyr)
library(stringr)

# Set the URL and headers
url <- "https://www.sportsbet.com.au/betting/australian-rules/afl/geelong-cats-v-hawthorn-9107388"
headers <- c('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36')

# Get the page content
page <- GET(url, add_headers(headers)) %>%
  content("text") %>%
  read_html()

# Get all span text elements
all_spans <- page %>% html_nodes("span") %>% html_text()

# Create empty data frame
player_odds_df <- data.frame(
  Player = character(),
  Odds = numeric(),
  Market = character(),
  Threshold = numeric(),
  Market_Type = character(),
  stringsAsFactors = FALSE
)

# Track the current market
current_market <- NA

# Loop through all spans
for (i in 1:length(all_spans)) {
  text <- all_spans[i]
  
  # Identify market headers
  if (text %in% c("To Score 1 or More Goals", "To Score 2 or More Goals", "To Score 3 or More Goals", 
                  "To Get 15 or More Disposals", "To Get 20 or More Disposals", "To Get 25 or More Disposals")) {
    current_market <- text
    next
  }
  
  if (!is.na(current_market) &&
      grepl("^[A-Za-z' ]+$", text) && 
      i+1 <= length(all_spans) && 
      grepl("^Last 5:", all_spans[i+1])) {
    
    player_name <- text
    
    for (j in (i+1):min(i+20, length(all_spans))) {
      if (grepl("^[0-9]+\\.[0-9]+$", all_spans[j])) {
        odds <- as.numeric(all_spans[j])
        threshold <- as.numeric(gsub("\\D", "", current_market))
        market_type <- ifelse(grepl("Disposals", current_market), "Disposals", "Goals")
        
        player_odds_df <- rbind(player_odds_df,
                                data.frame(
                                  Player = player_name,
                                  Odds = odds,
                                  Market = current_market,
                                  Threshold = threshold,
                                  Market_Type = market_type,
                                  stringsAsFactors = FALSE))
        break
      }
    }
  }
}

# Clean and arrange final output (no In_For or Probability)
player_odds_df <- player_odds_df %>%
  distinct() %>%
  arrange(Market_Type, Market, Player)

# View result
View(player_odds_df)

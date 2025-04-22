#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
player_stats <- fetch_player_stats_afltables(2003:2025)
colnames(player_stats)

player_stats <- player_stats %>% select(
  Date, Season, Round, Team, Player, ID, Kicks, Marks, Handballs, Disposals, Goals, Behinds, Hit.Outs, Tackles, Rebounds, Inside.50s,
  Clearances, Clangers, Frees.For, Frees.Against, Brownlow.Votes, Contested.Possessions, Uncontested.Possessions, Contested.Marks,
  Marks.Inside.50, One.Percenters, Bounces, Goal.Assists, Time.on.Ground, Age, Career.Games
  
)
colSums(is.na(player_stats))
player_stats <- player_stats %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))
colSums(is.na(player_stats))

stat_cols <- player_stats %>%
  select(where(is.numeric)) %>%
  select(-ID, -Season) %>%  
  colnames()

player_lagged <- player_stats %>%
  arrange(ID, Date) %>%
  group_by(ID, Player) %>%
  mutate(across(
    all_of(stat_cols),
    ~lag(cummean(.x), default = 0),
    .names = "avg_{.col}"
  )) %>%
  ungroup() %>%
  select(Date, Round, Season, Team, ID, Player, starts_with("avg_"))

has_na <- player_lagged %>%
  filter(if_any(everything(), is.na))

player_lagged <- na.omit(player_lagged)
colSums(is.na(player_lagged))
#####################################################
# Join back to player_stats to get actual values
player_stats <- player_stats %>%
  distinct(Date, Season, Round, Team, Player, ID, .keep_all = TRUE)
player_lagged <- player_lagged %>%
  distinct(Date, Season, Round, Team, Player, ID, .keep_all = TRUE)

player_model_data <- player_stats %>%
  inner_join(player_lagged, 
             by = c("Date", "Season", "Round", "Team", "Player", "ID"))
#####################################################
# Get last 3 average
player_rolling_3 <- player_stats %>%
  arrange(ID, Date) %>%
  group_by(ID, Player) %>%
  mutate(across(
    all_of(stat_cols),
    ~slide_dbl(.x, mean, .before = 3, .complete = FALSE, .partial = TRUE) %>% lag(default = 0),
    .names = "roll3_{.col}"
  )) %>%
  ungroup() %>%
  select(Date, Season, Round, Team, Player, ID, starts_with("roll3_"))

has_na <- player_rolling_3 %>%
  filter(if_any(everything(), is.na))

player_rolling_3 <- na.omit(player_rolling_3)
colSums(is.na(player_rolling_3))

player_model_data <- player_model_data %>%
  inner_join(player_rolling_3, by = c("Date", "Season", "Round", "Team", "Player", "ID"))
#####################################################
# Player Position
library(purrr)
seasons <- 2003:2025
player_details_all <- map_dfr(seasons, ~{
  fetch_player_details(season = .x, source = "footywire")
})

position <- player_details_all %>%
  select(first_name, surname, Position_1, Position_2) %>%
  mutate(
    Player = paste(first_name, surname),
    is_defender = as.integer(Position_1 == "Defender" | Position_2 == "Defender"),
    is_midfielder = as.integer(Position_1 == "Midfield" | Position_2 == "Midfield"),
    is_forward = as.integer(Position_1 == "Forward" | Position_2 == "Forward")
  ) %>%
  select(Player, is_defender, is_midfielder, is_forward) %>% 
  mutate(
    across(c(is_defender, is_midfielder, is_forward), ~replace_na(., 0))
  )

player_model_data <- player_model_data %>%
  left_join(position, by = "Player") %>%
  mutate(across(c(is_defender, is_midfielder, is_forward), ~replace_na(., 0)))
#####################################################
# Add some sort of feature for Team or Opposition performance e.g. higher predictions against worst teams
#####################################################
# Modelling
train_data <- player_model_data %>% filter(Season < 2024)
test_data  <- player_model_data %>% filter(Season == 2024)
#####################################################
# Linear Model
model_disposals <- lm(Disposals ~ ., data = train_data %>%
                        select(Disposals, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
model_goals <- lm(Goals ~ ., data = train_data %>%
                    select(Goals, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
model_marks <- lm(Marks ~ ., data = train_data %>%
                    select(Marks, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
model_tackles <- lm(Tackles ~ ., data = train_data %>%
                      select(Tackles, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
model_kicks <- lm(Kicks ~ ., data = train_data %>%
                    select(Kicks, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
model_clear <- lm(Clearances ~ ., data = train_data %>%
                    select(Clearances, starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward))
summary(model_disposals)
summary(model_goals)
summary(model_marks)
summary(model_tackles)
summary(model_kicks)
summary(model_clear)
# Go through models and drop redunant features such as disposals or non significant features, check out H20 package
#####################################################
library(xgboost)

train_xgb_model <- function(target_var) {
  y_train <- train_data[[target_var]]
  y_test  <- test_data[[target_var]]
  
  x_train <- train_data %>%
    select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward) %>%
    as.matrix()
  x_test <- test_data %>%
    select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward) %>%
    as.matrix()
  
  
  dtrain <- xgb.DMatrix(data = x_train, label = y_train)
  dtest  <- xgb.DMatrix(data = x_test, label = y_test)
  
  model <- xgboost(
    data = dtrain,
    nrounds = 100,
    objective = "reg:squarederror",
    verbose = 0
  )
  
  preds <- predict(model, newdata = x_test)
  
  list(model = model, predictions = preds)
}

# Target stats to model
target_stats <- c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")

# Store models and predictions
xgb_models <- list()
for (stat in target_stats) {
  result <- train_xgb_model(stat)
  xgb_models[[stat]] <- result$model
  test_data[[paste0("Predicted_", stat)]] <- result$predictions
}
#####################################################
library(Metrics)

for (stat in target_stats) {
  actual <- test_data[[stat]]
  predicted <- test_data[[paste0("Predicted_", stat)]]
  
  rmse_val <- rmse(actual, predicted)
  mae_val <- mae(actual, predicted)
  
  cat(stat, "- RMSE:", round(rmse_val, 2), "| MAE:", round(mae_val, 2), "\n")
}
#####################################################
# Predictions
future_lagged <- player_model_data %>%
  filter(Season == 2025, Round == 7) %>%
  distinct(ID, .keep_all = TRUE)


future_matrix <- future_lagged %>%
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward) %>%
  as.matrix()

for (stat in c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = future_matrix)
  future_lagged[[paste0("Predicted_", stat)]] <- pred
}

future_lagged_team <- future_lagged %>%
  filter(Team %in% c("Collingwood", "Essendon")) %>%
  select(
    Team, Player,
    starts_with("Predicted_")
  )

View(future_lagged_team)
#####################################################
# Comparision to a completed round
round_data <- player_model_data %>%
  filter(Season == 2025, Round == 6)

round_matrix <- round_data %>%
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward) %>%
  as.matrix()

for (stat in c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = round_matrix)
  round_data[[paste0("Predicted_", stat)]] <- round(pred, 0)
}

round_comparison <- round_data %>%
  select(
    Team, Player, ID,
    Disposals, Predicted_Disposals,
    Goals, Predicted_Goals,
    Marks, Predicted_Marks,
    Tackles, Predicted_Tackles,
    Kicks, Predicted_Kicks,
    Clearances, Predicted_Clearances
  ) %>% distinct(ID, .keep_all = TRUE)

target_stats <- c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")

for (stat in target_stats) {
  actual <- round_data[[stat]]
  predicted <- round_data[[paste0("Predicted_", stat)]]
  
  mae_val <- mae(actual, predicted)
  rmse_val <- rmse(actual, predicted)

  cat(paste0(stat, ":\n"))
  cat(sprintf("  MAE  = %.2f\n", mae_val))
  cat(sprintf("  RMSE = %.2f\n", rmse_val))
}
#####################################################
# Converting Predictions into probailities and odds
odds <- player_model_data %>% filter(Season == 2025, Round == 6)

odds_matrix <- odds %>%
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward) %>%
  as.matrix()

for (stat in c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = odds_matrix)
  odds[[paste0("Predicted_", stat)]] <- pred
}

odds <- odds %>%
  select(
    Team, Player, ID,
    Predicted_Disposals,
    Predicted_Goals,
    Predicted_Marks,
    Predicted_Tackles,
    Predicted_Kicks,
    Predicted_Clearances
  ) %>%
  distinct(ID, .keep_all = TRUE)
#####################################################
# Scraping Bookies
library(rvest)
library(httr)
library(dplyr)
library(stringr)

url <- "https://www.sportsbet.com.au/betting/australian-rules/afl/geelong-cats-v-hawthorn-9107388"
headers <- c('User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36')

page <- GET(url, add_headers(headers)) %>%
  content("text") %>%
  read_html()

all_spans <- page %>% html_nodes("span") %>% html_text()
print(all_spans)

player_odds_df <- data.frame(
  Player = character(),
  Odds = numeric(),
  Market = character(),
  Threshold = numeric(),
  Market_Type = character(),
  stringsAsFactors = FALSE
)

current_market <- NA

for (i in 1:length(all_spans)) {
  text <- all_spans[i]
  
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

player_odds_df <- player_odds_df %>%
  distinct() %>%
  arrange(Market_Type, Market, Player)

player_odds_df <- player_odds_df %>%
  mutate(Prob = 1 / Odds)

expected_goals <- player_odds_df %>%
  filter(Market_Type == "Goals") %>%
  arrange(Player, Threshold) %>%
  group_by(Player) %>%
  mutate(
    Next_Prob = lead(Prob, default = 0),
    Prob_Exact = Prob - Next_Prob,
    Mid_Value = ifelse(is.na(lead(Threshold)), Threshold + 1, (Threshold + lead(Threshold)) / 2)
  ) %>%
  summarise(In_For = round(sum(Prob_Exact * Mid_Value), 2), .groups = "drop")

expected_disposals <- player_odds_df %>%
  filter(Market_Type == "Disposals") %>%
  arrange(Player, Threshold) %>%
  group_by(Player) %>%
  mutate(
    Next_Prob = lead(Prob, default = 0),
    Prob_Exact = Prob - Next_Prob,
    Mid_Value = ifelse(is.na(lead(Threshold)), Threshold + 3, (Threshold + lead(Threshold)) / 2)
  ) %>%
  summarise(In_For_Disposals = round(sum(Prob_Exact * Mid_Value), 2), .groups = "drop")
#####################################################
# Comparision
model_infor <- future_lagged_team %>%
  select(Player, Team, Predicted_Disposals, Predicted_Goals)

bookie_infor <- player_odds_df %>%
  filter(Player %in% model_infor$Player) %>%
  filter(Market_Type %in% c("Disposals", "Goals")) %>%
  distinct(Player, Market_Type, In_For) %>%
  pivot_wider(names_from = Market_Type, values_from = In_For, names_prefix = "Bookie_")

compare_infor <- model_infor %>%
  left_join(bookie_infor, by = "Player") %>%
  select(Player, Team, 
         Pred_Disposals = Predicted_Disposals,
         Bookie_Disposals = Bookie_Disposals,
         Pred_Goals = Predicted_Goals,
         Bookie_Goals = Bookie_Goals)

View(compare_infor)
#####################################################
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
# Modelling
train_data <- player_model_data %>% filter(Season < 2024)
test_data  <- player_model_data %>% filter(Season == 2024)
#####################################################
# Linear Model
model_disposals <- lm(Disposals ~ ., data = train_data %>% select(Disposals, starts_with("avg_"), starts_with("roll3_")))
model_goals     <- lm(Goals ~ ., data = train_data %>% select(Goals, starts_with("avg_"), starts_with("roll3_")))
model_marks     <- lm(Marks ~ ., data = train_data %>% select(Marks, starts_with("avg_"), starts_with("roll3_")))
model_tackles   <- lm(Tackles ~ ., data = train_data %>% select(Tackles, starts_with("avg_"), starts_with("roll3_")))
model_kicks     <- lm(Kicks ~ ., data = train_data %>% select(Kicks, starts_with("avg_"), starts_with("roll3_")))
model_clear     <- lm(Clearances ~ ., data = train_data %>% select(Clearances, starts_with("avg_"), starts_with("roll3_")))

summary(model_disposals)
summary(model_goals)
summary(model_marks)
summary(model_tackles)
summary(model_kicks)
summary(model_clear)
#####################################################
library(xgboost)

train_xgb_model <- function(target_var) {
  y_train <- train_data[[target_var]]
  y_test  <- test_data[[target_var]]
  
  x_train <- train_data %>% select(starts_with("avg_"), starts_with("roll3_")) %>% as.matrix()
  x_test  <- test_data %>% select(starts_with("avg_"), starts_with("roll3_")) %>% as.matrix()
  
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
  filter(Season == 2025, Round == 6)

future_matrix <- future_lagged %>%
  select(starts_with("avg_"), starts_with("roll3_")) %>%
  as.matrix()

for (stat in c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = future_matrix)
  future_lagged[[paste0("Predicted_", stat)]] <- pred
}

future_lagged_team <- future_lagged %>%
  filter(Team %in% c("Geelong", "Hawthorn")) %>%
  select(
    Team, Player,
    starts_with("Predicted_")
  )

View(future_lagged_team)
#####################################################
# Comparision to a completed round
round_data <- player_model_data %>%
  filter(Season == 2025, Round == 5)

round_matrix <- round_data %>%
  select(starts_with("avg_"), starts_with("roll3_")) %>%
  as.matrix()

for (stat in c("Disposals", "Goals", "Marks", "Tackles", "Kicks", "Clearances")) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = round_matrix)
  round_data[[paste0("Predicted_", stat)]] <- round(pred, 0)
}

round_comparison <- round_data %>%
  select(
    Team, Player,
    Disposals, Predicted_Disposals,
    Goals, Predicted_Goals,
    Marks, Predicted_Marks,
    Tackles, Predicted_Tackles,
    Kicks, Predicted_Kicks,
    Clearances, Predicted_Clearances
  )

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
# Scrape TAB prices - how can i get in for?
#####################################################
# We need to be able to scrape prices for 1+, 2+ ... as much as we can to calculate in for to compare



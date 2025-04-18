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
model_lm <- lm(Marks ~ ., data = train_data %>% 
                 select(Marks, starts_with("avg_"), starts_with("roll3_")))
summary(model_lm)
#####################################################
library(xgboost)

x_train <- train_data %>% select(starts_with("avg_")) %>% as.matrix()
y_train <- train_data$Marks

x_test <- test_data %>% select(starts_with("avg_")) %>% as.matrix()
y_test <- test_data$Marks

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

model_xgb <- xgboost(
  data = dtrain,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

test_data$Predicted_Marks <- predict(model_xgb, newdata = x_test)
#####################################################
library(Metrics)

rmse <- rmse(test_data$Marks, test_data$Predicted_Marks)
mae <- mae(test_data$Marks, test_data$Predicted_Marks)

cat("RMSE:", rmse, "\nMAE:", mae)
#####################################################
# Predictions
future_lagged <- player_lagged %>% filter(Season == 2025, Round == 6)

future_lagged$Predicted_Marks <- predict(model_xgb, newdata = future_lagged %>% select(starts_with("avg_")) %>% as.matrix())

future_lagged_nm_carl <- future_lagged %>% filter(Team == 'Brisbane Lions' | Team == 'Collingwood') %>% 
  select(
    Team, Player, Predicted_Marks
  )
#####################################################
# Scrape TAB prices
library(rvest)
library(httr)
library(dplyr)

url <- "https://www.tab.com.au/sports/betting/AFL%20Football/competitions/AFL/matches/North%20Melbourne%20v%20Carlton"

headers <- c(
  'User-Agent' = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36'
)

response <- GET(url, add_headers(.headers = headers))

page <- content(response, "text") %>% read_html()

players <- page %>%
  html_nodes("td._e4e14m span._1eek9ib") %>%
  html_text()

odds_matrix <- page %>%
  html_nodes("td._18h3vh3 span._njbycu") %>%
  html_text()

n_cols <- 5

odds_df <- matrix(odds_matrix, ncol = n_cols, byrow = TRUE) %>%
  as.data.frame()

colnames(odds_df) <- c("2+ Marks", "4+ Marks", "6+ Marks", "8+ Marks", "10+ Marks")

tab_prices <- cbind(Player = players, odds_df)
tab_prices
#####################################################
# Getting In For
# Odds for Player X
odds <- c(`2+` = 1.03, `4+` = 1.60, `6+` = 3.40, `8+` = 11.00, `10+` = 41.00)

p_cum <- 1 / odds

p_exact <- c(
  `2` = p_cum["2+"],
  `4` = p_cum["4+"] - p_cum["6+"],
  `6` = p_cum["6+"] - p_cum["8+"],
  `8` = p_cum["8+"] - p_cum["10+"],
  `10+` = p_cum["10+"]
)
expected_marks <- sum(as.numeric(names(p_exact)) * p_exact)
#####################################################
#####################################################
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2) 
library(fitzRoy) 
library(slider)
#####################################################
# player_stats <- fetch_player_stats_afltables(2003:2025) # earliest year is 2003
# Read the Above in
# saveRDS(player_stats, file = "player_stats_2003_2025.rds")
player_stats <- readRDS("player_stats_2003_2025.rds")

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
# library(purrr)
# seasons <- 2022:2025
# player_details_all <- map_dfr(seasons, ~{
#   fetch_player_details(season = .x, source = "footywire")
# })
# saveRDS(player_details_all, file = "player_details_all.rds")
player_details_all <- readRDS("player_details_all.rds")

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
# Add some sort o feature for incredibly bad teams e.g. is_bottom_four
#####################################################
# is_debut season - player details has debutYear
# library(purrr)
# seasons <- 2003:2025
# debut_year <- map_dfr(seasons, ~{
#   fetch_player_details(season = .x, source = "AFL")
# })
# saveRDS(debut_year, file = "debut_year_2003_2025.rds")
debut_year <- readRDS("debut_year_2003_2025.rds")

debut_year <- debut_year %>%
  mutate(Player = paste(firstName, surname)) %>%
  select(Player, debutYear)

player_model_data <- player_model_data %>%
  left_join(debut_year, by = "Player") %>%
  mutate(
    is_debut_season = as.integer(debutYear == Season),
    is_debut_season = replace_na(is_debut_season, 0)
  )
#####################################################
# is_over_100_games _200 _300 
player_model_data <- player_model_data %>%
  mutate(
    is_over_100_games = as.integer(Career.Games >= 100),
    is_over_200_games = as.integer(Career.Games >= 200),
    is_over_300_games = as.integer(Career.Games >= 300)
  )
#####################################################
# is_home_game
# results <- fetch_results_afltables(2003:2025)
# colnames(results)
# saveRDS(results, file = "results_afltables_2003_2025.rds")
results <- readRDS("results_afltables_2003_2025.rds")


home_away_flags <- results %>%
  select(Date, Season, Round = Round.Number, Home.Team, Away.Team) %>%
  pivot_longer(
    cols = c(Home.Team, Away.Team),
    names_to = "HomeAway",
    values_to = "Team"
  ) %>%
  mutate(
    is_home_game = ifelse(HomeAway == "Home.Team", 1, 0),
    Round = as.character(Round)
  ) %>%
  select(Date, Season, Round, Team, is_home_game)

player_model_data <- player_model_data %>%
  left_join(home_away_flags, by = c("Date", "Season", "Round", "Team")) %>%
  mutate(is_home_game = replace_na(is_home_game, 0))
#####################################################
# played_last_game
player_appearances <- player_stats %>%
  arrange(ID, Date) %>%
  select(ID, Player, Date, Season, Round, Team)

player_appearances <- player_appearances %>%
  group_by(ID) %>%
  mutate(last_game_date = lag(Date)) %>%
  ungroup()

team_games <- player_stats %>%
  distinct(Team, Date, Season, Round) %>%
  arrange(Team, Date) %>%
  group_by(Team) %>%
  mutate(prev_game_date = lag(Date)) %>%
  ungroup()

player_appearances <- player_appearances %>%
  left_join(team_games, by = c("Team", "Season", "Date")) %>%
  mutate(
    played_last_game = as.integer(last_game_date == prev_game_date)
  )

player_appearances <- player_appearances %>%
  mutate(played_last_game = replace_na(played_last_game, 0))

player_model_data <- player_model_data %>%
  left_join(
    player_appearances %>% select(ID, Date, played_last_game),
    by = c("ID", "Date")
  ) %>%
  mutate(played_last_game = replace_na(played_last_game, 0))
#####################################################
# is_brownlow_winner
brownlow_winners <- c(
'Patrick Cripps',
'Lachie Neale',
'Ollie Wines',
'Nat Fyfe',
'Tom Mitchell',
'Dustin Martin',
'Patrick Dangerfield',
'Matt Pridis',
'Gary Ablett',
'Sam Mithcell',
'Trent Cotchin',
'Dane Swan',
'Chris Judd',
'Adam Cooney',
'Jimmy Bartel',
'Adam Goodes',
'Ben Cousins',
'Mark Ricciuto',
'Nathan Buckley',
'Simon Black',
'Jason Akermanis',
'Shane Woewodin',
'Shane Crawford',
'Robert Harvey',
'Michael Voss',
'James Hird'
)


player_model_data <- player_model_data %>% 
  mutate(is_brownlow_winner = ifelse(Player %in% brownlow_winners, 1, 0))
#####################################################
# is_bnf_winner - where can I find this information?
#####################################################
# is_stat_beast - does this player average x+ in a certain stat
# player_model_data <- player_model_data %>%
#   mutate(
#     is_disposal_beast = as.integer(avg_Disposals >= 25),
#     is_goal_beast = as.integer(avg_Goals >= 2),
#     is_tackle_beast = as.integer(avg_Tackles >= 5)
#   )
# player_model_data <- player_model_data %>%
#   mutate(
#     is_stat_beast = as.integer(is_disposal_beast + is_goal_beast + is_tackle_beast >= 1)
#   )
#####################################################
# is_wet_weather - RAIN or WINDY in weatherType
# weather <- fetch_results_afl(2003:2025)
# Read the above in
# saveRDS(weather, file = "weather_afl_2003_2025.rds")
# weather <- readRDS("weather_afl_2003_2025.rds")
# weather <- weather %>% select(match.date, round.roundNumber, match.homeTeam.name, match.awayTeam.name,
#                               weather.description, weather.tempInCelsius, weather.weatherType)
# 
# weather_flags <- weather %>%
#   mutate(
#     is_wet_weather = as.integer(grepl("RAIN|WINDY", weather.weatherType, ignore.case = TRUE))
#   ) %>%
#   rename(
#     Date = match.date,
#     Home.Team = match.homeTeam.name,
#     Away.Team = match.awayTeam.name
#   ) %>%
#   select(Date, Home.Team, Away.Team, is_wet_weather)
# 
# weather_flags_long <- weather_flags %>%
#   pivot_longer(cols = c(Home.Team, Away.Team),
#                names_to = "HomeAway",
#                values_to = "Team") %>%
#   select(Date, Team, is_wet_weather)
# 
# player_model_data <- player_model_data %>%
#   left_join(weather_flags_long, by = c("Date", "Team")) %>%
#   mutate(is_wet_weather = replace_na(is_wet_weather, 0))
#####################################################
# is_key_forward or is_key_back or midfielder_forward - this is not adding properly - memory limit
# library(purrr)
# seasons <- 2003:2025
# player_details_specific <- map_dfr(seasons, ~{
#   fetch_player_details(season = .x, source = "AFL")
# })
# saveRDS(player_details_specific, "player_details_afl_2003_2025.rds")
player_details_specific <- readRDS("player_details_afl_2003_2025.rds")

detailed_position <- player_details_specific %>%
  select(firstName, surname, position) %>% 
  mutate(
    Player = paste(firstName, surname),
    is_key_defender = as.integer(position == "KEY_DEFENDER"),
    is_midfielder_forward = as.integer(position == "MIDFIELDER_FORWARD"),
    is_key_forward = as.integer(position == "KEY_FORWARD")
  ) %>%
  select(Player, is_key_defender, is_midfielder_forward, is_key_forward)

rm(player_stats, player_lagged, player_rolling_3,
   debut_year, position, results, player_appearances, prem_coach, player_details_specific, train_data, test_data)
gc()

player_model_data <- player_model_data %>%
  left_join(detailed_position, by = "Player", relationship = 'many-to-many') %>%
  mutate(across(c(is_key_defender, is_midfielder_forward, is_key_forward)))
#####################################################
# premiership_coach
premiership_coaches <- c(
  "Beveridge, Luke",
  "Blight, Malcolm",
  "Clarkson, Alastair",
  "Goodwin, Simon",
  "Hardwick, Damien",
  "Jeans, Allan",
  "Longmire, John",
  "Malthouse, Michael",
  "Malthouse, Mick",
  "Matthews, Leigh",
  "McRae, Craig",
  "Pagan, Denis",
  "Parkin, David",
  "Roos, Paul",
  "Scott, Chris",
  "Sheedy, Kevin",
  "Thompson, Mark",
  "Walls, Robert",
  "Williams, Mark"
)
# coach <- fetch_player_stats_afltables(2003:2025)
# # Read the above in
# saveRDS(coach, file = "player_stats_as_coach_data_2003_2025.rds")
coach <- readRDS("player_stats_as_coach_data_2003_2025.rds")


team_coach <- coach %>% select(Season, Round, Date, Playing.for, Coach) %>% 
  rename(Team = Playing.for)


prem_coach <- team_coach %>%
  select(Date, Season, Round, Team, Coach) %>%
  distinct() %>%  # So we don't duplicate teams - can maybe remove this
  mutate(is_premiership_coach = ifelse(Coach %in% premiership_coaches, 1, 0))

prem_coach <- prem_coach %>%
  mutate(Round = as.character(Round))

player_model_data <- player_model_data %>%
  left_join(
    prem_coach %>% select(Date, Season, Round, Team, is_premiership_coach),
    by = c("Date", "Season", "Round", "Team")
  ) %>%
  mutate(is_premiership_coach = replace_na(is_premiership_coach, 0))
#####################################################
# fetch_coaches_votes - extremely long load times
# coaches_votes <- fetch_coaches_votes(2003:2025)
# Read the above in
# saveRDS(coaches_votes, "coaches_votes_2003_2025.rds")
# coaches_votes <- readRDS("coaches_votes_2003_2025.rds")
# colnames(coaches_votes)
# 
# votes_roll3 <- coaches_votes %>%
#   arrange(Player, Date) %>%
#   group_by(Player) %>%
#   mutate(roll3_coach_votes = slide_dbl(Votes, mean, .before = 3, .complete = FALSE, .partial = TRUE)) %>%
#   ungroup() %>%
#   select(Date, Player, roll3_coach_votes)
# 
# votes_cumavg <- coaches_votes %>%
#   arrange(Player, Date) %>%
#   group_by(Player) %>%
#   mutate(avg_coach_votes = lag(cummean(Votes), default = 0)) %>%
#   ungroup() %>%
#   select(Date, Player, avg_coach_votes)
# 
# player_model_data <- player_model_data %>%
#   left_join(votes_roll3, by = c("Date", "Player")) %>%
#   left_join(votes_cumavg, by = c("Date", "Player")) %>%
#   mutate(across(c(roll3_coach_votes, avg_coach_votes), ~replace_na(., 0)))
#####################################################
# Team Stats
# team_avg_stats <- player_stats %>%
#   arrange(Team, Date) %>%
#   group_by(Team) %>%
#   mutate(
#     avg_Kicks_Team = lag(cummean(Kicks)),
#     avg_Marks_Team = lag(cummean(Marks)),
#     avg_Handballs_Team = lag(cummean(Handballs)),
#     avg_Disposals_Team = lag(cummean(Disposals)),
#     avg_Goals_Team = lag(cummean(Goals)),
#     avg_Behinds_Team = lag(cummean(Behinds)),
#     avg_Tackles_Team = lag(cummean(Tackles)),
#     avg_Rebounds_Team = lag(cummean(Rebounds)),
#     avg_Inside.50s_Team = lag(cummean(Inside.50s)),
#     avg_Clearances_Team = lag(cummean(Clearances)),
#     avg_Clangers_Team = lag(cummean(Clangers)),
#     avg_Brownlow.Votes_Team = lag(cummean(Brownlow.Votes)),
#     avg_Contested.Possessions_Team = lag(cummean(Contested.Possessions)),
#     avg_Uncontested.Possessions_Team = lag(cummean(Uncontested.Possessions)),
#     avg_Contested.Marks_Team = lag(cummean(Contested.Marks)),
#     avg_Marks.Inside.50_Team = lag(cummean(Marks.Inside.50)),
#     avg_One.Percenters_Team = lag(cummean(One.Percenters)),
#     avg_Goal.Assists_Team = lag(cummean(Goal.Assists)),
#     avg_Time.on.Ground_Team = lag(cummean(Time.on.Ground)),
#     avg_Age_Team = lag(cummean(Age)),
#     avg_Career_Games_Team = lag(cummean(Career.Games)),
#     
#     roll3_Kicks_Team = lag(slide_dbl(Kicks, mean, .before = 2, .complete = TRUE)),
#     roll3_Marks_Team = lag(slide_dbl(Marks, mean, .before = 2, .complete = TRUE)),
#     roll3_Handballs_Team = lag(slide_dbl(Handballs, mean, .before = 2, .complete = TRUE)),
#     roll3_Disposals_Team = lag(slide_dbl(Disposals, mean, .before = 2, .complete = TRUE)),
#     roll3_Goals_Team = lag(slide_dbl(Goals, mean, .before = 2, .complete = TRUE)),
#     roll3_Behinds_Team = lag(slide_dbl(Behinds, mean, .before = 2, .complete = TRUE)),
#     roll3_Tackles_Team = lag(slide_dbl(Tackles, mean, .before = 2, .complete = TRUE)),
#     roll3_Rebounds_Team = lag(slide_dbl(Rebounds, mean, .before = 2, .complete = TRUE)),
#     roll3_Inside.50s_Team = lag(slide_dbl(Inside.50s, mean, .before = 2, .complete = TRUE)),
#     roll3_Clearances_Team = lag(slide_dbl(Clearances, mean, .before = 2, .complete = TRUE)),
#     roll3_Clangers_Team = lag(slide_dbl(Clangers, mean, .before = 2, .complete = TRUE)),
#     roll3_Brownlow.Votes_Team = lag(slide_dbl(Brownlow.Votes, mean, .before = 2, .complete = TRUE)),
#     roll3_Contested.Possessions_Team = lag(slide_dbl(Contested.Possessions, mean, .before = 2, .complete = TRUE)),
#     roll3_Uncontested.Possessions_Team = lag(slide_dbl(Uncontested.Possessions, mean, .before = 2, .complete = TRUE)),
#     roll3_Contested.Marks_Team = lag(slide_dbl(Contested.Marks, mean, .before = 2, .complete = TRUE)),
#     roll3_Marks.Inside.50_Team = lag(slide_dbl(Marks.Inside.50, mean, .before = 2, .complete = TRUE)),
#     roll3_One.Percenters_Team = lag(slide_dbl(One.Percenters, mean, .before = 2, .complete = TRUE)),
#     roll3_Goal.Assists_Team = lag(slide_dbl(Goal.Assists, mean, .before = 2, .complete = TRUE)),
#     roll3_Time.on.Ground_Team = lag(slide_dbl(Time.on.Ground, mean, .before = 2, .complete = TRUE)),
#     roll3_Age_Team = lag(slide_dbl(Age, mean, .before = 2, .complete = TRUE)),
#     roll3_Career_Games_Team = lag(slide_dbl(Career.Games, mean, .before = 2, .complete = TRUE))
#   ) %>%
#   group_by(Date, Season, Round, Team) %>%
#   summarise(across(starts_with("avg_"), mean, na.rm = TRUE),
#             across(starts_with("roll3_"), mean, na.rm = TRUE),
#             .groups = "drop")

# Check Structure of Round in Both Datasets
# str(team_avg_stats)
# str(player_model_data)
# player_model_data <- team_avg_stats %>%
#   left_join(team_avg_stats, by = c("Date", "Season", "Round", "Team"))

#####################################################
saveRDS(player_model_data, "player_model_data.rds")
player_model_data <- readRDS("player_model_data.rds")

# Modelling
train_data <- player_model_data %>% filter(Season < 2024)
saveRDS(train_data, "train_data.rds")
train_data <- readRDS("train_data.rds")
test_data  <- player_model_data %>% filter(Season == 2024)
saveRDS(test_data, "test_data.rds")
test_data <- readRDS("test_data.rds")


rm(player_stats, player_lagged, player_rolling_3, detailed_position,
   debut_year, position, results, player_appearances, prem_coach, coach, has_na, home_away_flags, player_details_all,
   player_details_specific, team_coach, team_games)
gc()
#####################################################
# Linear Model
# Disposals Model
model_disposals <- lm(Disposals ~ ., data = train_data %>%
                        select(
                          Disposals,
                          starts_with("avg_"),
                          starts_with("roll3_"),
                          is_defender, is_midfielder, is_forward,
                          is_home_game, played_last_game,
                          is_over_100_games, is_over_200_games, is_over_300_games,
                          is_brownlow_winner,
                          #is_key_defender, is_key_forward, is_midfielder_forward,
                          is_premiership_coach,
                          is_debut_season
                        ))

# Goals Model
model_goals <- lm(Goals ~ ., data = train_data %>%
                    select(
                      Goals,
                      starts_with("avg_"),
                      starts_with("roll3_"),
                      is_defender, is_midfielder, is_forward,
                      is_home_game, played_last_game,
                      is_over_100_games, is_over_200_games, is_over_300_games,
                      is_brownlow_winner,
                      #is_key_defender, is_key_forward, is_midfielder_forward,
                      is_premiership_coach,
                      is_debut_season
                    ))

# Marks Model
model_marks <- lm(Marks ~ ., data = train_data %>%
                    select(
                      Marks,
                      starts_with("avg_"),
                      starts_with("roll3_"),
                      is_defender, is_midfielder, is_forward,
                      is_home_game, played_last_game,
                      is_over_100_games, is_over_200_games, is_over_300_games,
                      is_brownlow_winner,
                      #is_key_defender, is_key_forward, is_midfielder_forward,
                      is_premiership_coach,
                      is_debut_season
                    ))

# Tackles Model
model_tackles <- lm(Tackles ~ ., data = train_data %>%
                    select(
                      Tackles,
                      starts_with("avg_"),
                      starts_with("roll3_"),
                      is_defender, is_midfielder, is_forward,
                      is_home_game, played_last_game,
                      is_over_100_games, is_over_200_games, is_over_300_games,
                      is_brownlow_winner,
                      #is_key_defender, is_key_forward, is_midfielder_forward,
                      is_premiership_coach,
                      is_debut_season
                    ))

# Kicks Model
model_kicks <- lm(Kicks ~ ., data = train_data %>%
                    select(
                      Kicks,
                      starts_with("avg_"),
                      starts_with("roll3_"),
                      is_defender, is_midfielder, is_forward,
                      is_home_game, played_last_game,
                      is_over_100_games, is_over_200_games, is_over_300_games,
                      is_debut_season, is_brownlow_winner,
                      #is_key_defender, is_key_forward, is_midfielder_forward,
                      is_premiership_coach,
                      is_debut_season
                    ))

# Clearances Model
model_clear <- lm(Clearances ~ ., data = train_data %>%
                    select(
                      Clearances,
                      starts_with("avg_"),
                      starts_with("roll3_"),
                      is_defender, is_midfielder, is_forward,
                      is_home_game, played_last_game,
                      is_over_100_games, is_over_200_games, is_over_300_games,
                      is_debut_season, is_brownlow_winner,
                      #is_key_defender, is_key_forward, is_midfielder_forward,
                      is_premiership_coach,
                      is_debut_season
                    ))

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
    select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward, is_home_game, played_last_game, is_over_100_games, 
           is_over_200_games, is_over_300_games, is_brownlow_winner,
           #is_key_defender, is_key_forward, is_midfielder_forward,
           is_premiership_coach,
           is_debut_season
           ) %>%
    as.matrix()
  x_test <- test_data %>%
    select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward, is_home_game, played_last_game, is_over_100_games, 
           is_over_200_games, is_over_300_games, is_brownlow_winner, 
           #is_key_defender, is_key_forward, is_midfielder_forward,
           is_premiership_coach,
           is_debut_season
           ) %>%
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
target_stats <- c("Disposals"
                  , "Goals", "Marks", "Tackles"
                  #, "Kicks", "Clearances"
                  )

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
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward, is_home_game, played_last_game, is_over_100_games, 
         is_over_200_games, is_over_300_games, is_brownlow_winner, 
         #is_key_defender, is_key_forward, is_midfielder_forward,
         is_premiership_coach,
         is_debut_season
         ) %>%
  as.matrix()

for (stat in c("Disposals"
               , "Goals", "Marks", "Tackles"
               #, "Kicks", "Clearances"
               )) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = future_matrix)
  future_lagged[[paste0("Predicted_", stat)]] <- pred
}

future_lagged_team <- future_lagged %>%
  filter(Team %in% c("Richmond", "Melbourne")) %>%
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
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward, is_home_game, played_last_game, is_over_100_games, 
         is_over_200_games, is_over_300_games, is_brownlow_winner, 
         #is_key_defender, is_key_forward, is_midfielder_forward,
         is_premiership_coach,
         is_debut_season
         ) %>%
  as.matrix()

for (stat in c("Disposals"
               , "Goals", "Marks", "Tackles"
               #, "Kicks", "Clearances"
               )) {
  model <- xgb_models[[stat]]
  pred <- predict(model, newdata = round_matrix)
  round_data[[paste0("Predicted_", stat)]] <- round(pred, 0)
}

round_comparison <- round_data %>%
  select(
    Team, Player, ID,
    Disposals,
    Predicted_Disposals,
    Goals, Predicted_Goals,
    Marks, Predicted_Marks,
    Tackles, Predicted_Tackles
    #,
    # Kicks, Predicted_Kicks,
    # Clearances, Predicted_Clearances
  ) %>% distinct(ID, .keep_all = TRUE)

target_stats <- c("Disposals"
                  , "Goals", "Marks", "Tackles"
                  #, "Kicks", "Clearances"
                  )

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
odds <- player_model_data %>% filter(Season == 2025, Round == 7)

odds_matrix <- odds %>%
  select(starts_with("avg_"), starts_with("roll3_"), is_defender, is_midfielder, is_forward, is_home_game, played_last_game, is_over_100_games, 
         is_over_200_games, is_over_300_games, is_brownlow_winner, 
         #is_key_defender, is_key_forward, is_midfielder_forward,
         is_premiership_coach,
         is_debut_season
         ) %>%
  as.matrix()

for (stat in c("Disposals"
               , "Goals", "Marks", "Tackles"
               #, "Kicks", "Clearances"
               )) {
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
    Predicted_Tackles
    #,
    # Predicted_Kicks,
    # Predicted_Clearances
  ) %>%
  distinct(ID, .keep_all = TRUE)
#####################################################
# # # Use your actual value from model summary
resid_sds <- list(
  Disposals = sd(residuals(model_disposals)),
  Goals = sd(residuals(model_goals)),
  Marks = sd(residuals(model_marks)),
  Tackles = sd(residuals(model_tackles))
  #,
  # Kicks = sd(residuals(model_kicks)),
  # Clearances = sd(residuals(model_clear))
)

thresholds <- list(
Disposals = c(15, 20, 25, 30),
   Goals = c(1, 2, 3),
   Marks = c(4, 6, 8),
   Tackles = c(4, 6, 8)
#,
   # Kicks = c(10, 15, 20),
   # Clearances = c(4, 6, 8)
 )

for (stat in names(thresholds)) {
  for (thresh in thresholds[[stat]]) {
    prob_col <- paste0("Prob_", thresh, "_", stat)
    odds_col <- paste0("Odds_", thresh, "_", stat)
    pred_col <- paste0("Predicted_", stat)

    odds[[prob_col]] <- 1 - pnorm(thresh, mean = odds[[pred_col]], sd = resid_sds[[stat]])
    odds[[odds_col]] <- round(1 / odds[[prob_col]], 2)
  }
}

odds <- odds %>% filter(Team == 'Richmond' | Team == 'Melbourne') %>% select(Odds_20_Disposals, Odds_30_Disposals, Odds_1_Goals, Odds_3_Goals,
                                                       Odds_6_Marks, Odds_8_Marks, Odds_6_Tackles, Odds_8_Tackles, Player)

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
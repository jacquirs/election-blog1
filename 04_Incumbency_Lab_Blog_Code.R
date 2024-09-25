#' @title GOV 1347: Week 4 (Incumbency) Laboratory Session
#' @author Matthew E. Dardet
#' @date September 25, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(car)
library(caret)
library(CVXR)
library(glmnet)
library(kableExtra)
library(maps)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(viridis)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read, merge, and process data.
####----------------------------------------------------------#

# Read incumbency/vote data.
d_vote <- read_csv("popvote_1948-2020.csv")
d_state_vote <- read_csv("state_popvote_1948_2020.csv")
d_vote$party[d_vote$party == "democrat"] <- "DEM"
d_vote$party[d_vote$party == "republican"] <- "REP"

# Read economic data.
d_econ <- read_csv("fred_econ.csv") |> 
  filter(quarter == 2)

# Read polling and election results data. 
d_pollav_natl <- read_csv("national_polls_1968-2024.csv")
d_pollav_state <- read_csv("state_polls_1968-2024.csv")

# Shape and merge polling and election data using November polls. 
d_poll_nov <- d_vote |> 
  left_join(d_pollav_natl |> 
              group_by(year, party) |> 
              top_n(1, poll_date) |> 
              select(-candidate), 
            by = c("year", "party")) |> 
  rename(nov_poll = poll_support) |> 
  filter(year <= 2020) |> 
  drop_na()

# Create dataset of polling average by week until the election. 
d_poll_weeks <- d_pollav_natl |> 
  group_by(year, party, weeks_left) |>
  summarize(mean_poll_week = mean(poll_support)) |> 
  filter(weeks_left <= 30) |> 
  pivot_wider(names_from = weeks_left, values_from = mean_poll_week) |> 
  left_join(d_vote, by = c("year", "party"))

####----------------------------------------------------------#
#### Descriptive statistics on the incumbency advantage. 
####----------------------------------------------------------#

# How many post-war elections (18 and 2024) have there been 
# where an incumbent president won? 
d_vote |> 
  filter(winner) |> 
  select(year, win_party = party, win_cand = candidate) |> 
  mutate(win_party_last = lag(win_party, order_by = year),
         win_cand_last = lag(win_cand, order_by = year)) |> 
  mutate(reelect_president = win_cand_last == win_cand) |> 
  filter(year > 1948 & year < 2024) |> 
  group_by(reelect_president) |> 
  summarize(N = n()) |> 
  mutate(Percent = round(N/sum(N) * 100, 2)) |>
  as.data.frame()

# A different way of assessing the incumbency advantage 
# (out of 11 elections where there was at least one incumbent running). 
inc_tab <- d_vote |> 
  filter(year > 1948 & year < 2024) |>
  select(year, party, candidate, incumbent, winner) |> 
  pivot_wider(names_from = party, 
              values_from = c(candidate, incumbent, winner)) |> 
  filter(incumbent_DEM == TRUE | incumbent_REP == TRUE)


cat(paste0("Elections with At Least One Incumbent Running: ", nrow(inc_tab), "\n",
   "Incumbent Victories: ", (sum(inc_tab$incumbent_REP & inc_tab$winner_REP) + 
                             sum(inc_tab$incumbent_DEM & inc_tab$winner_DEM)), "\n",
    "Percentage: ", round((sum(inc_tab$incumbent_REP & inc_tab$winner_REP) + 
                           sum(inc_tab$incumbent_DEM & inc_tab$winner_DEM))/
                           nrow(inc_tab)*100, 2)))

# In the six elections since 2000?
inc_tab |> 
  filter(year >= 2000)
       
# How many post-war elections have there been where the incumbent *party* won? 
d_vote |> 
  filter(winner) |> 
  select(year, win_party = party, win_cand = candidate) |> 
  mutate(win_party_last = lag(win_party, order_by = year),
         win_cand_last = lag(win_cand, order_by = year)) |> 
  mutate(reelect_party = win_party_last == win_party) |> 
  filter(year > 1948 & year < 2024) |> 
  group_by(reelect_party) |> 
  summarize(N = n()) |> 
  mutate(Percent = round(N/sum(N) * 100, 2)) |>
  as.data.frame()

# How many post-war elections have there been where winner served in 
# previous administration?
100*round(prop.table(table(`prev_admin` = d_vote$prev_admin[d_vote$year > 1948 & 
                                     d_vote$year < 2024 & 
                                     d_vote$winner == TRUE])), 4)

####----------------------------------------------------------#
#### Pork analysis. 
####----------------------------------------------------------#
 
# Read federal grants dataset from Kriner & Reeves (2008). 
d_pork_state <- read_csv("fedgrants_bystate_1988-2008.csv")

# What strategy do presidents pursue? 
d_pork_state |> 
  filter(!is.na(state_year_type)) |> 
  group_by(state_year_type) |>
  summarize(mean_grant = mean(grant_mil, na.rm = T), se_grant = sd(grant_mil, na.rm = T)/sqrt(n())) |> 
  ggplot(aes(x = state_year_type, y = mean_grant, ymin = mean_grant-1.96*se_grant, ymax = mean_grant+1.96*se_grant)) + 
  coord_flip() + 
  geom_bar(stat = "identity", fill = "chartreuse4") + 
  geom_errorbar(width = 0.2) + 
  labs(x = "Type of State & Year", 
       y = "Federal Grant Spending (Millions of $)", 
       title = "Federal Grant Spending (Millions $) by State Election Type") + 
  theme_minimal() + 
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

# Do presidents strategize for their successor as well? 
d_pork_state |> 
  filter(!is.na(state_year_type2)) |> 
  group_by(state_year_type2) |>
  summarize(mean_grant = mean(grant_mil, na.rm = T), se_grant = sd(grant_mil, na.rm = T)/sqrt(n())) |> 
  ggplot(aes(x = state_year_type2, y = mean_grant, ymin = mean_grant-1.96*se_grant, ymax = mean_grant+1.96*se_grant)) + 
  coord_flip() + 
  geom_bar(stat = "identity", fill = "chartreuse4") + 
  geom_errorbar(width = 0.2) + 
  labs(x = "Type of State & Year", 
       y = "Federal Grant Spending (Millions of $)", 
       title = "Federal Grant Spending (Millions $) by State Election Type") + 
  theme_minimal() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

# Pork county model. 
d_pork_county <- read_csv("fedgrants_bycounty_1988-2008.csv")

pork_mod_county_1 <- lm(dvoteswing_inc  ~ dpct_grants*comp_state + as.factor(year), 
                      d_pork_county)
summary(pork_mod_county_1)

pork_mod_county_2 <- lm(dvoteswing_inc ~ dpct_grants*comp_state + as.factor(year) +
                          dpc_income + inc_ad_diff + inc_campaign_diff + 
                          dhousevote_inc + iraq_cas2004 + iraq_cas2008 + 
                          dpct_popl,
                        data = d_pork_county)
summary(pork_mod_county_2)

# Pork state model. 
d_pork_state_model <- d_state_vote |>
  mutate(state_abb = state.abb[match(d_state_vote$state, state.name)]) |>
  inner_join(d_pork_state, by = c("year", "state_abb")) |>
  left_join(d_vote, by = "year") |>
  filter(incumbent_party == TRUE) |>
  mutate(inc_pv2p = ifelse(party == "REP", R_pv2p, D_pv2p)) |>
  mutate(is_comp = case_when(state_year_type == "swing + election year" ~ 1,
                             .default = 0)) |>
  group_by(state) |>
  mutate(change_grant_mil = (1-grant_mil/(lag(grant_mil, n = 1)))*100,
         change_inc_pv2p = (1-inc_pv2p/(lag(inc_pv2p, n = 1)))*100) |>
  ungroup() |>
  select(state, year, is_comp, change_grant_mil, change_inc_pv2p)

pork_state_mod <- lm(change_inc_pv2p ~ is_comp*change_grant_mil + as.factor(year),
                     data = d_pork_state_model)
summary(pork_state_mod)

####----------------------------------------------------------#
#### Time for a change model. 
####----------------------------------------------------------#

# Join data for time for change model.
d_tfc_train <- d_vote |> 
  left_join(d_econ, by = "year") |> 
  filter(incumbent_party) |>
  mutate(incumbent = as.numeric(incumbent))

# Estimate time for change model through 2016.
tfc_mod_2016 <- lm(pv2p ~ GDP_growth_quarterly + incumbent + juneapp, 
                   data = subset(d_tfc_train, year < 2020))
summary(tfc_mod_2016)

# Estimate simplified time for change model for 2020. 
# https://www-cambridge-org.ezp-prod1.hul.harvard.edu/core/services/aop-cambridge-core/content/view/47BBC0D5A2B7913DBB37FDA0542FD7E8/S1049096520001389a.pdf/its-the-pandemic-stupid-a-simplified-model-for-forecasting-the-2020-presidential-election.pdf
tfc_mod_2020 <- lm(pv2p ~ juneapp, 
                   data = subset(d_tfc_train, year < 2024))
summary(tfc_mod_2020)

# Evaluate models and compare them to your own models/predictions. 
# TODO: 

####----------------------------------------------------------#
#### Expert predictions. 
####----------------------------------------------------------#

# Read expert prediction data. 
# Read data from Cook Political Report. 
# Years: 1988-2020
# IMPORTANT: Please do not commit/push this data to GitHub or share it anywhere outside of this course!
d_cook <- read_csv("CPR_EC_Ratings.csv")[,-1] |> 
  mutate(rating_numeric = case_when(Rating == "Solid D" ~ 1,
                                    Rating == "Likely D" ~ 2,
                                    Rating == "Lean D" ~ 3,
                                    Rating == "Toss Up" ~ 4,
                                    Rating == "Lean R" ~ 5,
                                    Rating == "Likely R" ~ 6,
                                    Rating == "Solid R" ~ 7)) |> 
  mutate(solid_D = as.numeric(rating_numeric == 1),
         likely_D = as.numeric(rating_numeric == 2),
         lean_D = as.numeric(rating_numeric == 3),
         toss_up = as.numeric(rating_numeric == 4),
         lean_R = as.numeric(rating_numeric == 5),
         likely_R = as.numeric(rating_numeric == 6),
         solid_R = as.numeric(rating_numeric == 7))
# N.B. Read important note above!

# Read data from Sabato's Crystal Ball.
# Years: 2004-2024
d_sabato <- read_csv("sabato_crystal_ball_ratings.csv") |> 
  rename(state_abb = state)

state_abb_xwalk <- d_state_vote |>
  mutate(state_abb = state.abb[match(d_state_vote$state, state.name)]) |> 
  select(state, state_abb) |> 
  distinct() 
state_abb_xwalk[51,]$state <- "District of Columbia"
state_abb_xwalk[51,]$state_abb <- "DC"

d_sabato <- d_sabato |> 
  left_join(state_abb_xwalk, by = "state_abb") |>
  mutate(safe_D = as.numeric(rating == 1),
         likely_D = as.numeric(rating == 2),
         lean_D = as.numeric(rating == 3),
         toss_up = as.numeric(rating == 4),
         lean_R = as.numeric(rating == 5),
         likely_R = as.numeric(rating == 6),
         safe_R = as.numeric(rating == 7))
# Ratings: 
# 1: Safe Democrat
# 2: Likely Democrat
# 3: Lean Democrat
# 4: Toss Up
# 5: Lean Republican
# 6: Likely Republican
# 7: Safe Republican

# 2020 Comparison. 
d_sabato_2020 <- d_sabato |> 
  filter(year == 2020) |> 
  select(state, sabato_rating = rating)

d_expert_2020 <- d_cook |> 
  filter(Cycle == 2020) |> 
  select(state = State, cook_rating = rating_numeric) |> 
  left_join(d_sabato_2020, by = "state") |> 
  mutate(rating_match = as.numeric(cook_rating == sabato_rating))

d_expert_2020$rating_match |> table()

d_expert_2020$state[d_expert_2020$rating_match == 0 & !is.na(d_expert_2020$sabato_rating)]
# Why the NAs? Cook makes ratings for Maine and Nebraska districts separately. 
# These may be important for the 2024 election, but are difficult to find data for. 

d_expert_2020 <- d_expert_2020 |> 
  drop_na()

# Compare rating mismatches for 2020.
d_expert_2020[d_expert_2020$state %in% c(d_expert_2020$state[d_expert_2020$rating_match == 0]),]

# Merge in 2020 state vote data. 
d_state_vote_2020 <- d_state_vote |> 
  filter(year == 2020) |> 
  select(state, year, R_pv2p, D_pv2p) |> 
  mutate(margin = D_pv2p - R_pv2p, 
         winner = ifelse(D_pv2p > R_pv2p, "D", "R"))
d_state_vote_2020[d_state_vote_2020$state == "District Of Columbia",]$state <- "District of Columbia"

d_expert_2020 <- d_expert_2020 |>
  left_join(d_state_vote_2020, by = "state")

# See which expert group was more accurate for 2020 (counting toss ups as misses). 
d_expert_2020 <- d_expert_2020 |> 
  mutate(cook_correct = as.numeric((winner == "D" & cook_rating < 4) | 
                                   (winner == "R" & cook_rating > 4)),
         sabato_correct = as.numeric((winner == "D" & sabato_rating < 4) | 
                                     (winner == "R" & sabato_rating > 4)))

d_expert_2020 |>
  select(cook_correct, sabato_correct) |> 
  colMeans()

# Which states did Cook miss? 
d_expert_2020[d_expert_2020$cook_correct == 0,]$state
d_expert_2020[d_expert_2020$cook_correct == 0,] |> view()

# Sabato? 
d_expert_2020[d_expert_2020$sabato_correct == 0,]$state
d_expert_2020[d_expert_2020$sabato_correct == 0,] |> view()

# 2024 Comparison:

# 2024 toss-ups? 
# 7 from Cook: https://www.cookpolitical.com/ratings/presidential-race-ratings

# 191 EV (Solid D) + 34 (Likely D) + 1 (Lean D) = 226 EV Dem
# 93 Toss Up EV
  # 11 AZ
  # 16 GA
  # 15 MI
  # 6 NV
  # 16 NC
  # 19 PA
  # 10 WI
# 148 (Solid R) + 71 (Likely R) + 0 (Lean R) = 219 EV Rep

# Sabato: 
# https://centerforpolitics.org/crystalball/2024-president/
d_sabato |> 
  filter(year == 2024 & rating == 4) |> 
  select(state)

# 226 D EV
# 93 ? EV
# 219 R EV

# Conclusion: Both have same set of states as each. 
# Importance of these swing states!

# Combine expert predictions generally. 
d_expert <- d_cook |> 
  select(year = Cycle, state = State, cook_rating = rating_numeric) |> 
  left_join(d_sabato |> select(year, state, sabato_rating = rating), by = c("year", "state")) |> 
  mutate(rating_match = as.numeric(cook_rating == sabato_rating)) |> 
  drop_na()

d_expert |> 
  group_by(year) |> 
  summarize(mean_match_rate = mean(rating_match)) 

# Merge in voting data. 
d_state_vote_wide <- d_state_vote |> 
  select(state, year, R_pv2p, D_pv2p, R_pv2p_lag1, R_pv2p_lag2, D_pv2p_lag1, D_pv2p_lag2) |>
  mutate(margin = D_pv2p - R_pv2p, 
         winner = ifelse(D_pv2p > R_pv2p, "D", "R"))
d_state_vote_wide[d_state_vote_wide$state == "District Of Columbia",]$state <- "District of Columbia"
d_expert <- d_expert |> 
  left_join(d_state_vote_wide, 
            by = c("state", "year"))
d_expert <- d_expert |> 
  mutate(cook_correct = as.numeric((winner == "D" & cook_rating < 4) | 
                                     (winner == "R" & cook_rating > 4)),
         sabato_correct = as.numeric((winner == "D" & sabato_rating < 4) | 
                                       (winner == "R" & sabato_rating > 4)))

d_expert |>
  select(cook_correct, sabato_correct) |> 
  colMeans()

d_expert |> 
  group_by(year) |> 
  summarize(mean_cook_correct = mean(cook_correct),
            mean_sabato_correct = mean(sabato_correct))

# How can we incorporate expert predictions into our models? 
# TODO: 

####----------------------------------------------------------#
#### Ensembling at the national level (incumbents only).
####----------------------------------------------------------#

# Split poll data into training and testing data based on inclusion or exclusion of 2024. 
d_poll_weeks_train_inc <- d_poll_weeks |> 
  filter(incumbent & year <= 2020)
d_poll_weeks_test_inc <- d_poll_weeks |> 
  filter(incumbent & year == 2024)

colnames(d_poll_weeks)[3:33] <- paste0("poll_weeks_left_", 0:30)
colnames(d_poll_weeks_train_inc)[3:33] <- paste0("poll_weeks_left_", 0:30)
colnames(d_poll_weeks_test_inc)[3:33] <- paste0("poll_weeks_left_", 0:30)

# First check how many weeks of polling we have for 2024. 
d_pollav_natl |> 
  filter(year == 2024) |> 
  select(weeks_left) |> 
  distinct() |> 
  range() # Let's take week 30 - 7 as predictors since those are the weeks we have polling data for 2024 and historically. 

x.train <- d_poll_weeks_train_inc |>
  ungroup() |> 
  select(all_of(paste0("poll_weeks_left_", 7:30))) |> 
  as.matrix()
y.train <- d_poll_weeks_train_inc$pv2p
x.test <- d_poll_weeks_test_inc |>
  ungroup() |> 
  select(all_of(paste0("poll_weeks_left_", 7:30))) |> 
  as.matrix()

# Using elastic-net for simplicity. 
set.seed(02138)
enet.poll <- cv.glmnet(x = x.train, y = y.train, alpha = 0.5)
lambda.min.enet.poll <- enet.poll$lambda.min

# Predict 2024 national pv2p share using elastic-net. 
(polls.pred <- predict(enet.poll, s = lambda.min.enet.poll, newx = x.test))

# Estimate models using polls alone, fundamentals alone, and combined fundamentals and polls. 
# Read economic data. 
d_econ <- read_csv("fred_econ.csv") |> 
  filter(quarter == 2)

# Combine datasets and create vote lags. 
d_combined <- d_econ |> 
  left_join(d_poll_weeks, by = "year") |> 
  filter(year %in% c(unique(d_vote$year), 2024)) |> 
  group_by(party) |> 
  mutate(pv2p_lag1 = lag(pv2p, 1), 
         pv2p_lag2 = lag(pv2p, 2)) |> 
  ungroup() |> 
  mutate(gdp_growth_x_incumbent = GDP_growth_quarterly * incumbent, 
         rdpi_growth_quarterly = RDPI_growth_quarterly * incumbent,
         cpi_x_incumbent = CPI * incumbent,
         unemployment_x_incumbent = unemployment * incumbent,
         sp500_x_incumbent = sp500_close * incumbent) # Generate interaction effects.

# Create fundamentals-only dataset and split into training and test sets. 
d_fund_inc <- d_combined |> 
  filter(incumbent) |> 
  select("year", "pv2p", "GDP", "GDP_growth_quarterly", "RDPI", "RDPI_growth_quarterly", "CPI", "unemployment", "sp500_close",
         "rdpi_growth_quarterly", "pv2p_lag1", "pv2p_lag2") 
x.train.fund <- d_fund_inc |> 
  filter(year <= 2020) |>
  select(-c(year, pv2p)) |> 
  slice(-1) |> 
  as.matrix()
y.train.fund <- d_fund_inc |> 
  filter(year <= 2020) |> 
  select(pv2p) |> 
  slice(-1) |> 
  as.matrix()
x.test.fund <- d_fund_inc |> 
  filter(year == 2024) |> 
  select(-c(year, pv2p)) |> 
  drop_na() |> 
  as.matrix()

# Estimate elastic-net using fundamental variables only.
set.seed(02138)
enet.fund <- cv.glmnet(x = x.train.fund, y = y.train.fund, intercept = FALSE, alpha = 0.5)
lambda.min.enet.fund <- enet.fund$lambda.min

# Predict 2024 national pv2p share using elastic-net. 
(fund.pred <- predict(enet.fund, s = lambda.min.enet.fund, newx = x.test.fund))
  
# Sequester data for combined model.
d_combo_inc <- d_combined |> 
  filter(incumbent) |> 
  select("year", "pv2p", "GDP", "GDP_growth_quarterly", "RDPI", "RDPI_growth_quarterly", "CPI", "unemployment", "sp500_close",
         "rdpi_growth_quarterly", "pv2p_lag1", "pv2p_lag2", all_of(paste0("poll_weeks_left_", 7:30))) 

x.train.combined <- d_combo_inc |> 
  filter(year <= 2020) |> 
  select(-c(year, pv2p)) |> 
  slice(-1) |> 
  as.matrix()
y.train.combined <- d_combo_inc |>
  filter(year <= 2020) |> 
  select(pv2p) |> 
  slice(-1) |> 
  as.matrix()
x.test.combined <- d_combo_inc |>
  filter(year == 2024) |> 
  select(-c(year, pv2p)) |> 
  drop_na() |> 
  as.matrix()
  
# Estimate combined model.
set.seed(02138)
enet.combined <- cv.glmnet(x = x.train.combined, y = y.train.combined, intercept = FALSE, alpha = 0.5)
lambda.min.enet.combined <- enet.combined$lambda.min

# Predict 2024 national pv2p share using elastic-net.
(combo.pred <- predict(enet.combined, s = lambda.min.enet.combined, newx = x.test.combined))

# Ensemble 1: Predict based on unweighted (or equally weighted) ensemble model between polls and fundamentals models. 
(unweighted.ensemble.pred <- (polls.pred + fund.pred)/2)

# Ensemble 2: Weight based on polls mattering closer to November. (Nate Silver)
election_day_2024 <- "2024-11-05"
today <- "2024-09-18"
days_left <- as.numeric(as.Date(election_day_2024) - as.Date(today))

(poll_model_weight <- 1- (1/sqrt(days_left)))
(fund_model_weight <- 1/sqrt(days_left))

(ensemble.2.pred <- polls.pred * poll_model_weight + fund.pred * fund_model_weight)  

# Ensemble 3. Weight based on fundamentals mattering closer to November. (Gelman & King, 1993)
(poll_model_weight <- 1/sqrt(days_left))
(fund_model_weight <- 1-(1/sqrt(days_left)))

(ensemble.3.pred <- polls.pred * poll_model_weight + fund.pred * fund_model_weight)

####----------------------------------------------------------#
#### Super learning at the state level for swing states.
####----------------------------------------------------------#

# Get set of states where we have polling data for 2024 according to 538 poll averages.
states_2024 <- d_pollav_state$state[d_pollav_state$year == 2024] |> unique()

# Predicting for Democratic incumbents.
# Simplifications and assumptions: 
  # Assuming Harris can be treated as incumbent for 2024 (could test either)
  # Getting weights from testing models on 2020 (could do different years)
  # Pooled models (could run state-specific models)
  # Using LOO-CV on 2020 (could do K-fold CV)
  # Using average poll support across all 30 weeks until election (could do weekly support, various imputation methods)
d_state_combo <- d_pollav_state |> 
  filter((state %in% states_2024)) |> 
  group_by(year, state, party) |>
  mutate(mean_pollav = mean(poll_support)) |>
  top_n(1, poll_date) |> 
  rename(latest_pollav = poll_support) |> 
  ungroup() |> 
  left_join(d_vote |> select(-pv, -pv2p), by = c("year", "party")) |> 
  filter(party == "DEM") |> 
  left_join(d_state_vote_wide, by = c("year", "state")) 

# Model 1. Polling averages only. 
# Estimate model.
mod_1 <- lm(D_pv2p ~ latest_pollav + mean_pollav, 
            data = subset(d_state_combo, year < 2020))

# Model 2. Lagged vote model. 
mod_2 <- lm(D_pv2p ~ D_pv2p_lag1 + D_pv2p_lag2, 
            data = subset(d_state_combo, year < 2020))

# Model 3. Combined models. 
mod_3 <- lm(D_pv2p ~ incumbent + latest_pollav + mean_pollav + D_pv2p_lag1 + D_pv2p_lag2, 
            data = subset(d_state_combo, year < 2020))

# Predictions from each model. 
pred_1 <- as.numeric(predict(mod_1, newdata = subset(d_state_combo, year == 2020)))
pred_2 <- as.numeric(predict(mod_2, newdata = subset(d_state_combo, year == 2020)))
pred_3 <- as.numeric(predict(mod_3, newdata = subset(d_state_combo, year == 2020)))

# Get weights to build super learner. 
d_weight <- data.frame("truth" = d_state_combo$D_pv2p[d_state_combo$year == 2020],
                       "polls" = pred_1,
                       "lag_vote" = pred_2,
                       "combo" = pred_3)

# Constrained optimization for ensemble mod weights. 
mod_ensemble <- lm(truth ~ polls + lag_vote + combo, 
                   data = d_weight)

# Get weights and estimated weighted ensemble via constrained regression.
c <- 3 # number of models
predictions <- cbind(pred_1, pred_2, pred_3)
y.test <- d_weight$truth
w <- lm(y.test ~ predictions-1)
beta <- Variable(c)
objective <- Minimize(sum_squares(y.test - predictions %*% beta))
prob <- Problem(objective)
constraints(prob) <- list(beta >= 0, beta <= 1)
solution_prob <- solve(prob)
weights <- solution_prob$getValue(beta)

# Predict using previous model output.
ensemble_pred <- cbind("state" = subset(d_state_combo, year == 2020)$state,
                       "pred" = round(as.numeric(t(weights) %*% t(predictions)), 3)) |> 
  as.data.frame()

ensemble_pred <- ensemble_pred |> 
  mutate(winner = ifelse(pred > 50, "D", "R"))

ensemble_pred

# Merge with electoral college dataset and make plots. 
# TODO: 

                             
                             
                             
                             
                             
                             

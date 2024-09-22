#' @title GOV 1347: Week 3 (Polling) Laboratory Session
#' @author Matthew E. Dardet
#' @date September 18, 2024

####----------------------------------------------------------#
#### Preamble
####-----------------7-----------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(car)
library(caret)
library(CVXR)
library(glmnet)
library(tidyverse)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read, merge, and process data.
####----------------------------------------------------------#

# Read data (processed FiveThirtyEight polling average datasets).
d_pollav_natl <- read_csv("national_polls_1968-2024.csv")
d_pollav_state <- read_csv("state_polls_1968-2024.csv")

####----------------------------------------------------------#
#### Visualizing poll variation over time.
####----------------------------------------------------------#

# Plot 1. Polling Averages Over 2020 Election Cycle with and Without Event Labels. 
d_pollav_natl |> 
  filter(year == 2020) |> 
  ggplot(aes(x = poll_date, y = poll_support, color = party)) +
  geom_point(size = 1) + 
  geom_line() + 
  scale_x_date(date_labels = "%b %d") + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) +
  labs(x = "Date",
       y = "Average Poll Approval", 
       title = "Polling Averages by Date, 2020") + 
  theme_classic()

# Plot 2. RNC and DNC Bumps in 2020. 
d_pollav_natl |> 
  filter(year == 2020) |> 
  ggplot(aes(x = poll_date, y = poll_support, color = party)) +
  geom_rect(xmin = as.Date("2020-08-17"), xmax = as.Date("2020-08-20"), ymin = 47.5, ymax = 100, alpha = 0.1, color = NA, fill = "grey") + 
  annotate("text", x = as.Date("2020-08-07"), y = 51.5, label = "DNC", size = 4) + 
  geom_rect(xmin = as.Date("2020-08-24"), xmax = as.Date("2020-08-27"), ymin = 0, ymax = 46, alpha = 0.1, color = NA, fill = "grey") +
  annotate("text", x = as.Date("2020-09-04"), y = 45, label = "RNC", size = 4) +
  geom_point(size = 1) + 
  geom_line() + 
  scale_x_date(date_labels = "%b %d") + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) +
  labs(x = "Date",
       y = "Average Poll Approval", 
       title = "Polling Averages by Date, 2020 (with Conference Dates)") + 
  theme_classic()

# Plot 3. Adding in Extra Dates of Interest for 2020 —— "game changers"? 
d_pollav_natl |> 
  filter(year == 2020) |> 
  ggplot(aes(x = poll_date, y = poll_support, color = party)) +
  geom_rect(xmin = as.Date("2020-08-17"), xmax = as.Date("2020-08-20"), ymin = 47.5, ymax = 100, alpha = 0.1, color = NA, fill = "grey") + 
  annotate("text", x = as.Date("2020-08-07"), y = 51.5, label = "DNC", size = 4) +
  geom_rect(xmin = as.Date("2020-08-24"), xmax = as.Date("2020-08-27"), ymin = 0, ymax = 47.2, alpha = 0.1, color = NA, fill = "grey") +
  annotate("text", x = as.Date("2020-09-04"), y = 45, label = "RNC", size = 4) +
  geom_rect(xmin = as.Date("2020-10-02"), xmax = as.Date("2020-10-12"), ymin = 0, ymax = 42.7, alpha = 0.05, color = NA, fill = "grey") +
  
  geom_point(size = 1) + 
  geom_line() + 
  
  geom_segment(x = as.Date("2020-03-12"), xend = as.Date("2020-03-12"), y = 0, yend = 44.8, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-03-12"), y = 42.5, label = "COVID \n Market Crash", size = 3) +
  geom_segment(x = as.Date("2020-04-08"), xend = as.Date("2020-04-08"), y = 49, yend = 100, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-03-25"), y = 51.3, label = "Bernie Ends Run", size = 3) +
  geom_segment(x = as.Date("2020-04-16"), xend = as.Date("2020-04-16"), y = 0, yend = 44, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-04-16"), y = 44.7, label = "22 mil \n Unemployment", size = 3) +
  geom_segment(x = as.Date("2020-05-27"), xend = as.Date("2020-05-27"), y = 0, yend = 43, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-06-05"), y = 44, label = "100k COVID Dead, \n George Floyd", size = 3) +
  
  geom_segment(x = as.Date("2020-07-14"), xend = as.Date("2020-07-14"), y = 0, yend = 50.3, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-06-19"), y = 47.5, label = "Moderna Announces", size = 3) +
  
  geom_segment(x = as.Date("2020-09-29"), xend = as.Date("2020-09-29"), y = 50, yend = 100, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-9-12"), y = 49.5, label = "Pres. Debate", size = 3) +
  geom_segment(x = as.Date("2020-10-07"), xend = as.Date("2020-10-07"), y = 51.7, yend = 100, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-10-17"), y = 50.3, label = "VP Debate", size = 3) +
  geom_segment(x = as.Date("2020-10-22"), xend = as.Date("2020-10-22"), y = 52, yend = 100, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-10-30"), y = 51.5, label = "Pres. Debate", size = 3) +
  annotate("text", x = as.Date("2020-10-15"), y = 43.7, label = "Trump Has COVID", size = 3) +
  geom_segment(x = as.Date("2020-09-18"), xend = as.Date("2020-09-18"), y = 50, yend = 100, linetype = "dashed", alpha = 0.4, color = "grey") +
  annotate("text", x = as.Date("2020-09-03"), y = 51.5, label = "RBG Passes", size = 3) +
  
  scale_x_date(date_labels = "%b %d") + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) +
  labs(x = "Date",
       y = "Average Poll Approval", 
       title = "Polling Averages by Date, 2020 (with Game Changers?)") + 
  theme_classic()

# Plot 4. Poll Averages and "Game Changers" for 1988
d_pollav_natl |>
  filter(year == 1988) |>
  ggplot(aes(x = poll_date, y = poll_support, color = party)) +
  geom_rect(xmin=as.Date("1988-07-18"), xmax=as.Date("1988-07-21"), ymin=47, ymax=100, alpha=0.1, colour=NA, fill="grey") +
  annotate("text", x=as.Date("1988-07-10"), y=50, label="DNC", size=4) +
  geom_rect(xmin=as.Date("1988-08-15"), xmax=as.Date("1988-08-18"), ymin=0, ymax=44, alpha=0.1, colour=NA, fill="grey") +
  annotate("text", x=as.Date("1988-08-26"), y=40, label="RNC", size=4) +
  
  geom_point(size = 1) +
  geom_line() + 
  
  geom_segment(x=as.Date("1988-09-13"), xend=as.Date("1988-09-13"), y=49, yend=100, lty=2, color="grey", alpha=0.4) +
  annotate("text", x=as.Date("1988-09-13"), y=52, label="Tank Gaffe\n(?)", size=3) +
  annotate("text", x=as.Date("1988-09-21"), y=57, label="Willie Horton Ad\n(?)", size=3) +
  geom_segment(x=as.Date("1988-09-21"), xend=as.Date("1988-09-21"), y=49, yend=100, lty=2, color="grey", alpha=0.4) +
  annotate("text", x=as.Date("1988-10-15"), y=64, label="First Debate\n(Death\nPenalty\nGaffe)", size=3) +
  geom_segment(x=as.Date("1988-10-15"), xend=as.Date("1988-10-15"), y=49, yend=100, lty=2, color="grey", alpha=0.4) +
  scale_x_date(date_labels = "%b, %Y") +
  scale_color_manual(values = c("dodgerblue4","firebrick1")) +
  labs(x = "Date",
       y = "Average Poll Approval", 
       title = "Polling Averages by Date, 1988 (with Game Changers?)") + 
  theme_classic()

# Plot 5. Poll Averages for 2024
d_pollav_natl |> 
  filter(year == 2024) |> 
  ggplot(aes(x = poll_date, y = poll_support, color = party)) +
  geom_point(size = 1) + 
  geom_line() + 
  scale_x_date(date_labels = "%b %d") + 
  scale_color_manual(values = c("dodgerblue4", "firebrick1")) +
  labs(x = "Date",
       y = "Average Poll Approval", 
       title = "Polling Averages by Date, 2024") + 
  theme_classic()

####----------------------------------------------------------#
#### Regularized regression with polling data.
####----------------------------------------------------------#

# Read election results data. 
d_vote <- read_csv("popvote_1948-2020.csv")
d_vote$party[d_vote$party == "democrat"] <- "DEM"
d_vote$party[d_vote$party == "republican"] <- "REP"

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

# OLS: Democratic candidates pv2p on November polling average. 
ols.nov.1 <- lm(pv2p ~ nov_poll, 
                data = subset(d_poll_nov, party == "DEM"))
summary(ols.nov.1)

# OLS: Party-stacked pv2p on November polling average.
ols.nov.2 <- lm(pv2p ~ nov_poll, 
                data = d_poll_nov)
summary(ols.nov.2)

# Create dataset of polling average by week until the election. 
d_poll_weeks <- d_pollav_natl |> 
  group_by(year, party, weeks_left) |>
  summarize(mean_poll_week = mean(poll_support)) |> 
  filter(weeks_left <= 30) |> 
  pivot_wider(names_from = weeks_left, values_from = mean_poll_week) |> 
  left_join(d_vote, by = c("year", "party"))
 
# Split into training and testing data based on inclusion or exclusion of 2024. 
d_poll_weeks_train <- d_poll_weeks |> 
  filter(year <= 2020)
d_poll_weeks_test <- d_poll_weeks |> 
  filter(year == 2024)

colnames(d_poll_weeks)[3:33] <- paste0("poll_weeks_left_", 0:30)
colnames(d_poll_weeks_train)[3:33] <- paste0("poll_weeks_left_", 0:30)
colnames(d_poll_weeks_test)[3:33] <- paste0("poll_weeks_left_", 0:30)

# Comparison of OLS and regularized regression methods. 
ols.pollweeks <- lm(paste0("pv2p ~ ", paste0( "poll_weeks_left_", 0:30, collapse = " + ")), 
                    data = d_poll_weeks_train)
summary(ols.pollweeks) # N.B. Inestimable: p (31) > n (30)! 

# Separate data into X and Y for training. 
x.train <- d_poll_weeks_train |>
  ungroup() |> 
  select(all_of(starts_with("poll_weeks_left_"))) |> 
  as.matrix()
y.train <- d_poll_weeks_train$pv2p

# Ridge. 
ridge.pollsweeks <- glmnet(x = x.train, y = y.train, alpha = 0) # Set ridge using alpha = 0. 

# Visualize shrinkage. 
plot(ridge.pollsweeks, xvar = "lambda")

# Get particular coefficients. 
coef(ridge.pollsweeks, s = 0.1)

# Lasso.
lasso.pollsweeks <- glmnet(x = x.train, y = y.train, alpha = 1) # Set lasso using alpha = 1.

# Visualize shrinkage.
plot(lasso.pollsweeks, xvar = "lambda")

# Get particular coefficients.
coef(lasso.pollsweeks, s = 0.1)

# Elastic net.
enet.pollsweeks <- glmnet(x = x.train, y = y.train, alpha = 0.5) # Set elastic net using alpha = 0.5.

# Visualize shrinkage.
plot(enet.pollsweeks, xvar = "lambda")

# Can use cross-validated versions to find the optimal values of lambda that minimize the MSE of your predictions. 
# N.B. Use set.seed() and your favorite number e.g., 12345, 02138, before each CV/any stochastic call if you want your results to be stable. 
cv.ridge.pollweeks <- cv.glmnet(x = x.train, y = y.train, alpha = 0)
cv.lasso.pollweeks <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
cv.enet.pollweeks <- cv.glmnet(x = x.train, y = y.train, alpha = 0.5)

# Get minimum lambda values 
lambda.min.ridge <- cv.ridge.pollweeks$lambda.min
lambda.min.lasso <- cv.lasso.pollweeks$lambda.min
lambda.min.enet <- cv.enet.pollweeks$lambda.min

# Predict on training data using lambda values that minimize MSE.
(mse.ridge <- mean((predict(ridge.pollsweeks, s = lambda.min.ridge, newx = x.train) - y.train)^2))
(mse.lasso <- mean((predict(lasso.pollsweeks, s = lambda.min.lasso, newx = x.train) - y.train)^2))
(mse.enet <- mean((predict(enet.pollsweeks, s = lambda.min.enet, newx = x.train) - y.train)^2))

# Generate plot comparing coefficients for each of the weeks. 
d.coefplot <- data.frame("OLS" = coef(ols.pollweeks)[-1], 
                         "Ridge" = coef(ridge.pollsweeks, s = lambda.min.ridge)[-1], 
                         "Lasso" = coef(lasso.pollsweeks, s = lambda.min.lasso)[-1], 
                         "Elastic Net" = coef(enet.pollsweeks, s = lambda.min.enet)[-1]) |> 
  rownames_to_column("coef_name") |> 
  pivot_longer(cols = -coef_name, names_to = "method", values_to = "coef_est") |> 
  mutate(week = rep(0:30, each = 4))

d.coefplot[which(is.na(d.coefplot$coef_est)),]$coef_est <- 0 

d.coefplot |>
  ggplot(aes(x = coef_est, y = reorder(coef_name, -week), color = method)) +
  geom_segment(aes(xend = 0, yend = reorder(coef_name, -week)), alpha = 0.5, lty = "dashed") +
  geom_vline(aes(xintercept = 0), lty = "dashed") +   
  geom_point() + 
  labs(x = "Coefficient Estimate", 
       y = "Coefficient Name", 
       title = "Comparison of Coefficients Across Regularization Methods") + 
  theme_classic()

# First check how many weeks of polling we have for 2024. 
d_pollav_natl |> 
  filter(year == 2024) |> 
  select(weeks_left) |> 
  distinct() |> 
  range() # Let's take week 30 - 7 as predictors since those are the weeks we have polling data for 2024 and historically. 

x.train <- d_poll_weeks_train |>
  ungroup() |> 
  select(all_of(paste0("poll_weeks_left_", 7:30))) |> 
  as.matrix()
y.train <- d_poll_weeks_train$pv2p
x.test <- d_poll_weeks_test |>
  ungroup() |> 
  select(all_of(paste0("poll_weeks_left_", 7:30))) |> 
  as.matrix()

# Using elastic-net for simplicity. 
set.seed(02138)
enet.poll <- cv.glmnet(x = x.train, y = y.train, alpha = 0.5)
lambda.min.enet.poll <- enet.poll$lambda.min

# Predict 2024 national pv2p share using elastic-net. 
(polls.pred <- predict(enet.poll, s = lambda.min.enet.poll, newx = x.test))

# Harris: 51.8%
# Trump: 50.7% 

####----------------------------------------------------------#
#### Model ensembling.
####----------------------------------------------------------#

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
d_fund <- d_combined |> 
  select("year", "pv2p", "GDP", "GDP_growth_quarterly", "RDPI", "RDPI_growth_quarterly", "CPI", "unemployment", "sp500_close",
         "incumbent", "gdp_growth_x_incumbent", "rdpi_growth_quarterly", "cpi_x_incumbent", "unemployment_x_incumbent", "sp500_x_incumbent", 
         "pv2p_lag1", "pv2p_lag2") 
x.train.fund <- d_fund |> 
  filter(year <= 2020) |>
  select(-c(year, pv2p)) |> 
  slice(-c(1:9)) |> 
  as.matrix()
y.train.fund <- d_fund |> 
  filter(year <= 2020) |> 
  select(pv2p) |> 
  slice(-c(1:9)) |> 
  as.matrix()
x.test.fund <- d_fund |> 
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
d_combo <- d_combined |> 
  select("year", "pv2p", "GDP", "GDP_growth_quarterly", "RDPI", "RDPI_growth_quarterly", "CPI", "unemployment", "sp500_close",
         "incumbent", "gdp_growth_x_incumbent", "rdpi_growth_quarterly", "cpi_x_incumbent", "unemployment_x_incumbent", "sp500_x_incumbent", 
         "pv2p_lag1", "pv2p_lag2", all_of(paste0("poll_weeks_left_", 7:30))) 

x.train.combined <- d_combo |> 
  filter(year <= 2020) |> 
  select(-c(year, pv2p)) |> 
  slice(-c(1:9)) |> 
  as.matrix()
y.train.combined <- d_combo |>
  filter(year <= 2020) |> 
  select(pv2p) |> 
  slice(-c(1:9)) |> 
  as.matrix()
x.test.combined <- d_combo |>
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










#' @title GOV 1347: Week 6 (Ads) Laboratory Session
#' @author Matthew E. Dardet
#' @date October 9, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(car)
library(caret)
library(cowplot)
library(curl)
library(CVXR)
library(foreign)
library(geofacet)
library(glmnet)
library(haven)
library(janitor)
library(kableExtra)
library(maps)
library(mlr3)
library(randomForest)
library(ranger)
library(RColorBrewer)
library(rstan)
library(scales)
library(sf)
library(shinystan)
library(tidyverse)
library(viridis)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read, merge, and process data.
####----------------------------------------------------------#

# Read popular vote datasets. 
d_popvote <- read_csv("popvote_1948_2020.csv")
d_state_popvote <- read_csv("state_popvote_1948_2020.csv")

# Read elector distribution dataset. 
d_ec <- read_csv("corrected_ec_1948_2024.csv")

# Read ads datasets. 
ad_campaigns <- read_csv("ad_campaigns_2000-2012.csv")
ad_creative <- read_csv("ad_creative_2000-2012.csv")
ads_2020 <- read_csv("ads_2020.csv")
facebook_ads_2020 <- read_csv("facebook_ads_2020.csv")
facebook_ads_biden_2020 <- read_csv("facebook_ads_biden_2020.csv")
campaign_spending <- read_csv("FEC_contributions_by_state_2008_2024.csv")

# Read polling data. 
d_polls <- read_csv("national_polls_1968-2024.csv")
d_state_polls <- read_csv("state_polls_1968-2024.csv")

# Read turnout data. 
d_turnout <- read_csv("state_turnout_1980_2022.csv")

####--------------------------------------------------------------#
#### Descriptive statistics on ads and campaign spending over time. 
####--------------------------------------------------------------#

# Tone and Political Ads. 
ad_campaigns |>
  left_join(ad_creative) |>
  group_by(cycle, party) |> mutate(tot_n=n()) |> ungroup() |>
  group_by(cycle, party, ad_tone) |> summarise(pct=n()*100/first(tot_n)) |>
  filter(!is.na(ad_tone)) |>
  ggplot(aes(x = cycle, y = pct, fill = ad_tone, group = party)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(2000, 2012, 4)) +
  ggtitle("Campaign Ads Aired By Tone") +
  scale_fill_manual(values = c("red","orange","gray","darkgreen","white"), name = "tone") +
  xlab("") + ylab("%") +
  facet_wrap(~ party) + theme_minimal() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        strip.text.x = element_text(size = 20))

## The Purpose of Political Ads
ad_campaigns |>
  left_join(ad_creative) |>
  group_by(cycle, party) |> mutate(tot_n=n()) |> ungroup() |>
  group_by(cycle, party, ad_purpose) |> summarise(pct=n()*100/first(tot_n)) |>
  filter(!is.na(ad_purpose)) |>
  bind_rows( ##2016 raw data not public yet! This was entered manually
    data.frame(cycle = 2016, ad_purpose = "personal", party = "democrat", pct = 67),
    data.frame(cycle = 2016, ad_purpose = "policy", party = "democrat", pct = 12),
    data.frame(cycle = 2016, ad_purpose = "both", party = "democrat", pct = 21),
    data.frame(cycle = 2016, ad_purpose = "personal", party = "republican", pct = 11),
    data.frame(cycle = 2016, ad_purpose = "policy", party = "republican", pct = 71),
    data.frame(cycle = 2016, ad_purpose = "both", party = "republican", pct = 18)
  ) |>
  ggplot(aes(x = cycle, y = pct, fill = ad_purpose, group = party)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(2000, 2016, 4)) +
  # ggtitle("Campaign Ads Aired By Purpose") +
  scale_fill_manual(values = c("grey","red","darkgreen","black","white"), name = "purpose") +
  xlab("") + ylab("%") +
  facet_wrap(~ party) + theme_minimal() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=15),
        strip.text.x = element_text(size = 20))

## The Elections and Their Issues
top_issues <- ad_campaigns |> 
  left_join(ad_creative) |>
  filter(!grepl("None|Other", ad_issue)) |>
  group_by(cycle, ad_issue) |> summarise(n=n()) |> top_n(5, n)

### making each plot in a grid to have its own x-axis (issue name)
### is tricky with `facet_wrap`, so we use this package `cowplot`
### which allows us to take a list of separate plots and grid them together
plist <- lapply(c(2000,2004,2008,2012), function(c) {
  top_issues |> filter(cycle == c) |> 
    ggplot(aes(x = reorder(ad_issue, n), y = n)) +
    geom_bar(stat = "identity") + coord_flip() + theme_bw() +
    xlab("") + ylab("number ads aired") + ggtitle(paste("Top 5 Ad\nIssues in",c))
  
})
cowplot::plot_grid(plotlist = plist, nrow = 2, ncol = 2, align = "hv")

## Campaign Ads Aired By Issue and Party: 2000
party_issues2000 <- ad_campaigns |>
  filter(cycle == 2000) |>
  left_join(ad_creative) |>
  filter(ad_issue != "None") |>
  ## this `group_by` is to get our denominator
  group_by(ad_issue) |> mutate(tot_n=n()) |> ungroup() |>
  ## this one is get numerator and calculate % by party
  group_by(ad_issue, party) |> summarise(p_n=n()*100/first(tot_n)) |> ungroup() |>
  ## finally, this one so we can sort the issue names
  ## by D% of issue ad-share instead of alphabetically
  group_by(ad_issue) |> mutate(Dp_n = ifelse(first(party) == "democrat", first(p_n), 0))

ggplot(party_issues2000, aes(x = reorder(ad_issue, Dp_n), y = p_n, fill = party)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ylab("% of ads on topic from each party") + xlab("issue") + 
  # ggtitle("Campaign Ads Aired by Topic in 2000") +
  coord_flip() + 
  theme_bw()

## Campaign Ads Aired By Issue and Party: 2012
party_issues2012 <- ad_campaigns |>
  filter(cycle == 2012) |>
  left_join(ad_creative) |>
  filter(ad_issue != "None") |>
  group_by(cycle, ad_issue) |> mutate(tot_n=n()) |> ungroup() |>
  group_by(cycle, ad_issue, party) |> summarise(p_n=n()*100/first(tot_n)) |> ungroup() |>
  group_by(cycle, ad_issue) |> mutate(Dp_n = ifelse(first(party) == "democrat", first(p_n), 0))

ggplot(party_issues2012, aes(x = reorder(ad_issue, Dp_n), y = p_n, fill = party)) + 
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red")) +
  ylab("% of ads on topic from each party") + xlab("issue") +
  # ggtitle("Campaign Ads Aired by Topic in 2012") +
  coord_flip() + 
  theme_bw()

## When to Buy Ads? 
ad_campaigns |>
  mutate(year = as.numeric(substr(air_date, 1, 4))) |>
  mutate(month = as.numeric(substr(air_date, 6, 7))) |>
  filter(year %in% c(2000, 2004, 2008, 2012), month > 7) |>
  group_by(cycle, air_date, party) |>
  summarise(total_cost = sum(total_cost)) |>
  ggplot(aes(x=air_date, y=total_cost, color=party)) +
  # scale_x_date(date_labels = "%b, %Y") +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_manual(values = c("blue","red"), name = "") +
  geom_line() + geom_point(size=0.5) +
  facet_wrap(cycle ~ ., scales="free") +
  xlab("") + ylab("ad spend") +
  theme_bw() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=11),
        strip.text.x = element_text(size = 20))

## Tone in Political Ads
ad_campaigns |>
  left_join(ad_creative) |>
  filter(ad_tone %in% c("attack", "promote")) |>
  mutate(year = as.numeric(substr(air_date, 1, 4))) |>
  mutate(month = as.numeric(substr(air_date, 6, 7))) |>
  filter(year %in% c(2000, 2004, 2008, 2012), month > 7) |>
  group_by(cycle, air_date, ad_tone) |>
  summarise(total_cost = sum(n_stations)) |>
  group_by(cycle, air_date) |>
  mutate(total_cost = total_cost/sum(total_cost)) |>
  ungroup() |>
  ggplot(aes(x=air_date, y=total_cost, fill=ad_tone, color=ad_tone)) +
  # scale_x_date(date_labels = "%b") +
  scale_fill_manual(values = c("purple","green"), name = "ad tone") +
  scale_color_manual(values = c("purple","green"), name = "ad tone") +
  geom_bar(stat = "identity") +
  facet_wrap(cycle ~ ., scales="free") +
  xlab("") + ylab("% of ads bought on day") +
  theme_bw() +
  theme(axis.title = element_text(size=20),
        axis.text = element_text(size=10),
        strip.text.x = element_text(size = 20))

## The State-level Air War in 2008 (Obama vs. McCain)
ad_campaigns |>
  mutate(year = as.numeric(substr(air_date, 1, 4))) |>
  mutate(month = as.numeric(substr(air_date, 6, 7))) |>
  mutate(state = state.name[match(state, state.abb)]) |>
  filter(cycle == 2008) |>
  left_join(d_state_popvote |> filter(year == 2008) |> select(-year), by="state") |>
  mutate(winner=ifelse(D_pv2p > R_pv2p, "democrat", "republican")) |>
  group_by(cycle, state, air_date, party, winner) |>
  summarise(total_cost = sum(total_cost)) |>
  filter(!is.na(state)) |>
  # ggplot(aes(x=air_date, y=log(total_cost+1), color=party)) +
  ggplot(aes(x=party, y=total_cost, fill=party)) +
  geom_bar(stat="identity") +
  geom_rect(aes(fill=winner), xmin=-Inf, xmax=Inf, ymin=46.3*10^6, ymax=52*10^6) +
  facet_geo(~ state, scales="free_x") +
  scale_fill_manual(values = c("blue", "red")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  xlab("") + ylab("ad spend") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# Visualizing Facebook ads and Biden Facebook ads in 2020. 
d_facebook <- facebook_ads_2020 |> 
  rename(date = from_date, new_ads = num_of_new_ads) |> 
  group_by(date) |> 
  summarize(new_spend = sum(new_spend, na.rm = T),
            new_ads = sum(new_ads, na.rm = T))

d_facebook |> 
  ggplot(aes(x = date, y = new_ads)) + 
  geom_line() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Date", 
       y = "New Facebook Ads") +
  theme_minimal()

d_facebook |> 
  ggplot(aes(x = date, y = new_spend)) +
  geom_line() +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Date", 
       y = "New Facebook Ad Spending") +
  theme_minimal()
  
d_facebook_biden <- facebook_ads_biden_2020 |> 
  rename(date = from_date, new_ads = num_of_new_ads) |> 
  group_by(date) |> 
  summarize(new_spend = sum(new_spend, na.rm = T),
            new_ads = sum(new_ads, na.rm = T))

d_facebook_biden |>
  ggplot(aes(x = date, y = new_ads)) + 
  geom_line() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Date", 
       y = "New Facebook Ads (Biden Only)") +
  theme_minimal()

d_facebook_biden |>
  ggplot(aes(x = date, y = new_spend)) +
  geom_line() +
  scale_y_continuous(labels = dollar_format()) +
  geom_smooth(method = "lm", se = TRUE) + 
  labs(x = "Date", 
       y = "New Facebook Ad Spending (Biden Only)")
  theme_minimal()

# Visualizing FEC contributions by state in 2020, 2024, over time. 
# dataset: campaign_spending
# TODO

# Estimate state-level regression of vote share on campaign spending. 
d_campaign_spending <- d_state_popvote |> 
  mutate(state_abb = state.abb[match(d_state_popvote$state, state.name)]) |> 
  left_join(campaign_spending |> filter(party == "Democrat"), by = c("year" = "election_year", "state_abb" = "contribution_state")) |> 
  filter(year >= 2008)

lm(D_pv ~ contribution_receipt_amount, 
   data = d_campaign_spending) |> summary()

lm(D_pv2p ~ contribution_receipt_amount, 
   data = d_campaign_spending) |> summary()

lm(D_pv ~ contribution_receipt_amount + factor(state), 
   data = d_campaign_spending) |> summary()

lm(D_pv2p ~ contribution_receipt_amount + factor(state), 
   data = d_campaign_spending) |> summary()


# Log transformation of spending. 
lm(D_pv ~ log(contribution_receipt_amount), 
   data = d_campaign_spending) |> summary()

lm(D_pv2p ~ log(contribution_receipt_amount), 
   data = d_campaign_spending) |> summary()

lm(D_pv ~ log(contribution_receipt_amount) + factor(state), 
   data = d_campaign_spending) |> summary()

lm(D_pv2p ~ log(contribution_receipt_amount) + factor(state), 
   data = d_campaign_spending) |> summary() # Why might this be? 















####--------------------------------------------------------------#
#### Bayesianism.
####--------------------------------------------------------------#

# Process state-level polling data.
d_pollav_state <- d_state_polls |>
  group_by(year, state, party) |>
  mutate(mean_pollav = mean(poll_support, na.rm = TRUE)) |>
  top_n(1, poll_date) |>
  rename(latest_pollav = poll_support) |>
  select(-c(weeks_left, days_left, poll_date, candidate, before_convention)) |>
  pivot_wider(names_from = party, values_from = c(latest_pollav, mean_pollav))

# Merge data.
d <- d_pollav_state |>
  left_join(d_state_popvote, by = c("year", "state")) |>
  left_join(d_popvote |> filter(party == "democrat"), by = "year") |>
  left_join(d_turnout, by = c("year", "state")) |>
  filter(year >= 1980) |>
  ungroup()

# Sequester states for which we have polling data for 2024.
states.2024 <- unique(d$state[d$year == 2024])
states.2024 <- states.2024[-which(states.2024 == "Nebraska Cd 2")]
d <- d |>
  filter(state %in% states.2024)

# Separate into training and testing for simple poll prediction model. 
d.train <- d |> filter(year < 2024) |> select(year, state, D_pv2p, latest_pollav_DEM, mean_pollav_DEM, 
                                              D_pv2p_lag1, D_pv2p_lag2) |> drop_na()
d.test <- d |> filter(year == 2024) |> select(year, state, D_pv2p, latest_pollav_DEM, mean_pollav_DEM, 
                                              D_pv2p_lag1, D_pv2p_lag2)

# Add back in lagged vote share for 2024. 
t <- d |> 
  filter(year >= 2016) |> 
  arrange(year) |> 
  group_by(state) |> 
  mutate(
    D_pv2p_lag1 = lag(D_pv2p, 1),
    R_pv2p_lag1 = lag(R_pv2p, 1), 
    D_pv2p_lag2 = lag(D_pv2p, 2),
    R_pv2p_lag2 = lag(R_pv2p, 2)) |> 
  filter(year == 2024) |> 
  select(state, year, D_pv2p, R_pv2p, D_pv2p_lag1, R_pv2p_lag1, D_pv2p_lag2, R_pv2p_lag2) 

# Subset testing data to only relevant variables for our simple model. 
d.test <- d.test |> 
  select(-c(D_pv2p, D_pv2p_lag1, D_pv2p_lag2)) |> 
  left_join(t, by = c("state", "year"))

# Standard frequentist linear regression. 
reg.ols <- lm(D_pv2p ~ latest_pollav_DEM + mean_pollav_DEM + D_pv2p_lag1 + D_pv2p_lag2, 
              data = d.train)
summary(reg.ols)
pred.ols.dem <- predict(reg.ols, newdata = d.test)

# Create dataset to summarize winners and EC vote distributions. 
win_pred <- data.frame(state = d.test$state,
                       year = rep(2024, length(d.test$state)),
                       simp_pred_dem = pred.ols.dem,
                       simp_pred_rep = 100 - pred.ols.dem) |> 
            mutate(winner = ifelse(simp_pred_dem > simp_pred_rep, "Democrat", "Republican")) |>
            left_join(d_ec, by = c("state", "year"))

win_pred

win_pred |> 
  filter(winner == "Democrat") |> 
  select(state)

win_pred |> 
  filter(winner == "Republican") |> 
  select(state)

win_pred |> 
  group_by(winner) |> 
  summarize(n = n(), ec = sum(electors))

# Bayesian linear regression using STAN. 
stan.data <- list(N = nrow(d.train), 
                  D_pv2p = d.train$D_pv2p, 
                  latest_pollav_DEM = d.train$latest_pollav_DEM, 
                  mean_pollav_DEM = d.train$mean_pollav_DEM, 
                  D_pv2p_lag1 = d.train$D_pv2p_lag1, 
                  D_pv2p_lag2 = d.train$D_pv2p_lag2)

stan.code <- "
data {
  int<lower=0> N;
  vector[N] D_pv2p;
  vector[N] latest_pollav_DEM;
  vector[N] mean_pollav_DEM;
  vector[N] D_pv2p_lag1;
  vector[N] D_pv2p_lag2;
} "

stan.code <- paste(stan.code, "
parameters {
  real alpha;
  real beta1;
  real beta2;
  real beta3;
  real beta4;
  real<lower=0> sigma;
} ")

stan.code <- paste(stan.code, "
model {
  D_pv2p ~ normal(alpha + beta1*latest_pollav_DEM + beta2*mean_pollav_DEM + beta3*D_pv2p_lag1 + beta4*D_pv2p_lag2, sigma);
} ")

stan.model <- stan_model(model_code = stan.code)

stan.fit <- sampling(stan.model, data = stan.data, chains = 4, iter = 4000, warmup = 1000)

# Compare coefficients from frequentist and Bayesian linear regressions. 
coef(reg.ols)
confint(reg.ols)
print(stan.fit, pars = c("alpha", "beta1", "beta2", "beta3", "beta4", "sigma"))






















#' @title GOV 1347: Week 2 (Economics) Laboratory Session
#' @author Matthew E. Dardet
#' @date September 10, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(car)
library(tidyverse)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read, merge, and process data.
####----------------------------------------------------------#

# Load popular vote data. 
d_popvote <- read_csv("popvote_1948-2020.csv")

# Load economic data from FRED: https://fred.stlouisfed.org. 
# Variables, units, & ranges: 
# GDP, billions $, 1947-2024
# GDP_growth_quarterly, %
# RDPI, $, 1959-2024
# RDPI_growth_quarterly, %
# CPI, $ index, 1947-2024
# unemployment, %, 1948-2024
# sp500_, $, 1927-2024 
d_fred <- read_csv("fred_econ.csv")

# Load economic data from the BEA: https://apps.bea.gov/iTable/?reqid=19&step=2&isuri=1&categories=survey#eyJhcHBpZCI6MTksInN0ZXBzIjpbMSwyLDMsM10sImRhdGEiOltbImNhdGVnb3JpZXMiLCJTdXJ2ZXkiXSxbIk5JUEFfVGFibGVfTGlzdCIsIjI2NCJdLFsiRmlyc3RfWWVhciIsIjE5NDciXSxbIkxhc3RfWWVhciIsIjIwMjQiXSxbIlNjYWxlIiwiMCJdLFsiU2VyaWVzIiwiUSJdXX0=.
# GDP, 1947-2024 (all)
# GNP
# RDPI
# Personal consumption expenditures
# Goods
# Durable goods
# Nondurable goods
# Services 
# Population (midperiod, thousands)
d_bea <- read_csv("bea_econ.csv") |> 
  rename(year = "Year",
         quarter = "Quarter", 
         gdp = "Gross domestic product", 
         gnp = "Gross national product", 
         dpi = "Disposable personal income", 
         consumption = "Personal consumption expenditures", 
         goods = "Goods", 
         durables = "Durable goods", 
         nondurables = "Nondurable goods", 
         services = "Services", 
         pop = "Population (midperiod, thousands)")

# Filter and merge data. 
d_inc_econ <- d_popvote |> 
  filter(incumbent_party == TRUE) |> 
  select(year, pv, pv2p, winner) |> 
  left_join(d_fred |> filter(quarter == 2)) |> 
  left_join(d_bea |> filter(quarter == "Q2") |> select(year, dpi))
  # N.B. two different sources of data to use, FRED & BEA. 
  # We are using second-quarter data since that is the latest 2024 release. 
  # Feel free to experiment with different data/combinations!

####----------------------------------------------------------#
#### Understanding the relationship between economy and vote share. 
####----------------------------------------------------------#

# Create scatterplot to visualize relationship between Q2 GDP growth and 
# incumbent vote share. 
d_inc_econ |> 
  ggplot(aes(x = GDP_growth_quarterly, y = pv2p, label = year)) + 
  geom_text() + 
  geom_hline(yintercept = 50, lty = 2) + 
  geom_vline(xintercept = 0.01, lty = 2) +
  labs(x = "Second Quarter GDP Growth (%)", 
       y = "Incumbent Party's National Popular Vote Share") + 
  theme_bw()

# Remove 2020 from plot.
d_inc_econ_2 <- d_inc_econ |>
  filter(year != 2020)

d_inc_econ_2 |> 
  ggplot(aes(x = GDP_growth_quarterly, y = pv2p, label = year)) + 
  geom_text() + 
  geom_hline(yintercept = 50, lty = 2) + 
  geom_vline(xintercept = 0.01, lty = 2) + 
  labs(x = "Second Quarter GDP Growth (%)", 
       y = "Incumbent Party's National Popular Vote Share") + 
  theme_bw()

# Compute correlations between Q2 GDP growth and incumbent vote 2-party vote share.
cor(d_inc_econ$GDP_growth_quarterly, 
    d_inc_econ$pv2p)
cor(d_inc_econ_2$GDP_growth_quarterly, 
    d_inc_econ_2$pv2p)

# Fit bivariate OLS. 
reg_econ <- lm(pv2p ~ GDP_growth_quarterly, 
               data = d_inc_econ)
reg_econ |> summary()

reg_econ_2 <- lm(pv2p ~ GDP_growth_quarterly, 
                         data = d_inc_econ_2)
reg_econ_2 |> summary()

# Can add bivariate regression lines to our scatterplots. 
d_inc_econ |> 
  ggplot(aes(x = GDP_growth_quarterly, y = pv2p, label = year)) + 
  geom_text() + 
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 50, lty = 2) + 
  geom_vline(xintercept = 0.01, lty = 2) + 
  labs(x = "Second Quarter GDP Growth (%)", 
       y = "Incumbent Party's National Popular Vote Share", 
       title = "Y = 51.25 + 0.274 * X") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18))

d_inc_econ_2 |> 
  ggplot(aes(x = GDP_growth_quarterly, y = pv2p, label = year)) + 
  geom_text() + 
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_hline(yintercept = 50, lty = 2) + 
  geom_vline(xintercept = 0.01, lty = 2) + 
  labs(x = "Second Quarter GDP Growth (%)", 
       y = "Incumbent Party's National Popular Vote Share", 
       title = "Y = 49.38 + 0.737 * X") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 18))

# Evaluate the in-sample fit of your preferred model.
# TODO: 

# Model Testing: Leave-One-Out
(out_samp_pred <- predict(reg_econ_2, d_inc_econ[d_inc_econ$year == 2020,]))
(out_samp_truth <- d_inc_econ |> filter(year == 2020) |> select(pv2p))
out_samp_pred - out_samp_truth # Dangers of fundamentals-only model!
# https://www.nytimes.com/2020/07/30/business/economy/q2-gdp-coronavirus-economy.html

# Model Testing: Cross-Validation (One Run)
years_out_samp <- sample(d_inc_econ_2$year, 9) 
mod <- lm(pv2p ~ GDP_growth_quarterly, 
          d_inc_econ_2[!(d_inc_econ_2$year %in% years_out_samp),])
out_samp_pred <- predict(mod, d_inc_econ_2[d_inc_econ_2$year %in% years_out_samp,])
out_samp_truth <- d_inc_econ_2$pv2p[d_inc_econ_2$year %in% years_out_samp]
mean(out_samp_pred - out_samp_truth)

# Model Testing: Cross-Validation (1000 Runs)
out_samp_errors <- sapply(1:1000, function(i) {
  # TODO
})

####----------------------------------------------------------#
#### Predicting 2024 results using simple economy model. 
####----------------------------------------------------------#
# Sequester 2024 data.
GDP_new <- d_fred |> 
  filter(year == 2024 & quarter == 2) |> 
  select(GDP_growth_quarterly)

# Predict.
predict(reg_econ_2, GDP_new)

# Predict uncertainty.
# TODO 





#' @title GOV 1347: Week 7 (Ground Game) Laboratory Session
#' @author Matthew E. Dardet
#' @date October 15, 2024

####----------------------------------------------------------#
#### Preamble
####----------------------------------------------------------#

# Load libraries.
## install via `install.packages("name")`
library(geofacet)
library(ggpubr)
library(ggthemes)
library(haven)
library(kableExtra)
library(maps)
library(mgcv)
library(mgcViz)
library(RColorBrewer)
library(scales)
library(sf)
library(spData)
library(stargazer)
library(tidygeocoder)
library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(viridis)

## set working directory here
# setwd("~")

####----------------------------------------------------------#
#### Read, merge, and process data.
####----------------------------------------------------------#

# Read popular vote datasets. 
d_popvote <- read_csv("popvote_1948_2020.csv")
d_state_popvote <- read_csv("state_popvote_1948_2020.csv")
d_state_popvote[d_state_popvote$state == "District of Columbia",]$state <- "District Of Columbia"

# Read elector distribution dataset. 
d_ec <- read_csv("corrected_ec_1948_2024.csv")

# Read polling data. 
d_polls <- read_csv("national_polls_1968-2024.csv")
d_state_polls <- read_csv("state_polls_1968-2024.csv")

# Read turnout data. 
d_turnout <- read_csv("state_turnout_1980_2022.csv")

# Read county turnout. 
d_county_turnout <- read_csv("county_turnout.csv")

# Read state-level demographics.
d_state_demog <- read_csv("demographics.csv")

# Read county demographics. 
d_county_demog <- read_csv("county_demographics.csv")

# Read campaign events datasets. 
d_campaign_events <- read_csv("campaigns_2016_2024.csv")[,-1]

####----------------------------------------------------------#
#### Binomial simulations for election prediction. 
####----------------------------------------------------------#

# How many days until the election? 
election.day <- as.Date("2024-11-05")
current.date <- Sys.Date()

election.day-current.date
# 20 days!!!!!!

# Merge popular vote and polling data. 
d <- d_state_popvote |> 
  inner_join(d_state_polls |> filter(weeks_left == 3)) |> 
  mutate(state_abb = state.abb[match(state, state.name)])

# Generate state-specific univariate poll-based forecasts with linear model.
state_forecast <- list()
state_forecast_outputs <- data.frame()
for (s in unique(d$state_abb)) {
  # Democrat model.
  state_forecast[[s]]$dat_D <- d |> filter(state_abb == s, party == "DEM")
  state_forecast[[s]]$mod_D <- lm(D_pv ~ poll_support, 
                                  state_forecast[[s]]$dat_D)
  
  # Republican model.
  state_forecast[[s]]$dat_R <- d |> filter(state_abb == s, party == "REP")
  state_forecast[[s]]$mod_R <- lm(R_pv ~ poll_support, 
                                  state_forecast[[s]]$dat_R)
  
  if (nrow(state_forecast[[s]]$dat_R) > 2) {
    # Save state-level model estimates. 
    state_forecast_outputs <- rbind(state_forecast_outputs, 
                                    rbind(cbind.data.frame(
                                      intercept = summary(state_forecast[[s]]$mod_D)$coefficients[1,1], 
                                      intercept_se = summary(state_forecast[[s]]$mod_D)$coefficients[1,2],
                                      slope = summary(state_forecast[[s]]$mod_D)$coefficients[2,1], 
                                      state_abb = s, 
                                      party = "DEM"), 
                                    rbind(cbind.data.frame(
                                     intercept = summary(state_forecast[[s]]$mod_R)$coefficients[1,1],
                                     intercept_se = summary(state_forecast[[s]]$mod_R)$coefficients[1,2],
                                     slope = summary(state_forecast[[s]]$mod_R)$coefficients[2,1],
                                     state_abb = s,
                                     party = "REP"
                                    ))))
  }
}

# Make graphs of polls in different states/parties at different levels of strength/significance of outcome. 
state_forecast_trends <- state_forecast_outputs |> 
  mutate(`0` = intercept, 
         `25` = intercept + slope*25, 
         `50` = intercept + slope*50, 
         `75` = intercept + slope*75, 
         `100` = intercept + slope*100) |>
  select(-intercept, -slope) |> 
  gather(x, y, -party, -state_abb, -intercept_se) |> 
  mutate(x = as.numeric(x))

# Q: What's wrong with this map? 
# A: (1.) no polls in some states
#    (2.) very high variance for some states (Nevada)/negative slopes for others (Mississippi)
#    (3.) y is not always in the [0, 100] range
ggplot(state_forecast_trends, aes(x=x, y=y, ymin=y-intercept_se, ymax=y+intercept_se)) + 
  facet_geo(~ state_abb) +
  geom_line(aes(color = party)) + 
  geom_ribbon(aes(fill = party), alpha=0.5, color=NA) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Hypothetical Poll Support") +
  ylab("Predicted Voteshare\n(pv = A + B * poll)") +
  ggtitle("") +
  theme_bw()

state_forecast_trends |>
  filter(state_abb == "CA" | state_abb == "FL")|>
  ggplot(aes(x=x, y=y, ymin=y-intercept_se, ymax=y+intercept_se)) + 
  facet_wrap(~ state_abb) +
  geom_line(aes(color = party)) + 
  geom_hline(yintercept = 100, lty = 3) +
  geom_hline(yintercept = 0, lty = 3) + 
  geom_ribbon(aes(fill = party), alpha=0.5, color=NA) +
  ## N.B. You can, in fact, combine *different* data and aesthetics
  ##       in one ggplot; but this usually needs to come at the end 
  ##       and you must explicitly override all previous aesthetics
  geom_text(data = d |> filter(state_abb == "CA", party=="DEM"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "CA", party=="REP"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "FL", party=="DEM"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "FL", party=="REP"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Hypothetical Poll Support") +
  ylab("Predicted Voteshare\n(pv = A + B * poll)") +
  theme_bw()

# Merge turnout data into main dataset. 
d <- d |> 
  left_join(d_turnout, by = c("state", "year")) |> 
  filter(year >= 1980) # Filter to when turnout dataset begins. 

# Generate probabilistic univariate poll-based state forecasts. 
state_glm_forecast <- list()
state_glm_forecast_outputs <- data.frame()
for (s in unique(d$state_abb)) {
  # Democrat model. 
  state_glm_forecast[[s]]$dat_D <- d |> filter(state_abb == s, party == "DEM")
  state_glm_forecast[[s]]$mod_D <- glm(cbind(votes_D, vep - votes_D) ~ poll_support, # Cbind(N Success, N Total) for Binomial Model 
                                      state_glm_forecast[[s]]$dat_D, 
                                      family = binomial(link = "logit"))
  
  # Republican model. 
  state_glm_forecast[[s]]$dat_R <- d |> filter(state_abb == s, party == "REP")
  state_glm_forecast[[s]]$mod_R <- glm(cbind(votes_R, vep - votes_R) ~ poll_support, 
                                      state_glm_forecast[[s]]$dat_R, 
                                      family = binomial(link = "logit"))
  
  if (nrow(state_glm_forecast[[s]]$dat_R) > 2) {
    for (hypo_avg_poll in seq(from = 0, to = 100, by = 10)) { 
      # Democrat prediction. 
      D_pred_vote_prob <- predict(state_glm_forecast[[s]]$mod_D, 
                                  newdata = data.frame(poll_support = hypo_avg_poll), se = TRUE, type = "response")
      D_pred_qt <- qt(0.975, df = df.residual(state_glm_forecast[[s]]$mod_D)) # Used in the prediction interval formula. 
      
      # Republican prediction. 
      R_pred_vote_prob <- predict(state_glm_forecast[[s]]$mod_R, 
                                  newdata = data.frame(poll_support = hypo_avg_poll), se = TRUE, type = "response")
      R_pred_qt <- qt(0.975, df = df.residual(state_glm_forecast[[s]]$mod_R)) # Used in the prediction interval formula.
      
      # Save predictions. 
      state_glm_forecast_outputs <- rbind(state_glm_forecast_outputs, 
                                          cbind.data.frame(x = hypo_avg_poll,
                                                           y = D_pred_vote_prob$fit*100,
                                                           ymin = (D_pred_vote_prob$fit - D_pred_qt*D_pred_vote_prob$se.fit)*100,
                                                           ymax = (D_pred_vote_prob$fit + D_pred_qt*D_pred_vote_prob$se.fit)*100,
                                                           state_abb = s, 
                                                           party = "DEM"),
                                          cbind.data.frame(x = hypo_avg_poll,
                                                           y = R_pred_vote_prob$fit*100,
                                                           ymin = (R_pred_vote_prob$fit - R_pred_qt*R_pred_vote_prob$se.fit)*100,
                                                           ymax = (R_pred_vote_prob$fit + R_pred_qt*R_pred_vote_prob$se.fit)*100,
                                                           state_abb = s, 
                                                           party = "REP"))
    }
  }
}

# Make graphs of polls in different states/parties at different levels of strength/significance of outcome. 
ggplot(state_glm_forecast_outputs, aes(x=x, y=y, ymin=ymin, ymax=ymax)) + 
  facet_geo(~ state_abb) +
  geom_line(aes(color = party)) + 
  geom_ribbon(aes(fill = party), alpha=0.5, color=NA) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Hypothetical Poll Support") +
  ylab('Probability of State-Eligible Voter Voting for Party') +
  theme_bw()

state_glm_forecast_outputs |>
  filter(state_abb == "CA" | state_abb == "FL") |>
  ggplot(aes(x=x, y=y, ymin=ymin, ymax=ymax)) + 
  facet_wrap(~ state_abb) +
  geom_line(aes(color = party)) + 
  geom_ribbon(aes(fill = party), alpha=0.5, color=NA) +
  coord_cartesian(ylim=c(0, 100)) +
  geom_text(data = d |> filter(state_abb == "CA", party=="DEM"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "CA", party=="REP"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "FL", party=="DEM"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  geom_text(data = d |> filter(state_abb == "FL", party=="REP"), 
            aes(x = poll_support, y = D_pv, ymin = D_pv, ymax = D_pv, color = party, label = year), size=1.5) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  xlab("Hypothetical Poll Support") +
  ylab('Probability of\nState-Eligible Voter\nVoting for Party') +
  ggtitle("Binomial Logit") + 
  theme_bw() + 
  theme(axis.title.y = element_text(size=6.5))

# Simulating a distribution of potential election results in Pennsylvania for 2024. 
# First step. Let's use GAM (general additive model) to impute VEP in Pennsylvania for 2024 using historical VEP.

# Get historical eligible voting population in Pennsylvania. 
vep_PA_2020 <- as.integer(d_turnout$vep[d_turnout$state == "Pennsylvania" & d_turnout$year == 2020])
vep_PA <- d_turnout |> filter(state == "Pennsylvania") |> select(vep, year)

# Fit regression for 2024 VEP prediction. 
lm_vep_PA <- lm(vep ~ year, vep_PA)

plot(x = vep_PA$year, y = vep_PA$vep, xlab = "Year", ylab = "VEP", main = "Voting Eligible Population in Pennsylvania by Year")
abline(lm_vep_PA, col = "red")

vep_PA_2024_ols <- predict(lm_vep_PA, newdata = data.frame(year = 2024)) |> as.numeric()

gam_vep_PA <- mgcv::gam(vep ~ s(year), data = vep_PA)
print(plot(getViz(gam_vep_PA)) + l_points() + l_fitLine(linetype = 3) + l_ciLine(colour = 2) + theme_get()) 

# Use generalized additive model (GAM) to predict 2024 VEP in Pennsylvania.
vep_PA_2024_gam <- predict(gam_vep_PA, newdata = data.frame(year = 2024)) |> as.numeric()

# Take weighted average of linear and GAM predictions for final prediction. 
vep_PA_2024 <- as.integer(0.75*vep_PA_2024_gam + 0.25*vep_PA_2024_ols)
vep_PA_2024

# Split datasets by party. 
PA_D <- d |> filter(state == "Pennsylvania" & party == "DEM")
PA_R <- d |> filter(state == "Pennsylvania" & party == "REP")

# Fit Democrat and Republican models. 
PA_D_glm <- glm(cbind(votes_R, vep - votes_R) ~ poll_support, data = PA_D, family = binomial(link = "logit"))
PA_R_glm <- glm(cbind(votes_R, vep - votes_R) ~ poll_support, data = PA_R, family = binomial(link = "logit"))

# Get predicted draw probabilities for D and R. 
(PA_pollav_D <- d_state_polls$poll_support[d_state_polls$state == "Pennsylvania" & d_state_polls$weeks_left == 3 & d_state_polls$party == "DEM"] |> mean(na.rm = T))
(PA_pollav_R <- d_state_polls$poll_support[d_state_polls$state == "Pennsylvania" & d_state_polls$weeks_left == 3 & d_state_polls$party == "REP"] |> mean(na.rm = T))
(PA_sdpoll_D <- sd(d_state_polls$poll_support[d_state_polls$state == "Pennsylvania" & d_state_polls$weeks_left == 3 & d_state_polls$party == "DEM"] |> na.omit()))
(PA_sdpoll_R <- sd(d_state_polls$poll_support[d_state_polls$state == "Pennsylvania" & d_state_polls$weeks_left == 3 & d_state_polls$party == "REP"] |> na.omit()))

(prob_D_vote_PA_2024 <- predict(PA_D_glm, newdata = data.frame(poll_support = PA_pollav_D), se = TRUE, type = "response")[[1]] |> as.numeric())
(prob_R_vote_PA_2024 <- predict(PA_R_glm, newdata = data.frame(poll_support = PA_pollav_R), se = TRUE, type = "response")[[1]] |> as.numeric())

# Get predicted distribution of draws from the population. 
sim_D_votes_PA_2024 <- rbinom(n = 10000, size = vep_PA_2024, prob = prob_D_vote_PA_2024)
sim_R_votes_PA_2024 <- rbinom(n = 10000, size = vep_PA_2024, prob = prob_R_vote_PA_2024)

# Simulating a distribution of election results: Harris PA PV. 
hist(sim_D_votes_PA_2024, breaks = 100, col = "blue", main = "Predicted Turnout Draws for Harris \n from 10,000 Binomial Process Simulations")

# Simulating a distribution of election results: Trump PA PV. 
hist(sim_R_votes_PA_2024, breaks = 100, col = "red", main = "Predicted Turnout Draws for Trump \n from 10,000 Binomial Process Simulations")

# Simulating a distribution of election results: Trump win margin. 
sim_elxns_PA_2024 <- ((sim_R_votes_PA_2024-sim_D_votes_PA_2024)/(sim_D_votes_PA_2024 + sim_R_votes_PA_2024))*100
hist(sim_elxns_PA_2024, breaks = 100, col = "firebrick1", main = "Predicted Draws of Win Margin for Trump \n from 10,000 Binomial Process Simulations", xlim = c(0, 0.4))

# Simulations incorporating prior for SD. 
sim_D_votes_PA_2024_2 <- rbinom(n = 10000, size = vep_PA_2024, prob = rnorm(10000, PA_pollav_D/100, PA_sdpoll_D/100))
sim_R_votes_PA_2024_2 <- rbinom(n = 10000, size = vep_PA_2024, prob = rnorm(10000, PA_pollav_R/100, PA_sdpoll_R/100))
sim_elxns_PA_2024_2 <- ((sim_R_votes_PA_2024_2-sim_D_votes_PA_2024_2)/(sim_D_votes_PA_2024_2 + sim_R_votes_PA_2024_2))*100
h <- hist(sim_elxns_PA_2024_2, breaks = 100, col = "firebrick1")
cuts <- cut(h$breaks, c(-Inf, 0, Inf))
plot(h, yaxt = "n", bty = "n", xlab = "", ylab = "", main = "", xlim = c(-35, 35), col = c("blue", "red")[cuts], cex.axis=0.8)

####----------------------------------------------------------#
#### Ground Game: Field offices and campaign events. 
####----------------------------------------------------------#

# Where should campaigns build field offices? 
fo_2012 <- read_csv("fieldoffice_2012_bycounty.csv")

lm_obama <- lm(obama12fo ~ romney12fo + 
                 swing + 
                 core_rep + 
                 swing:romney12fo + 
                 core_rep:romney12fo + 
                 battle + 
                 medage08 + 
                 pop2008 + 
                 pop2008^2 + 
                 medinc08 + 
                 black + 
                 hispanic + 
                 pc_less_hs00 + 
                 pc_degree00 + 
                 as.factor(state), 
               fo_2012)

lm_romney <- lm(romney12fo ~ 
                  obama12fo + 
                  swing + 
                  core_dem + 
                  swing:obama12fo + 
                  core_dem:obama12fo + 
                  battle + 
                  medage08 + 
                  pop2008 + 
                  pop2008^2 + 
                  medinc08 + 
                  black + 
                  hispanic + 
                  pc_less_hs00 + 
                  pc_degree00 + 
                  as.factor(state),
                  fo_2012)

stargazer(lm_obama, lm_romney, header=FALSE, type='latex', no.space = TRUE,
          column.sep.width = "3pt", font.size = "scriptsize", single.row = TRUE,
          keep = c(1:7, 62:66), omit.table.layout = "sn",
          title = "Placement of Field Offices (2012)")

# Effects of field offices on turnout and vote share. 
fo_dem <- read_csv("fieldoffice_2004-2012_dems.csv")

ef_t <- lm(turnout_change ~ dummy_fo_change + battle + dummy_fo_change:battle + as.factor(state) + as.factor(year), fo_dem)

ef_d <- lm(dempct_change ~ dummy_fo_change + battle + dummy_fo_change:battle + as.factor(state) + as.factor(year), fo_dem)

stargazer(ef_t, ef_d, header=FALSE, type='latex', no.space = TRUE,
          column.sep.width = "3pt", font.size = "scriptsize", single.row = TRUE,
          keep = c(1:3, 53:54), keep.stat = c("n", "adj.rsq", "res.dev"),
          title = "Effect of DEM Field Offices on Turnout and DEM Vote Share (2004-2012)")

# Field Strategies of Obama, Romney, Clinton, and Trump in 2016. 
fo_add <- read_csv("fieldoffice_2012-2016_byaddress.csv")



# Clinton 2016 Field Offices - Obama 2008 Field Offices. 



# Case Study: Wisconsin Field Offices in 2012 and 2016 



# Visualizing campaign events. 
d_campaign_events$party[d_campaign_events$candidate %in% c("Trump / Pence", "Trump", "Pence", "Trump/Pence", "Vance")] <- "REP"
d_campaign_events$party[d_campaign_events$candidate %in% c("Biden / Harris", "Biden", "Harris", "Biden/Harris", "Walz", "Kaine", "Clinton", "Clinton / Kaine")] <- "DEM"
p.ev.1 <- d_campaign_events |> group_by(date, party) |> summarize(n_events = n(), year) |> filter(year == 2016) |> ggplot(aes(x = date, y = n_events, color = party)) + geom_point() + geom_smooth() + ggtitle("2016") + theme_bw()
p.ev.2 <- d_campaign_events |> group_by(date, party) |> summarize(n_events = n(), year) |> filter(year == 2020) |> ggplot(aes(x = date, y = n_events, color = party)) + geom_point() + geom_smooth() + ggtitle("2020") +  theme_bw()
p.ev.3 <- d_campaign_events |> group_by(date, party) |> summarize(n_events = n(), year) |> filter(year == 2024) |> ggplot(aes(x = date, y = n_events, color = party)) + geom_point() + geom_smooth() + ggtitle("2024") + theme_bw()

ggarrange(p.ev.1, p.ev.2, p.ev.3)

# Mapping campaign events. 
d_campaign_events <- d_campaign_events |> 
  geocode(city = city, state = state, method = 'osm', lat = latitude , long = longitude)

d_campaign_events <- read_csv("campaign_events_geocoded.csv")
d_campaign_events$party[d_campaign_events$candidate %in% c("Trump / Pence", "Trump", "Pence", "Trump/Pence", "Vance")] <- "REP"
d_campaign_events$party[d_campaign_events$candidate %in% c("Biden / Harris", "Biden", "Harris", "Biden/Harris", "Walz", "Kaine", "Clinton", "Clinton / Kaine")] <- "DEM"

us_geo <- states(cb = TRUE) |> 
  shift_geometry() |> 
  filter(STUSPS %in% unique(fo_add$state))

d_ev_transformed <- st_as_sf(d_campaign_events |> drop_na(), coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(crs = st_crs(us_geo)) |>
  shift_geometry() |> 
  st_make_valid()

(ev16 <- ggplot() +
   geom_sf(data = us_geo) + 
   geom_sf(data = d_ev_transformed |> filter(year == 2016), aes(color = party), size = 3, alpha = 0.75, pch = 3) +
   ggtitle("2016 Campaign Events") +
   theme_void())

(ev20 <- ggplot() +
   geom_sf(data = us_geo) + 
   geom_sf(data = d_ev_transformed |> filter(year == 2020), aes(color = party), size = 3, alpha = 0.75, pch = 3) +
   ggtitle("2020 Campaign Events") +
   theme_void())

(ev24 <- ggplot() +
   geom_sf(data = us_geo) + 
   geom_sf(data = d_ev_transformed |> filter(year == 2024), aes(color = party), size = 3, alpha = 0.75, pch = 3) +
   ggtitle("2024 Campaign Events") +
   theme_void())

# Can the number of campaign events predict state-level vote share? 
d_ev_state <- d_campaign_events |> 
  group_by(year, state, party) |> 
  summarize(n_events = n()) |> 
  pivot_wider(names_from = party, values_from = n_events) |> 
  rename(n_ev_D = DEM, n_ev_R = REP)
d_ev_state$n_ev_D[which(is.na(d_ev_state$n_ev_D))] <- 0
d_ev_state$n_ev_R[which(is.na(d_ev_state$n_ev_R))] <- 0
d_ev_state$ev_diff_D_R <- d_ev_state$n_ev_D - d_ev_state$n_ev_R
d_ev_state$ev_diff_R_D <- d_ev_state$n_ev_R - d_ev_state$n_ev_D

d <- d |> 
  left_join(d_ev_state, by = c("year", "state_abb" = "state"))

lm_ev_D <- lm(D_pv2p ~ n_ev_D + ev_diff_D_R, data = d)
lm_ev_R <- lm(R_pv2p ~ n_ev_R + ev_diff_R_D, data = d)

summary(lm_ev_D)
summary(lm_ev_R)

stargazer(lm_ev_D, lm_ev_R, header=FALSE, type='latex', no.space = TRUE,
          column.sep.width = "3pt", font.size = "scriptsize", single.row = TRUE,
          keep.stat = c("n", "adj.rsq", "res.dev"),
          title = "Association Between Campaign Events and Voting Outcomes of Interest")


####----------------------------------------------------------#
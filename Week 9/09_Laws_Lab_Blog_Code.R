#' @title GOV 1347: Week 9 (Ground Game) Laboratory Session
#' @author Matthew E. Dardet
#' @date October 30, 2024

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
library(readstata13)
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

####-------------------------------------------------------------------------#
#### Read, merge, and process data.
####-------------------------------------------------------------------------#

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

# Read state-level demographics.
d_state_demog <- read_csv("demographics.csv")

# Read election administration and laws data. 
d.mepi.presel <- read_csv("p0residential_EPI_index.csv")
d.mepi.midel <- read_csv("midterm_EPI_index.csv")
d.mepi <- rbind(d.mepi.presel, d.mepi.midel) |> 
  arrange(state_abbv, year); rm(d.mepi.presel, d.mepi.midel)
d.covi <- read_csv("covi.csv")

####-------------------------------------------------------------------------#
#### Visualizing the variation in state-level election laws. 
####-------------------------------------------------------------------------#

# Read states shapefile. 
us_geo <- st_read("states.shp")

d.geo_covi <- us_geo |> 
  filter(us_geo$STUSPS %in% unique(d.covi$state)) |> 
  left_join(d.covi, by = c("STUSPS" = "state"))

# COVI over time. 
for (i in 1:length(unique(d.geo_covi$year)[-9])) {
  yr <- unique(d.geo_covi$year)[i]
  plot <- ggplot() + 
    geom_sf(data = d.geo_covi |> filter(year == yr), aes(fill = FinalCOVI)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    ggtitle(paste0("Cost of Voting Index by State, ", yr)) +
    theme_minimal()
  print(plot)
}

# Initial COVI from 2024. 
ggplot() + 
  geom_sf(data = d.geo_covi |> filter(year == 2024), aes(fill = InitialCOVI)) + 
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  ggtitle("Cost of Voting Index by State, 2024") +
  theme_minimal()

d.geo_mepi <- us_geo |> 
  filter(us_geo$STUSPS %in% unique(d.mepi$state_abbv)) |> 
  left_join(d.mepi, by = c("STUSPS" = "state_abbv"))

# MEPI over time. 
for (i in 1:length(unique(d.geo_mepi$year))) {
  yr <- unique(d.geo_mepi$year)[i]
  plot <- ggplot() + 
    geom_sf(data = d.geo_mepi |> filter(year == yr), aes(fill = index)) + 
    scale_fill_viridis_c(option = "plasma", direction = -1) +
    ggtitle(paste0("Election Performance Index by State, ", yr)) +
    theme_minimal()
  print(plot)
}

####-------------------------------------------------------------------------#
#### Do variation in laws predict voter turnout and election outcomes? 
####-------------------------------------------------------------------------#

d <- d_state_popvote |> 
  mutate(state_abb = state.abb[match(state, state.name)]) |> 
  left_join(d.covi |> select(state, year, FinalCOVI), by = c("state_abb" = "state", "year")) |> 
  left_join(d.mepi |> select(state_abbv, year, "MEPI" = index), by = c("state_abb" = "state_abbv", "year"))

# COVI regression. 
lm(D_pv2p ~ FinalCOVI, 
   data = d) |> summary()
lm(D_pv2p ~ FinalCOVI + factor(state), 
   data = d) |> summary()

# MEPI regression. 
lm(D_pv2p ~ MEPI, 
   data = d) |> summary()
lm(D_pv2p ~ MEPI + factor(state),
   data = d) |> summary()















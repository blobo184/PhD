## Script to poststratify 

## Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Libraries
library(tidyverse)
library(reshape2)
source("00_Utility.R")

##  Poststrat frame for 2012
# Load and amend
load("../Data/post_2012.RData")
post_2012 <- post_2012 %>%
  mutate(Area = ST,
         Ethnicity = Race,
         Female = ifelse(Gender == "Female", 1, 0),
         Campaign_week = "W1") %>%
  dplyr::select(-ST, -Gender, -Race)

# Apend Region
Region <- get_ST_to_Region()
post_2012 <- left_join(post_2012, Region, by = "Area")

# Append past dem
load("../Data/anes_2012_tidy.RData")
dat <- dat %>%
  group_by(Area) %>%
  summarise(past_dem = unique(past_dem))
post_2012 <- left_join(post_2012, dat, by = "Area")

# # Append Turnout measure
# load("../Results/Turnout/US_turnout12.RData")
# post_2012 <- cbind(post_2012, post_row_turnout[2])
# rm(dat, post_row_turnout)

## Get Truth (i.e. actual past Dem as % of totla population)
load("../Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2012) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, democrat_2 = 'democratic-farmer-labor', totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         democrat_2 = as.numeric(as.character(democrat_2)),
         democrat = ifelse(is.na(democrat), democrat_2, democrat),
         Dem = democrat / (republican+democrat)) %>%
  dplyr::select(Area = state_po, past_dem=Dem) 

past_turnout <- read.csv("../Data/Turnout/State_turnout.csv")
states <- get_State_to_ST() # Get State to St
past_turnout <- left_join(past_turnout, states, by = c("X" = "State")) %>% # append ST to turnout frame
  dplyr::filter(Year == "2012") %>%
  dplyr::select(Area = ST, Turnout = Vote)

Past_vote <- left_join(Past_vote, past_turnout, by = "Area") %>%
  mutate(Truth = past_dem*Turnout) %>%
  dplyr::select(Area, Truth, Turnout)
rm(past_turnout, x)

post_2012 <- left_join(post_2012, Past_vote, by = "Area")

# Save
save(post_2012, file = "../Data/post_2012_final.RData")

##  Poststrat frame for 2016
# Load and amend
load("../Data/post_2016.RData")
post_2016 <- post_2016 %>%
  mutate(Area = ST,
         Ethnicity = Race,
         Female = ifelse(Gender == "Female", 1, 0),
         Campaign_week = "W1") %>%
  dplyr::select(-ST, -Gender, -Race)

# Apend Region
Region <- get_ST_to_Region()
post_2016 <- left_join(post_2016, Region, by = "Area")

# Append past dem
load("../Data/anes_2016_tidy.RData")
dat <- dat %>%
  group_by(Area) %>%
  summarise(past_dem = unique(past_dem))
post_2016 <- left_join(post_2016, dat, by = "Area")

# Append Turnout measure
# load("../Results/Turnout/US_turnout16.RData")
# post_2016 <- cbind(post_2016, post_row_turnout[2])
# rm(dat, post_row_turnout)

## Get Truth (i.e. actual past Dem as % of totla population)
load("../Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2016) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         Dem = democrat / (republican+democrat)) %>%
  dplyr::select(Area = state_po, past_dem=Dem) 


past_turnout <- read.csv("../Data/Turnout/State_turnout.csv")
states <- get_State_to_ST() # Get State to St
past_turnout <- left_join(past_turnout, states, by = c("X" = "State")) %>% # append ST to turnout frame
  dplyr::filter(Year == "2016") %>%
  dplyr::select(Area = ST, Turnout = Vote)

Past_vote <- left_join(Past_vote, past_turnout, by = "Area") %>%
  mutate(Truth = past_dem*Turnout) %>%
  dplyr::select(Area, Truth, Turnout)
rm(past_turnout, x)
post_2016 <- left_join(post_2016, Past_vote, by = "Area")

# save
save(post_2016, file = "../Data/post_2016_final.RData")

################################ End #############################
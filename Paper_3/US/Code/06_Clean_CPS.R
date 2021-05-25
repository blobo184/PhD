## Clean cps turnout data and create turnout measure by 
## demographic subgroups

## Set directory
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)
library(ipumsr)
source("00_Utility.R")

## Load cps data, needs to be unacked using ipumsr package
ddi <- read_ipums_ddi("../Data/Turnout/cps_00005.xml")
data <- read_ipums_micro(ddi)

## recoding data
data <- data %>%
  filter(VOTED != 99) %>% # Exclude those not surveyed for cps
  filter(AGE > 17) %>%
  mutate(
    Ethnicity = case_when(
      HISPAN %in% 100:612 ~ "Hispanic",
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      RACE == 801 ~ "Black",
      RACE %in% 300:700 ~ "Other",
      RACE %in% 802:830 ~ "Other",
      TRUE ~ NA_character_),
    Marital = case_when(
      MARST %in% 1:2 ~ "Married",
      MARST %in% 3:4 ~ "Sep-Divorced",
      MARST == 5 ~ "Widowed",
      MARST == 6 ~ "Single",
      MARST == 7 ~ "Widowed",
      TRUE ~ NA_character_),
    Age = case_when(
      AGE %in% 18:34 ~ "18-34",
      AGE %in% 35:44 ~ "35-44",
      AGE %in% 45:64 ~ "45-64",
      AGE > 64 ~ "65+",
      TRUE ~ NA_character_),
    Female = ifelse(SEX == 2, 1, 0),
    Education = case_when(
      EDUC %in% 2:71 ~" No HS diploma",
      EDUC == 73 ~ "HS diploma",
      EDUC %in% 80:90 ~ "Some college",
      EDUC %in% 91:111 ~ "College grad",
      EDUC %in% 123:125 ~ "Postgrad",
      TRUE ~ NA_character_),
    Voted = case_when(
      VOTED == 1 ~ "DNV",
      VOTED == 2 ~ "Voted",
      VOTED == 96 ~ NA_character_,
      VOTED == 97 ~ "DNV",
      VOTED == 98 ~ NA_character_),
    Area = FIPS_to_ST(as.numeric(STATEFIP))) 


## separate into necessary years and save
cps08 <-  data %>%
  filter(AGE  > 17) %>%
  filter(YEAR == 2008) %>%
  drop_na() %>%
  dplyr::select(Area,  Age, Ethnicity, Marital, Female, Education, Voted)

## Calculate past majority
load("../Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2008) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         Dem = democrat / (republican+democrat),
         Rep = republican / (republican+democrat),
         Majority = abs(Dem - Rep)) %>%
  dplyr::select(Area = state_po, Majority08 = Majority) 
cps08 <- left_join(cps08, Past_vote, by = "Area")
rm(Past_vote,x)

## Append past turnout
past_turnout <- read.csv("../Data/Turnout/State_turnout.csv") # Load
states <- get_State_to_ST() # Get State to St
past_turnout <- left_join(past_turnout, states, by = c("X" = "State")) %>% # append ST to turnout frame
  dplyr::filter(Year == "2008") %>%
  dplyr::select(Area = ST, Turnout08 = Vote)
rm(states)
cps08 <- left_join(cps08, past_turnout, by = "Area")

## Finally create binary vote and rescale area-vars
cps08 <- cps08 %>%
  mutate(Vote = ifelse(Voted == "Voted", 1, 0),
         Majority08 = arm::rescale(Majority08),
         Turnout08 = arm::rescale(Turnout08))

# Save
save(cps08, file= "../Data/Turnout/cps08.RData")

## 2012
# Filter main data frame and select vars
cps12 <-  data %>%
  filter(AGE  > 17) %>%
  filter(YEAR == 2012) %>%
  drop_na() %>%
  dplyr::select(Area,  Age, Ethnicity, Marital, Female, Education, Voted)

## Calculate past majority
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
         Dem = democrat / (republican+democrat),
         Rep = republican / (republican+democrat),
         Majority = abs(Dem - Rep)) %>%
  dplyr::select(Area = state_po, Majority12=Majority) 

cps12 <- left_join(cps12, Past_vote, by = "Area")
rm(Past_vote, x)

## Append past turnout
past_turnout <- read.csv("../Data/Turnout/State_turnout.csv") # Load
states <- get_State_to_ST() # Get State to St
past_turnout <- left_join(past_turnout, states, by = c("X" = "State")) %>% # append ST to turnout frame
  dplyr::filter(Year == "2012") %>%
  dplyr::select(Area = ST, Turnout12 = Vote)
rm(states)
cps12 <- left_join(cps12, past_turnout, by = "Area")

cps12 <- cps12 %>%
  mutate(Vote = ifelse(Voted == "Voted", 1, 0),
         Majority12 = arm::rescale(Majority12),
         Turnout12 = arm::rescale(Turnout12))

## Save
save(cps12, file= "../Data/Turnout/cps12.RData")

############################ End ################################
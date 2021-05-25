## Script to clean ANES 2008 data

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## LIbraries
library(rio)
library(tidyverse)

## Load data
anes_2008 <- rio::import("../Data/anes_timeseries_2008.sav")

## Vote intention
anes_2008 <- anes_2008 %>%
  mutate(Vote = case_when(
    V083169 == 5 ~ "WNV", ## Will you vote Q
    V083169 == -8 ~ NA_character_,
    V083169 == -9 ~ NA_character_,
    V083169a == 1 ~ "Dem",
    V083169a == 2 ~ "Rep",
    V083169a == 7 ~ "Other",
    V083169a == 5 ~ "WNV",
    V083169a %in% -8:-9 ~ "WNV",
    TRUE ~ rio::characterize(V083169a)))
#table(anes_2008$Vote, useNA = "ifany")

## Gender
anes_2008 <- anes_2008 %>%
  mutate(Female = ifelse(V081101 == 2, 1, 0))

#table(anes_2008$Female, anes_2008$V081101, useNA = "ifany")

## Ethnicity
anes_2008 <- anes_2008 %>%
  mutate(Ethnicity = case_when(
    V083251a == 10 ~ "Black",
    V083251a %in% 20:30 ~ "Other",
    V083251a == 40 ~ "Hispanic",
    V083251a == 50 ~ "White",
    V083251a == 81 ~ "Black",
    V083251a %in% 82:83 ~ "Other",
    V083251a == 84 ~ "Hispanic",
    V083251a == 85 ~ "White",
    V083251a == 90 ~ "Other",
    TRUE ~ NA_character_))

#table(anes_2008$Ethnicity,anes_2008$V083251a, useNA = "ifany")

## Age
anes_2008 <- anes_2008 %>%
  dplyr::filter(V083215x <0 | V083215x> 17) %>% # Exclude <18s, but not NAs
  mutate(Age = case_when(
    V083215x > 17 & V083215x < 35 ~ "18-34",
    V083215x > 34 & V083215x < 45 ~ "35-44",
    V083215x > 44 & V083215x < 65 ~ "45-64",
    V083215x > 64 ~ "65+",
    TRUE ~ NA_character_))

#table(anes_2008$Age, anes_2008$V083215x)

## State
anes_2008 <- anes_2008 %>%
  mutate(ST = V081201a)

## Education
anes_2008 <- anes_2008 %>%
  mutate(Education = case_when(
    V083218x %in% 0:2 ~ "No HS diploma",
    V083218x == 3 ~ "HS diploma",
    V083218x == 4 ~ "Some college",
    V083218x %in% 5:6 ~ "College grad",
    V083218x == 7 ~ "Postgrad",
    TRUE ~ NA_character_))
  
#table(anes_2008$Education,anes_2008$V083218x, useNA = "ifany")

## Marital
anes_2008 <- anes_2008 %>%
  mutate(Marital = case_when(
    V083216x == 1 ~ "Married",
    V083216x %in% 2:3 ~ "Sep-Divorced",
    V083216x == 4 ~ "Widowed",
    V083216x %in% 5:6 ~ "Single",
    TRUE ~ NA_character_))

#table(anes_2008$Marital, anes_2008$V083216x, useNA = "ifany")    

## Campaign week
anes_2008 <- anes_2008 %>%
  mutate(temp_date = paste0("2008",V082001c),
    temp_date = as.Date(temp_date, format = "%Y%m%d"), 
    temp_date = as.numeric(abs(temp_date - max(temp_date))),
    Campaign_week = cut(temp_date,
                             breaks = seq(-1,56, by = 7),
                             labels = 1:8),
    Campaign_week = ifelse(is.na(Campaign_week), 8, Campaign_week),
    Campaign_week = paste0("W", Campaign_week))


## Weight
anes_2008 <- anes_2008 %>%
  mutate(wt = V080101a)
         
## Select variables
anes_2008 <- anes_2008 %>%
  dplyr::select(Area = ST, Vote, Female, Ethnicity, Age, Education, Marital, Campaign_week, wt) %>%
  dplyr::filter(Vote == "Dem"|Vote == "Rep") %>%
  mutate(Dem = ifelse(Vote == "Dem", 1,0))

## Append Past vote
load("../Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2004) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, democrat_2 = 'democratic-farmer-labor',totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         democrat_2 = as.numeric(as.character(democrat_2)),
         democrat = ifelse(is.na(democrat), democrat_2, democrat),
         Dem = democrat / (republican+democrat)) %>%
  dplyr::select(Area = state_po, past_dem = Dem) 

anes_2008 <- left_join(anes_2008, Past_vote, by = "Area") %>%
  mutate(past_dem = arm::rescale(past_dem))

## Append region
source("../Code/00_Utility.R")
codes <- data.frame(FIPS=as.numeric(1:100)) %>%
  mutate(Area = FIPS_to_ST(FIPS)) %>%
  drop_na()%>%
  mutate(State = FIPS_to_State(FIPS),
         Region = State_to_Region(State)) %>%
  dplyr::select(Area, Region)
anes_2008 <- left_join(anes_2008, codes, by = "Area")
rm(x,Past_vote, codes)

## Save anes_2008 as dat
dat <- anes_2008 %>%
  drop_na() %>%
  mutate(Year = 2008)

save(dat, file = "../Data/anes_2008_tidy.RData")

################################ End #############################
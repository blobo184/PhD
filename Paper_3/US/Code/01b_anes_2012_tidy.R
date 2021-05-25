## Script to clean ANEs 2012 data

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(rio)
library(tidyverse)
source("00_Utility.R")

## Load raw data
anes_2012 <- rio::import("../Data/anes_timeseries_2012.sav")

## Vote intention 
## Those who refuse will be coded as NA and later imputed or excluded.
anes_2012 <- anes_2012 %>%
  mutate(Voted = case_when(prevote_presvtwho == 1 ~ "Dem",
                           prevote_presvtwho == 2 ~ "Rep",
                           prevote_presvtwho %in% 3:5 ~ "Other",
                           TRUE ~ NA_character_), 
         vi = case_when(
           prevote_regint == 2 ~ "WNV",
           prevote_intpres == 2 ~ "WNV",
           prevote_intpreswho == 1 ~ "Dem",
           prevote_intpreswho == 2 ~ "Rep",
           prevote_intpreswho == 5 ~ "Other",
           prevote_intpreswho %in% -8:-9 ~ "WNV",
           TRUE ~ NA_character_),
         Vote = ifelse(is.na(Voted), vi, Voted))

#table(anes_2012$Vote, useNA = "ifany")

## State
# If registration ST is diff to sample ST, take registration ST
anes_2012 <- anes_2012 %>%
  mutate(ST = case_when(
    prevote_reg_samestate == 0 ~ FIPS_to_ST(prevote_reg_state), 
    TRUE ~ sample_state))


## Age
anes_2012 <- anes_2012 %>%
  dplyr::filter(dem_age_r_x <0 | dem_age_r_x> 17) %>% # Exclude <18s, but not NAs
  mutate(Age = case_when(
    dem_age_r_x > 17 & dem_age_r_x < 35 ~ "18-34",
    dem_age_r_x > 34 & dem_age_r_x < 45 ~ "35-44",
    dem_age_r_x > 44 & dem_age_r_x < 65 ~ "45-64",
    dem_age_r_x > 64 ~ "65+",
    TRUE ~ NA_character_))

#table(anes_2012$Age, anes_2012$dem_age_r_x, useNA = "ifany")

## Education
anes_2012 <- anes_2012 %>%
  mutate(Education = case_when(
    dem_edu %in% 1:8 ~ "No HS diploma",
    dem_edu == 9 ~ "HS diploma",
    dem_edu == 10 ~ "Some college",
    dem_edu > 10 & dem_edu < 14 ~ "College grad",
    dem_edu > 13 & dem_edu < 17 ~ "Postgrad",
    dem_edu == 90 ~ "HS or less",
    TRUE ~ NA_character_))

#table(anes_2012$Education,rio::characterize(anes_2012$dem_edu))

## Gender
anes_2012 <- anes_2012 %>%
  mutate(Female = case_when(
    gender_respondent_x == 1 ~ 0,
    gender_respondent_x == 2 ~ 1,
    TRUE ~ NA_real_))

#table(anes_2012$Female, anes_2012$V161342, useNA = "ifany")                     

## Ethnicity
anes_2012 <- anes_2012 %>%
  mutate(Ethnicity = case_when(
    dem_raceeth_x == 1 ~ "White",
    dem_raceeth_x == 2 ~ "Black",
    dem_raceeth_x == 3 ~ "Other",
    dem_raceeth_x == 4 ~ "Other",
    dem_raceeth_x == 5 ~ "Hispanic",
    dem_raceeth_x == 6 ~ "Other",
    TRUE ~ NA_character_))

#table(anes_2012$Ethnicity, rio::characterize(anes_2012$dem_raceeth_x), useNA = "ifany")

## Marital status
anes_2012 <- anes_2012 %>%
  mutate(Marital = case_when(
    dem_marital == 1 ~ "Married",
    dem_marital == 2 ~ "Married",
    dem_marital == 3 ~ "Widowed",
    dem_marital == 4 ~ "Sep-Divorced",
    dem_marital == 5 ~ "Sep-Divorced",
    dem_marital == 6 ~ "Single",
    TRUE ~ NA_character_))

#table(anes_2012$Marital,rio::characterize(anes_2012$dem_marital), useNA = "ifany")

## Date - at present i've separarted into 9 weeks
anes_2012 <- anes_2012 %>%
  mutate(temp_date = ifelse(admin_pre_ftf_iwdatebeg != "", admin_pre_ftf_iwdatebeg, admin_pre_web1_iwdatebeg),
         temp_date = as.Date(temp_date, format = "%Y%m%d"), 
         temp_date = as.numeric(abs(temp_date - max(temp_date))),
         Campaign_week = cut(temp_date,
                             breaks = seq(-1,56, by = 7),
                             labels = 1:8),
         Campaign_week = ifelse(is.na(Campaign_week), 8, Campaign_week),
         Campaign_week = paste0("W", Campaign_week))

#table(anes_2012$Campaign_week,anes_2012$temp_date)

# Select vars
anes_2012 <- anes_2012 %>%
  dplyr::select(Vote, Area = ST, Ethnicity, Education, Age, Female, Marital, Campaign_week, wt = weight_full) %>%
  dplyr::filter(Vote == "Dem"|Vote == "Rep") %>%
  mutate(Dem = ifelse(Vote == "Dem", 1,0))

## Append Past vote
load("../Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2008) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         Dem = democrat / (republican+democrat)) %>%
  dplyr::select(Area = state_po, past_dem=Dem) 

anes_2012 <- left_join(anes_2012, Past_vote, by = "Area") %>%
  mutate(past_dem = arm::rescale(past_dem))

## Append region
source("../Code/00_Utility.R")
codes <- data.frame(FIPS=as.numeric(1:100)) %>%
  mutate(Area = FIPS_to_ST(FIPS)) %>%
  drop_na()%>%
  mutate(State = FIPS_to_State(FIPS),
         Region = State_to_Region(State)) %>%
  dplyr::select(Area, Region)
anes_2012 <- left_join(anes_2012, codes, by = "Area")
rm(x,Past_vote, codes)


## save 
dat <- anes_2012 %>%
  drop_na() %>%
  mutate(Year = 2012)
save(dat, file = "../Data/anes_2012_tidy.RData")

############################### End #############
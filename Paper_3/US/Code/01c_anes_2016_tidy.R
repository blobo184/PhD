## Script to clean ANEs 2016 data

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(rio)
library(tidyverse)
source("00_Utility.R")

## Load raw data
anes_2016 <- rio::import("../Data/anes_timeseries_2016.sav")

## Vote intention 
## Those who refuse will be coded as NA and later imputed or excluded.
anes_2016 <- anes_2016 %>%
  mutate(V161027 = rio::characterize(V161027),
         Voted = case_when(V161027 == "1. Hillary Clinton" ~ "Dem",
                           V161027 == "2. Donald Trump" ~ "Rep",
                           V161027 == "3. Gary Johnson" ~ "Other",
                           V161027 == "4. Jill Stein" ~ "Other",
                           V161027 == "5. Other candidate {SPECIFY}" ~ "Other",
                           TRUE ~ NA_character_), 
         V161031 = rio::characterize(V161031),
         vi = case_when(
           V161020 == 2 ~ "WNV",
           V161030 == 2 ~ "WNV",
           V161031 == "1. Hillary Clinton" ~ "Dem",
           V161031 == "2. Donald Trump" ~ "Rep",
           V161031 == "3. Gary Johnson" ~ "Other",
           V161031 == "4. Jill Stein" ~ "Other",
           V161031 == "5. Other candidate {SPECIFY}" ~ "Other",
           V161031 == "7. Other specify - specified as: RF" ~ "WNV",
           V161031 == "8. Other specify - specified as: none /no one /NA" ~ "WNV",
           V161031 == "6. Other specify - specified as: DK" ~ "WNV",
           V161031 == "-8. Don't know (FTF only)" ~ "WNV",
           V161031 == "-9. Refused" ~ "WNV",
           TRUE ~ NA_character_),
         Vote = ifelse(is.na(Voted), vi, Voted))

#table(anes_2016$Vote, useNA = "ifany")

## State
# If registration ST is diff to sample ST, take registration ST
anes_2016 <- anes_2016 %>%
  mutate(ST = case_when(
    V161015a == 0 ~ FIPS_to_ST(V161015b), 
    TRUE ~ V161010e))

## Age
anes_2016 <- anes_2016 %>%
  dplyr::filter(V161267 <0 | V161267> 17) %>% # Exclude <18s, but not NAs
  mutate(Age = case_when(
    V161267 > 17 & V161267 < 35 ~ "18-34",
    V161267 > 34 & V161267 < 45 ~ "35-44",
    V161267 > 44 & V161267 < 65 ~ "45-64",
    V161267 > 64 ~ "65+",
    TRUE ~ NA_character_))

#table(anes_2016$Age, anes_2016$V161267, useNA = "ifany")

## Education
anes_2016 <- anes_2016 %>%
  mutate(Education = case_when(
    V161270 %in% 1:8 ~ "No HS diploma",
    V161270 == 9 ~ "HS diploma",
    V161270 == 10 ~ "Some college",
    V161270 > 10 & V161270 < 14 ~ "College grad",
    V161270 > 13 & V161270 < 17 ~ "Postgrad",
    V161270 == 90 ~ "HS diploma",
    TRUE ~ NA_character_))

## Gender
anes_2016 <- anes_2016 %>%
  mutate(Female = case_when(
    V161342 == 1 ~ 0,
    V161342 == 2 ~ 1,
    TRUE ~ NA_real_))

#table(anes_2016$Female, anes_2016$V161342, useNA = "ifany")                     

## Ethnicity
anes_2016 <- anes_2016 %>%
  mutate(Ethnicity = case_when(
    V161310x == 1 ~ "White",
    V161310x == 2 ~ "Black",
    V161310x == 3 ~ "Other",
    V161310x == 4 ~ "Other",
    V161310x == 5 ~ "Hispanic",
    V161310x == 6 ~ "Other",
    TRUE ~ NA_character_))

#table(anes_2016$Race, rio::characterize(anes_2016$V161310x), useNA = "ifany")

## Marital status
anes_2016 <- anes_2016 %>%
  mutate(Marital = case_when(
    V161268 == 1 ~ "Married",
    V161268 == 2 ~ "Married",
    V161268 == 3 ~ "Widowed",
    V161268 == 4 ~ "Sep-Divorced",
    V161268 == 5 ~ "Sep-Divorced",
    V161268 == 6 ~ "Single",
    TRUE ~ NA_character_))

#table(anes_2016$Marital,rio::characterize(anes_2016$V161268), useNA = "ifany")

## Date - at present i've separarted into 8 weeks
anes_2016 <- anes_2016 %>%
  mutate(temp_date = as.Date(V164004, format = "%Y%m%d"), 
         temp_date = as.numeric(abs(temp_date - max(temp_date))),
         Campaign_week = cut(temp_date,
                             breaks = seq(-1,56, by = 7),
                             labels = 1:8),
         Campaign_week = ifelse(is.na(Campaign_week), 8, Campaign_week),
         Campaign_week = paste0("W", Campaign_week))


# Select vars
anes_2016 <- anes_2016 %>%
  dplyr::select(Vote, Area = ST, Ethnicity, Education, Age, Female, Marital, Campaign_week, wt = V160101) %>%
  dplyr::filter(Vote == "Dem"|Vote == "Rep") %>%
  mutate(Dem = ifelse(Vote == "Dem", 1,0))

## Append Past vote
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

anes_2016 <- left_join(anes_2016, Past_vote, by = "Area") %>%
  mutate(past_dem = arm::rescale(past_dem))

## Append region
source("../Code/00_Utility.R")
codes <- data.frame(FIPS=as.numeric(1:100)) %>%
  mutate(Area = FIPS_to_ST(FIPS)) %>%
  drop_na()%>%
  mutate(State = FIPS_to_State(FIPS),
         Region = State_to_Region(State)) %>%
  dplyr::select(Area, Region)
anes_2016 <- left_join(anes_2016, codes, by = "Area")
rm(x,Past_vote, codes)

## save 
dat <- anes_2016 %>%
  drop_na() %>%
  mutate(Year = 2016)

save(dat, file = "../Data/anes_2016_tidy.RData")

################################## End ############################
## Tidy and recode CCES data

# libraries
library(tidyverse)
source("Code/0_Functions.R")

## Read in data
dat <- read_delim("Data/CCES16.tab", 
                  "\t", escape_double = FALSE, trim_ws = TRUE)

## VI recode
## Only use those who are definitely going to vote or have already voted
dat <- dat %>%
  dplyr::filter(dat$CC16_364 == 1 | dat$CC16_364 == 3)

# Recode already voted
dat <- dat %>%
  mutate(Voted = dplyr::case_when(
    CC16_364b == 1 ~ "Rep",
    CC16_364b == 2 ~ "Dem",
    CC16_364b == 3 ~ "Other",
    CC16_364b == 4 ~ "Other",
    CC16_364b == 5 ~ "Other",
    CC16_364b == 6 ~ "DNV",
    CC16_364b == 7 ~ "DNV",
    TRUE ~ NA_character_
  ))
#table(dat$Voted, dat$CC16_364b) ## Check codes align

# Recode vote intention
dat <- dat %>%
  mutate(vi = dplyr::case_when(
    CC16_364c == 1 ~ "Rep",
    CC16_364c == 2 ~ "Dem",
    CC16_364c == 3 ~ "Other",
    CC16_364c == 4 ~ "Other",
    CC16_364c == 5 ~ "Other",
    CC16_364c == 6 ~ "DNV",
    CC16_364c == 7 ~ "DNV",
    TRUE ~ NA_character_
  ))

# Join together already voted with vi
dat <- dat %>%
  mutate(Pres_vi  = ifelse(CC16_364 == 3, Voted, vi))

## Age
dat$Age <- 2016-dat$birthyr ## Convert birth year into age

## Group age into buckets
age_breaks <- c(17, 34, 44, 64, Inf)
age_labs <- c("18-34", "35-44", "45-64", "65+")
dat <- dat %>%
  mutate(Age_orig = Age,
         Age = cut(dat$Age,
                 breaks = age_breaks,
                 labels = age_labs))

#table(dat$Age_orig, dat$Age) ## Check that codes align

## Education
dat <- dat %>%
  mutate(Education = dplyr::case_when(
    educ == 1 ~ "HS or less",
    educ == 2 ~ "HS or less",
    educ == 3 ~ "Some college",
    educ == 4 ~ "College grad",
    educ == 5 ~ "College grad",
    educ == 6 ~ "Postgrad",
    TRUE ~ NA_character_
  ))

#table(dat$Education, dat$educ, useNA = "ifany") ## Check that codes align

## Race
table(dat$race)
dat <- dat %>%
  mutate(Race = dplyr::case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "Hispanic",
    race == 4 ~ "Other",
    race == 5 ~ "Other",
    race == 6 ~ "Other",
    race == 7 ~ "Other",
    race == 8 ~ "Other",
    TRUE ~ NA_character_
  ))

#table(dat$Race, dat$race, useNA = "ifany") ##Check codes align

## Gender
dat <- dat %>%
  mutate(Gender = dplyr::case_when(
    gender == 1 ~ "Male",
    gender == 2 ~ "Female",
    TRUE ~ NA_character_
  ))

#table(dat$Gender, dat$gender) ## Check codes align

## Marital status
dat <- dat %>%
  mutate(Marital = case_when(
    marstat == 1 ~ "Married",
    marstat == 2 ~ "Sep-Divorced",
    marstat == 3 ~ "Sep-Divorced",
    marstat == 4 ~ "Widowed",
    marstat == 5 ~ "Single",
    marstat == 6 ~ "Married"
  ))

#table(dat$marstat, dat$Marital, useNA = "ifany")

## State
dat <- dat %>%
  mutate(State = Number_to_State(inputstate),
         ST = Number_to_ST(inputstate))

#table(dat$State, dat$inputstate) # check codes align  
#table(dat$ST, dat$inputstate) # check codes align  

# ## Congressional district
# dat <- dat %>%
#   mutate(CD = paste(ST, formatC(cdid115, width = 2, flag = '0'), sep = "-"), # Format CF
#          CD = dplyr::recode(CD,
#                             "AK-01" = "AK-AL",
#                             "DE-01" = "DE-AL",
#                             "MT-01" = "MT-AL",
#                             "ND-01" = "ND-AL",
#                             "SD-01" = "SD-AL",
#                             "VT-01" = "VT-AL",
#                             "WY-01" = "WY-AL")) ## Deal with at-large states
# 

## Date
dat$date <- substr(as.character(dat$starttime_pre), 1,10)
dat$date <- as.Date(dat$date)
dat$days_from_elec <- abs(dat$date - max(dat$date+1, na.rm = T))

## Add region
dat$Region <- State_to_Region(dat$State)

## Select required variables and save
dat <- dat %>%
  dplyr::select(Pres_vi, Age, Gender, Education, Race, Marital, State, ST, w = commonweight_vv, Region, days_from_elec) %>%
  drop_na()


# ## Append CD pres. vote - Data download from Daily-Kos
# appends <- read.csv("Data/Daily_Kos_2012.csv")
# dat <- left_join(dat, appends, by = "CD")
# dat <- dat %>%
#   rename(Obama_CD = Obama, Romney_CD = Romney)

save(dat, file = "Data/CCES_recoded.RData")


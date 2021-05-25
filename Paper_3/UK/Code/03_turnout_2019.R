## Script to recode bes face-to-face surveys
## And model turnout

## Set directory
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)
library(arm)
library(brms)
library(reshape2)

## 2015 bes Face-to-face sample
bes_2015 <- rio::import("../Data/Turnout/bes_f2f_2015_v4.0.sav")

## Recode turnout var
bes_2015 <- bes_2015 %>%
  mutate(Vote = case_when(
    validatedTurnoutBinary == 1 ~ 1,
    validatedTurnoutBinary == 0 ~ 0,
    TRUE ~ NA_real_))

## Education
bes_2015 <- bes_2015 %>%
  mutate(ed_temp = rio::characterize(bes_2015$education),
         ed_temp = str_remove_all(ed_temp, "[,']"),
         ed_temp = tolower(ed_temp),
         Education = case_when(
           grepl("no qual", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("level 2 nvq/svq", ed_temp)=="TRUE" ~ "Level 2",
           grepl("level 1 nvq/svq",ed_temp)=="TRUE" ~ "Level 1",
           grepl("clerical",ed_temp)=="TRUE" ~ "Level 1",
           grepl("o level grade a-c", ed_temp)=="TRUE" ~ "Level 2",
           grepl("gcse d-g cse grades 2-5", ed_temp)=="TRUE" ~ "Level 1",
           grepl("dont know", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("a level", ed_temp)=="TRUE" ~ "Level 3",
           grepl("level 4 nvq/svq", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("univ/poly", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("level 3 nvq/svq", ed_temp)=="TRUE" ~ "Level 3",
           grepl("other tech", ed_temp)=="TRUE" ~ "Other",
           grepl("refused", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("recognised trade", ed_temp)=="TRUE" ~ "Level 2",
           grepl("scottish higher", ed_temp)=="TRUE" ~ "Level 3",
           grepl("scottish standard", ed_temp)=="TRUE" ~ "Level 2",
           grepl("nursing", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("teaching", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("degree", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("youth", ed_temp)=="TRUE" ~ "Level 2",
           TRUE ~ ed_temp))

## Age
bes_2015 <- bes_2015 %>%
  mutate(Age_old = Age,
         Age = case_when(
           Age > 15 & Age < 20 ~ "16-19",
           Age > 19 & Age < 25 ~ "20-24",
           Age > 24 & Age < 30 ~ "25-29",
           Age > 29 & Age < 45 ~ "30-44",
           Age > 44 & Age < 60 ~ "45-59",
           Age > 59 & Age < 65 ~ "60-64",
           Age > 64 & Age < 75 ~ "65-74",
           Age > 74 ~ "75+",
           TRUE ~ NA_character_))

## Gender
bes_2015 <- bes_2015 %>%
  mutate(Female = case_when(
    y09 == 1  ~ 0,
    y09 == 2 ~ 1,
    TRUE ~ NA_real_))

## PCON_ID
bes_2015 <- bes_2015 %>%
  mutate(PCON_ID = Constit_Code)
pcon_lookup <- read.csv("../Data/pcon_lookup.csv")
bes_2015 <- left_join(bes_2015, pcon_lookup, by = "PCON_ID")

## Select needed vars
bes_2015 <- bes_2015 %>%
  dplyr::select(PCON_ID, pcon, Age, Education, Female, Vote) %>%
  mutate(Year = 2015)

## 2017 bes Face-to-face sample
bes_2017 <- rio::import("../Data/Turnout/bes_f2f_2017_v1.5.sav")

## Recode turnout var
bes_2017 <- bes_2017 %>%
  mutate(Vote = case_when(
    validatedTurnoutBinary == 1 ~ 1,
    validatedTurnoutBinary == 0 ~ 0,
    TRUE ~ NA_real_))

## Education
bes_2017 <- bes_2017 %>%
  mutate(ed_temp = rio::characterize(bes_2017$education),
         ed_temp = str_remove_all(ed_temp, "[,']"),
         ed_temp = tolower(ed_temp),
         Education = case_when(
           grepl("no qual", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("level 2 nvq/svq", ed_temp)=="TRUE" ~ "Level 2",
           grepl("level 1 nvq/svq",ed_temp)=="TRUE" ~ "Level 1",
           grepl("clerical",ed_temp)=="TRUE" ~ "Level 1",
           grepl("o level grade a-c", ed_temp)=="TRUE" ~ "Level 2",
           grepl("gcse d-g cse grades 2-5", ed_temp)=="TRUE" ~ "Level 1",
           grepl("dont know", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("a level", ed_temp)=="TRUE" ~ "Level 3",
           grepl("level 4 nvq/svq", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("univ/poly", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("level 3 nvq/svq", ed_temp)=="TRUE" ~ "Level 3",
           grepl("other tech", ed_temp)=="TRUE" ~ "Other",
           grepl("refused", ed_temp)=="TRUE" ~ "No qualifications",
           grepl("recognised trade", ed_temp)=="TRUE" ~ "Level 2",
           grepl("scottish higher", ed_temp)=="TRUE" ~ "Level 3",
           grepl("scottish standard", ed_temp)=="TRUE" ~ "Level 2",
           grepl("nursing", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("teaching", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("degree", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("youth", ed_temp)=="TRUE" ~ "Level 2",
           grepl("accountancy", ed_temp)=="TRUE" ~ "Level 4/5",
           grepl("hgv", ed_temp)=="TRUE" ~ "Other",
           TRUE ~ ed_temp))

## Age
bes_2017 <- bes_2017 %>%
  mutate(Age_old = Age,
         Age = case_when(
           Age > 15 & Age < 20 ~ "16-19",
           Age > 19 & Age < 25 ~ "20-24",
           Age > 24 & Age < 30 ~ "25-29",
           Age > 29 & Age < 45 ~ "30-44",
           Age > 44 & Age < 60 ~ "45-59",
           Age > 59 & Age < 65 ~ "60-64",
           Age > 64 & Age < 75 ~ "65-74",
           Age > 74 ~ "75+",
           TRUE ~ NA_character_))

## Gender
bes_2017 <- bes_2017 %>%
  mutate(Female = case_when(
    y09 == 1  ~ 0,
    y09 == 2 ~ 1,
    TRUE ~ NA_real_))

## PCON_ID
bes_2017 <- bes_2017 %>%
  mutate(PCON_ID = Constit_Code)
bes_2017 <- left_join(bes_2017, pcon_lookup, by = "PCON_ID")

## Select needed vars
bes_2017 <- bes_2017 %>%
  dplyr::select(PCON_ID, pcon, Age, Education, Female, Vote) %>%
  mutate(Year = 2017)

## Bind together two frames and save
dat <- rbind(bes_2015, bes_2017)
dat <- dat  %>%
  drop_na()

## Append area_vars
area_vars <- read.csv("../Data/Turnout/Turnout_Area_vars.csv") %>%
  dplyr::select(PCON_ID = ONSConstID, Turnout17, Majority17, Region_ID = Region) %>%
  mutate(Turnout17 = arm::rescale(Turnout17), 
         Majority17 = arm::rescale(Majority17))

dat <- left_join(dat, area_vars, by = "PCON_ID")

# Save
save(dat, file = "../Data/Turnout/bes_turnout_19.RData")
rm(bes_2015, bes_2017, pcon_lookup)

## Model
mod <- brm(Vote ~ (1|Region_ID) + (1|Age) + (1|Education) +
             Female + Turnout17 + Majority17,
           data  = dat, family = bernoulli(link = "logit"),
           prior = c(set_prior("normal(0,2.5)", class = "b"),
                     set_prior("normal(0,2.5)", class = "Intercept"),
                     set_prior("normal(0,2.5)", class = "sd")),
           chains = 2, iter = 500,
           seed = 12345)
saveRDS(mod, "../Results/Turnout/UK_turnout19_mod.rds")
mod <- readRDS("../Results/Turnout/UK_turnout19_mod.rds")

## Poststrat
# Load and amend to exclude unnecessary vars and recalculate weights
post <- read.csv("../Data/Post.csv") %>%
  rename(PCON_ID = GSSCode, Age = age0,Education = education) %>%
  mutate(Female = ifelse(sex == "Female", 1, 0)) %>%
  group_by(PCON_ID, Female, Age, Education) %>%
  summarise(weight = sum(weight)) %>% 
  ungroup()

post <- left_join(post, area_vars, by = "PCON_ID")

## Actual poststratifcation
out<- brms::posterior_epred(mod,
                            newdata=post,
                            draws = 500)

out.m <- melt(out, varnames = c("iter", "post_row"))
post_row_turnout <- out.m %>%
  group_by(post_row) %>%
  summarise(Turnout = median(value))

save(post_row_turnout, file = "../Results/Turnout/final_turnout19.RData")

########################## End #########################

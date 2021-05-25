## Script to recode bes face-to-face surveys
## And model turnout

## Set directory
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)
library(arm)
library(brms)
library(reshape2)


## Load 2010 face-to-face bes data
bes_2010 <- rio::import("../Data/Turnout/2010BESPreandPost.sav")

## Recode turnout, NA for non-validated vote
bes_2010 <- bes_2010 %>%
  mutate(Vote = case_when(
           OUTCOMEW == 1 ~ 1,
           OUTCOMEW == 2 ~ 0,
           TRUE ~ NA_real_))

## Edcation
bes_2010 <- bes_2010 %>%
  mutate(ed_temp1 = rio::characterize(bes_2010$ZQ95_2),
         ed_temp2 = rio::characterize(bes_2010$ZQ95_3),
         ed_temp2 = str_remove_all(ed_temp2, "[,']"),
         ed_temp2 = tolower(ed_temp2),
    Education = case_when(
      grepl("No", ed_temp1)=="TRUE" ~ "No qualifications",
      grepl("level 2 nvq/svq", ed_temp2)=="TRUE" ~ "Level 2",
      grepl("level 1 nvq/svq",ed_temp2)=="TRUE" ~ "Level 1",
      grepl("clerical",ed_temp2)=="TRUE" ~ "Level 1",
      grepl("o level grade a-c", ed_temp2)=="TRUE" ~ "Level 2",
      grepl("gcse d-g cse grades 2-5", ed_temp2)=="TRUE" ~ "Level 1",
      grepl("dont know", ed_temp2)=="TRUE" ~ "No qualifications",
      grepl("a level", ed_temp2)=="TRUE" ~ "Level 3",
      grepl("level 4 nvq/svq", ed_temp2)=="TRUE" ~ "Level 4/5",
      grepl("univ/poly", ed_temp2)=="TRUE" ~ "Level 4/5",
      grepl("level 3 nvq/svq", ed_temp2)=="TRUE" ~ "Level 3",
      grepl("other tech", ed_temp2)=="TRUE" ~ "Other",
      grepl("refused", ed_temp2)=="TRUE" ~ "No qualifications",
      grepl("recognised trade", ed_temp2)=="TRUE" ~ "Level 2",
      grepl("scottish higher", ed_temp2)=="TRUE" ~ "Level 3",
      grepl("scottish standard", ed_temp2)=="TRUE" ~ "Level 2",
      grepl("nursing", ed_temp2)=="TRUE" ~ "Level 4/5",
      grepl("teaching", ed_temp2)=="TRUE" ~ "Level 4/5",
      grepl("degree", ed_temp2)=="TRUE" ~ "Level 4/5",
      grepl("youth", ed_temp2)=="TRUE" ~ "Level 2",
      TRUE ~ ed_temp2))

## Gender
bes_2010 <- bes_2010 %>%
  mutate(Female = ifelse(rio::characterize(bes_2010$ZQ88)=="Female", 1, 0))

## Age
bes_2010 <- bes_2010 %>%
  mutate(Age = case_when(
    ZQ89 > 15 & ZQ89 < 20 ~ "16-19",
    ZQ89 > 19 & ZQ89 < 25 ~ "20-24",
    ZQ89 > 24 & ZQ89 < 30 ~ "25-29",
    ZQ89 > 29 & ZQ89 < 45 ~ "30-44",
    ZQ89 > 44 & ZQ89 < 60 ~ "45-59",
    ZQ89 > 59 & ZQ89 < 65 ~ "60-64",
    ZQ89 > 64 & ZQ89 < 75 ~ "65-74",
    ZQ89 > 74 ~ "75+",
    TRUE ~ NA_character_
  ))

## PCON
bes_2010 <- bes_2010 %>%
  mutate(pcon = str_remove_all(TCONSTIT, "[\"]"),
         pcon = str_remove(pcon, " Co Const"),
         pcon = str_remove(pcon, " Boro Const"),
         pcon = str_remove(pcon, "Const"),
         pcon = str_remove(pcon, " Burgh "),
         pcon = str_remove(pcon, " Co Cons"),
         pcon = str_replace(pcon, "St. ", "St "))

pcon_lookup <- read.csv("../Data/pcon_lookup.csv")
bes_2010 <- left_join(bes_2010, pcon_lookup, by = "pcon")

## Select needed variables
bes_2010 <- bes_2010 %>%
  dplyr::select(PCON_ID, Region_ID, pcon, Age, Education, Female, Vote) %>%
  mutate(Year = 2010)

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
bes_2015 <- left_join(bes_2015, pcon_lookup, by = "PCON_ID")

## Select needed vars
bes_2015 <- bes_2015 %>%
  dplyr::select(PCON_ID, Region_ID, pcon, Age, Education, Female, Vote) %>%
  mutate(Year = 2015)

## Bind together two frames and save
dat <- rbind(bes_2010, bes_2015)
dat <- dat  %>%
  drop_na()

## Append area_vars
area_vars <- read.csv("../Data/Turnout/Turnout_Area_vars.csv") %>%
  dplyr::select(PCON_ID = ONSConstID, Turnout15, Majority15, Region_ID = Region) %>%
  mutate(Turnout15 = arm::rescale(Turnout15), 
         Majority15 = arm::rescale(Majority15))
dat <- left_join(dat, area_vars, by = "PCON_ID")

# Save
save(dat, file = "../Data/Turnout/bes_turnout_17.RData")
rm(bes_2010, bes_2015, pcon_lookup)

## Model
mod <- brm(Vote ~ (1|Region_ID) + (1|Age) + (1|Education) +
             Female + Turnout15 + Majority15,
           data  = dat, family = bernoulli(link = "logit"),
           prior = c(set_prior("normal(0,2.5)", class = "b"),
                     set_prior("normal(0,2.5)", class = "Intercept"),
                     set_prior("normal(0,2.5)", class = "sd")),
           chains = 2, iter = 500,
           seed = 12345)
saveRDS(mod, "../Results/Turnout/UK_turnout17_mod.rds")
mod <- readRDS("../Results/Turnout/UK_turnout17_mod.rds")

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

save(post_row_turnout, file = "../Results/Turnout/final_turnout17.RData")

########################## End #########################

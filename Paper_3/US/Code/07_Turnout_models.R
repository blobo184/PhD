## Script to run simple Multilevel models for turnout

## Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Libraries
library(rstan)
library(brms)
library(tidyverse)
library(reshape2)

## Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


## First model for 2008
load("../Data/Turnout/cps08.RData")
mod <- brm(Vote ~ 1 + (1| Area) + (1| Age) + (1| Education) + (1|Ethnicity) +
                    (1|Marital) + Female + Majority08 + Turnout08,
                  data  = cps08, 
                  family = bernoulli(link = "logit"),
                  prior = c(set_prior("normal(0,2.5)", class = "b"),
                            set_prior("normal(0,2.5)", class = "Intercept"),
                            set_prior("student_t(5,0,2.5)", class = "sd")),
                  chains = 2, iter = 1000,
                  seed = 12345)
#saveRDS(mod, file = "../Results/Turnout/turnout_12.rds")
mod <- readRDS("../Results/Turnout/turnout_12.rds")

## Load and amend poststrat
load("~/Paper 3/Research/US/Data/post_2012.RData")
post_2012 <- post_2012 %>%
  mutate(Female = ifelse(Gender == "Female", 1,0),
         Ethnicity = Race,
         Area = ST,
         #Education = str_replace_all(Education, " ", "."),
         Education = recode(Education,
                            "No HS diploma" = " No HS diploma")) %>%
  dplyr::select(-Race, -Gender, -ST)

## Append area vars
load("../Data/Turnout/cps08.RData")
area_vars <- cps08 %>%
  group_by(Area) %>%
  summarise(Majority08=unique(Majority08),
            Turnout08=unique(Turnout08))

post_2012 <- left_join(post_2012, area_vars, by = "Area")
rm(area_vars, cps08)
## Poststrat to poststrat rows
out<- brms::posterior_epred(mod,
                            newdata=post_2012,
                            draws = 500)

out.m <- melt(out, varnames = c("iter", "post_row"))
post_row_turnout <- out.m %>%
  group_by(post_row) %>%
  summarise(Turnout = median(value))
save(post_row_turnout, file="../Results/Turnout/US_turnout12.RData")

## First model for 2012
load("../Data/cps12.RData")
mod <- brm(Vote ~ 1 + (1| Area) +  (1| Age) + (1| Education) + (1|Ethnicity) +
             (1|Marital) + + Female + Majority12 + Turnout12,
           data  = cps12, 
           family = bernoulli(link = "logit"),
           prior = c(set_prior("normal(0,2.5)", class = "b"),
                     set_prior("normal(0,2.5)", class = "Intercept"),
                     set_prior("student_t(5,0,2.5)", class = "sd")),
           chains = 2, iter = 1000,
           seed = 12345)
saveRDS(mod, file = "../Results/turnout_16.rds")
mod <- readRDS("../Results/Turnout/turnout_16.rds")

## Load and amend poststrat
load("~/Paper 3/Research/US/Data/post_2016.RData")
post_2016 <- post_2016 %>%
  mutate(Female = ifelse(Gender == "Female", 1,0),
         Ethnicity = Race,
         Area = ST,
         #Education = str_replace_all(Education, " ", "."),
         Education = recode(Education,
                            "No HS diploma" = " No HS diploma")) %>%
  dplyr::select(-Race, -Gender, -ST)

## Append area vars
load("../Data/Turnout/cps12.RData")
area_vars <- cps12 %>%
  group_by(Area) %>%
  summarise(Majority12=unique(Majority12),
            Turnout12=unique(Turnout12))

post_2016 <- left_join(post_2016, area_vars, by = "Area")
rm(area_vars, cps12)
## Poststrat to poststrat rows
out<- brms::posterior_epred(mod,
                            newdata=post_2016,
                            draws = 500)

out.m <- melt(out, varnames = c("iter", "post_row"))
post_row_turnout <- out.m %>%
  group_by(post_row) %>%
  summarise(Turnout = median(value))
save(post_row_turnout, file="../Results/Turnout/US_turnout16.RData")

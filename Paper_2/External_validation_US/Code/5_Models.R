## Models
library(brms)
library(tidyverse)
library(arm)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## Even model
# load data
load("Data/sample_Even.RData")
dat <- sample_Even %>%
  mutate(Rep_vi = ifelse(Pres_vi == "Rep", 1, 0))
rm(sample_Even)

Model_Even <- brm(Rep_vi ~ (1| ST) +  (1| Region) + 
                    (1|Age) + (1| Education) + (1|Race) +
                    (1|Marital) + (1|campaign_week) +
                    (1|Gender) + Rep_2012,
                  data = dat,
                  family = bernoulli(link = "logit"),
                  prior = c(set_prior("student_t(5, 0, 5)", class = "b"),
                            set_prior("student_t(5, 0, 5)", class = "Intercept"),
                            set_prior("student_t(5, 0, 5)", class = "sd")),
                  chains = 2, iter = 1000,
                  seed = 12345, 
                  control = list(adapt_delta = 0.95,
                                 max_treedepth = 12))
saveRDS(Model_Even, file = "Results/Model_Even.RDS")
rm(Model_Even,dat)

## Two group model
# load data
load("Data/sample_Two.RData")
dat <- sample_Two %>%
  mutate(Rep_vi = ifelse(Pres_vi == "Rep", 1, 0))
rm(sample_Two)

Model_Two <- brm(Rep_vi ~ (1| ST) +  (1| Region) + 
                   (1|Age) + (1| Education) + (1|Race) +
                   (1|Marital) + (1|campaign_week) +
                   (1|Gender) + Rep_2012,
                 data = dat,
                 family = bernoulli(link = "logit"),
                 prior = c(set_prior("student_t(5, 0, 5)", class = "b"),
                           set_prior("student_t(5, 0, 5)", class = "Intercept"),
                           set_prior("student_t(5, 0, 5)", class = "sd")),
                 chains = 2, iter = 1000,
                 seed = 12345, 
                 control = list(adapt_delta = 0.95,
                                max_treedepth = 12))
saveRDS(Model_Two, file = "Results/Model_Two.RDS")
rm(Model_Two, dat)

## Three group model
# load data
load("Data/sample_Three.RData")
dat <- sample_Three %>%
  mutate(Rep_vi = ifelse(Pres_vi == "Rep", 1, 0))
rm(sample_Three)

Model_Three <- brm(Rep_vi ~ (1| ST) +  (1| Region) + 
                     (1|Age) + (1| Education) + (1|Race) +
                     (1|Marital) + (1|campaign_week) +
                     (1|Gender) + Rep_2012,
                   data = dat,
                   family = bernoulli(link = "logit"),
                   prior = c(set_prior("student_t(5, 0, 5)", class = "b"),
                             set_prior("student_t(5, 0, 5)", class = "Intercept"),
                             set_prior("student_t(5, 0, 5)", class = "sd")),
                   chains = 2, iter = 1000,
                   seed = 12345, 
                   control = list(adapt_delta = 0.95,
                                  max_treedepth = 12))

saveRDS(Model_Three, file = "Results/Model_Three.RDS")
rm(Model_Three, dat)
####################### End #################
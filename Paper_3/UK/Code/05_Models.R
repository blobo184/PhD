## Script to run test models

## Set working directory as UK folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Libraries
library(rstan)
library(brms)
library(tidyverse)
library(tidybayes)

## Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## Values for text
# lambda <- 1
# sample <- 10
# year <- 2017

## I've put all the models in a function
## I'll then run the different variations by using for loops
## for different sample, year and lambda values.
run_mods <- function(sample, year, lambda) {
  
  ## Select relevant sample folder to load data and save mods.
  sample <- if (sample == 5){
      "Sample_5/"
    } else if (sample == 10){
        "Sample_10/"
      } else if (sample == 20){
          "Sample_20/"
        } else if (sample == 30){
            "Sample_30/"
          } else {
              stop("Incorrect sample input, need a sample size of either 5, 10, 20 or 30")}
  
  ## Select which year, to load correct data and save mod year correctly.
  if (year == 2017){
    historic_mod <- "mod_2015.rds"
    current_mod <- "2017"
    historic_dat <- "bes_2015.RData"
    current_dat <- "bes_2017.RData"
  } else if (year == 2019){
    historic_mod <- "mod_2017.rds"
    current_mod <- "2019"
    historic_dat <- "bes_2017.RData"
    current_dat <- "bes_2019.RData"
  } else {
    stop("Incorrect year input, must be either 2017 or 2019")
  }
  
  ## For informative ranef effect mods, mod names either a,b,c
  inf_mod <- if (lambda == 1){
     "_inf_a.rds"} else if (lambda == 1.5) {
       "_inf_b.rds"} else if (lambda == 2) {
         "_inf_c.rds"} else {
           stop("Lambda values need to be either 1, 1.5 or 2")
         }
  
  ## For informative fixef effect mods, mod names either a,b,c
  fe_mod <- if (lambda == 1){
    "_fe_a.rds"} else if (lambda == 1.5) {
      "_fe_b.rds"} else if (lambda == 2) {
        "_fe_c.rds"} else {
          stop("Lambda values need to be either 1, 1.5 or 2")
        }
  ################################ Models ###################
  ## Historic data model, predicting Dem vote %
  if(file.exists(paste0("../Results/", sample, historic_mod))==FALSE){
    
    load(paste0("../Data/", sample, historic_dat))
    
    if(year == 2017){
    mod <- brm(Lab ~ 1 + (1|Area) + (1|Region) + 
                 (1|Age) + (1| Education) + 
                 Female + Past_Lab + Long_term_unemployed + 
                 Population_density + Industry_manufacturing,
               data  = dat, 
               family = bernoulli(link = "logit"),
               prior = c(set_prior("normal(0,5)", class = "b"),
                       set_prior("normal(0,5)", class = "Intercept"),
                       set_prior("student_t(5,0,5)", class = "sd")),
               control = list(adapt_delta = 0.95),
               chains = chain, iter = iters,
               seed = 12345)
    } else if (year == 2019){
    mod <- brm(Lab ~ 1 + (1|Area) + (1|Region) + 
                   (1|Age) + (1| Education) + 
                   Female + Past_Lab + Long_term_unemployed + 
                   Population_density + Industry_manufacturing +
                   leavehanretty,
               data  = dat, 
               family = bernoulli(link = "logit"),
               prior = c(set_prior("normal(0,5)", class = "b"),
                           set_prior("normal(0,5)", class = "Intercept"),
                           set_prior("student_t(5,0,5)", class = "sd")),
               control = list(adapt_delta = 0.95),
               chains = chain, iter = iters,
               seed = 12345)
    }
    
  saveRDS(mod, file = paste0("../Results/", sample, historic_mod))
  rm(mod, dat)
  }
  
  ## Current model, predicting Dem vote %, weakly inf priors
  if(file.exists(paste0("../Results/", sample, "mod_", current_mod, "_weak.rds"))==FALSE){
    
    # Load data
    load(paste0("../Data/", sample, current_dat))
    
    # Model
    start_time <- Sys.time()
    mod <- brm(Lab ~ 1 + (1|Area) + (1|Region) + 
                 (1|Age) + (1| Education) + 
                 Female + Past_Lab + Long_term_unemployed + 
                 Population_density + Industry_manufacturing +
                 leavehanretty,
               data  = dat, 
               family = bernoulli(link = "logit"),
               prior = c(set_prior("normal(0,5)", class = "b"),
                         set_prior("normal(0,5)", class = "Intercept"),
                         set_prior("student_t(5,0,5)", class = "sd")),
               control = list(adapt_delta = 0.95),
               chains = chain, iter = iters,
               seed = 12345)
    
    end_time <- Sys.time()
    
    # Save model run time
    write.table(cbind(year, sample, "Weak", difftime(end_time, start_time, units = "mins")), 
                file = "../Results/Model_times.csv", sep = ",", row.names = F, append = T, col.names = F)
    
    # Save model
    saveRDS(mod, file = paste0("../Results/", sample, "mod_", current_mod, "_weak.rds"))
    rm(mod, dat)
  }
  
  ## Current model, predicting Dem vote %, Inf priors, lambda = 1
  if(file.exists(paste0("../Results/", sample, "mod_", current_mod, inf_mod))==FALSE){
    
    # First load historic mod and get priors
    source("00_Utility.R")
    mod <- readRDS(paste0("../Results/", sample, historic_mod))
    mod_priors <- get_inf_priors(lambda = lambda, mod = mod)
    rm(mod)
    
    ## Load data
    load(paste0("../Data/", sample, current_dat))
    
    # Model
    start_time <- Sys.time()
    mod <- brm(Lab ~ 1 + (1|Area) + (1|Region) + 
                 (1|Age) + (1| Education) + 
                 Female + Past_Lab + Long_term_unemployed + 
                 Population_density + Industry_manufacturing +
                 leavehanretty,
               data  = dat, 
               family = bernoulli(link = "logit"),
               prior = mod_priors,
               control = list(adapt_delta = 0.95),
               chains = chain, iter = iters,
               seed = 12345)
    
    end_time <- Sys.time()
    
    # Save model run time
    write.table(cbind(year, sample, inf_mod, difftime(end_time, start_time, units = "mins")), 
                file = "../Results/Model_times.csv", sep = ",", row.names = F, append = T, col.names = F)
    
    # Save model
    saveRDS(mod, file = paste0("../Results/", sample, "mod_", current_mod, inf_mod))
    rm(mod, dat)
  }
  
  ## Fixed effects model
  if(file.exists(paste0("../Results/", sample, "mod_", current_mod, fe_mod))==FALSE){
    
    ## First load historic mod and get priors
    source("00_Utility.R")
    mod <- readRDS(paste0("../Results/", sample, historic_mod))
    mod_priors <- get_inf_priors_FE(lambda = lambda, mod = mod)
    rm(mod)
    
    ## Load data
    load(paste0("../Data/", sample, current_dat))
    
    # Model
    start_time <- Sys.time()
    mod <- brm(Lab ~ 1 + (1|Area) + Region + 
                 Age + Education + 
                 Female + Past_Lab + Long_term_unemployed + 
                 Population_density + Industry_manufacturing +
                 leavehanretty,
               data  = dat, 
               family = bernoulli(link = "logit"),
               prior = mod_priors,
               control = list(adapt_delta = 0.95),
               chains = chain, iter = iters,
               seed = 12345)
    end_time <- Sys.time()
    
    # Save model run time
    write.table(cbind(year, sample, fe_mod, difftime(end_time, start_time, units = "mins")), 
                file = "../Results/Model_times.csv", sep = ",", row.names = F, append = T, col.names = F)
    
    # Save model
    saveRDS(mod, file = paste0("../Results/", sample, "mod_", current_mod, fe_mod))
    rm(mod, dat)
  }
}

# ## Specifications
# dir.create("../Results/Sample_5/")
# dir.create("../Results/Sample_10/")
# dir.create("../Results/Sample_20/")
# dir.create("../Results/Sample_30/")

## Create csv for model times
write.table(cbind(Year="Year", sample="Sample",mod="Mod", Time="Time"),
            file = "../Results/Model_times.csv",
            sep = ",", row.names = F, col.names = F)


chain <- 1
iters <- 20
lambda <- c(1, 1.5, 2)
sample <- c(5, 10, 20, 30)
year <- c(2017, 2019)
for (s in 1:length(sample)) {
  for (y in 1:length(year)) {
    for (l in 1:length(lambda)) {
      run_mods(lambda = lambda[l],
                 sample = sample[s],
                 year = year[y]
      )
    }
  }
}

######################### End #########################
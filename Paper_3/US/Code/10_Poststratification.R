## Script to poststratify all US models

## Set directory
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(reshape2)
library(tidyverse)
library(brms)
source("00_Utility.R")

## Stan options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


## Poststratifaction of all models
## Get list of files
files_5 <- list.files("../Results/Sample_5/", full.names = T)
files_10 <- list.files("../Results/Sample_10/", full.names = T)
files_20 <- list.files("../Results/Sample_20/", full.names = T)
files_30 <- list.files("../Results/Sample_30/", full.names = T)

## Exclude base models and back-up files
files_5 <- files_5[grepl(c("inf|fe|weak"), files_5)==TRUE]
files_10 <- files_10[grepl(c("inf|fe|weak"), files_10)==TRUE]
files_20 <- files_20[grepl(c("inf|fe|weak"), files_20)==TRUE]
files_30 <- files_30[grepl(c("inf|fe|weak"), files_30)==TRUE]
files_5 <- files_5[grepl(c(".bak"), files_5)==FALSE]
files_10 <- files_10[grepl(c(".bak"), files_10)==FALSE]
files_20 <- files_20[grepl(c(".bak"), files_20)==FALSE]
files_30 <- files_30[grepl(c(".bak"), files_30)==FALSE]


## Load posststrat frames
load("../Data/post_2012_final.RData")
load("../Data/post_2016_final.RData")

poststrat_all_models <- function(paths, iters){
  
  holder <- list()
  for (i in 1:length(paths)) {
    # Load model
    mod <- readRDS(paths[i])
    
    ## Get model name
    mod_name <- str_split(paths[i], "mod_")[[1]][2]
    mod_name <- str_remove_all(mod_name, c(".rds|2012_|2016_"))
    
    ## Poststrat using either 2012 or 2016 frame  
    if (grepl("2012",paths[i])==TRUE){
      holder[[i]] <- post_strat(mod=mod, post = post_2012, iters = iters)
      holder[[i]]$Year <- "2012"
      holder[[i]]$Model <- mod_name
    } else {
      holder[[i]] <- post_strat(mod=mod, post = post_2016, iters = iters)
      holder[[i]]$Year <- "2016"
      holder[[i]]$Model <- mod_name
    } 
  }
  results <- do.call("rbind", holder)
  results 
}

results_sample_5 <- poststrat_all_models(paths = files_5, iters = 500)
results_sample_10 <- poststrat_all_models(paths = files_10, iters = 500)
results_sample_20 <- poststrat_all_models(paths = files_20, iters = 500)
results_sample_30 <- poststrat_all_models(paths = files_30, iters = 500)

save(results_sample_5, results_sample_10, results_sample_20, results_sample_30, file = "../Results/Results.RData")
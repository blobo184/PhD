## Script to get model run times

## Set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Libraries
library(brms)
library(tidyverse)


# Function loads model and gets model run time
# As data frame for warmup, sample and total
get_run_times <- function(file_location) {
  
  # Load libraries
  library(brms)
  library(rstan)
  
  # Get path
  path <- file_location
  
  # Load model
  mod <- readRDS(path)
  
  # Extraxt model run time
  time <- get_elapsed_time(mod$fit)
  
  # Reshape and recode times
  time <- as.data.frame(time)
  time$Total <- rowSums(time)
  time$Chain <- rownames(time)
  rownames(time) <- NULL
  time$Model <- strsplit(path, "/")[[1]][11]
  
  time
}

## Function to extract times for each model sample directories
# Create holding empty data frame
extract_times <- function(paths){
  holder <- data.frame(warmup = numeric(),
                       sample = numeric(),
                       Total = numeric(),
                       Chains = character(),
                       Model = character())
  
  paths <- list.files(paths, full.names = T)
  
  for (i in 1:length(paths)) {
    time <- get_run_times(paths[i])
    holder <- rbind(holder, time)
  }
  
  holder <- holder %>%
    group_by(Model) %>%
    filter(Total != min(Total))
 holder 
}

## Finally extract for each sample directory
sample_5 <- extract_times(paths = "C://Users/benlo/Dropbox (Royal Holloway)/Paper 3/Research/US/Results/Sample_5/") %>%
  mutate(Sample = 5)
sample_10 <- extract_times(paths = "C://Users/benlo/Dropbox (Royal Holloway)/Paper 3/Research/US/Results/Sample_10/") %>%
  mutate(Sample = 10)
sample_20 <- extract_times(paths = "C://Users/benlo/Dropbox (Royal Holloway)/Paper 3/Research/US/Results/Sample_20/") %>%
  mutate(Sample = 20)
sample_30 <- extract_times(paths = "C://Users/benlo/Dropbox (Royal Holloway)/Paper 3/Research/US/Results/Sample_30/") %>%
  mutate(Sample = 30)

## Combine frames and save
US_model_times <- do.call("rbind", list(sample_5, sample_10, sample_20, sample_30)) 
save(US_model_times, file = "../Results/US_model_times.RData")

################################## End #####################################

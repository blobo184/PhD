## Script to sample from anes for necessary sample sizes

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(rio)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
source("00_Utility.R")

## Load, drop nas and add year identifier.
load("../Data/anes_2008_tidy.RData")
total_sample_2008 <- dat

load("../Data/anes_2012_tidy.RData")
total_sample_2012 <- dat 

load("../Data/anes_2016_tidy.RData")
total_sample_2016 <- dat

## Create folders for diff sample sizes.
dir.create("../Data/Sample_5/")
dir.create("../Data/Sample_10/")
dir.create("../Data/Sample_20/")
dir.create("../Data/Sample_30/")

## Sample respondents from anes surveys
## The below function samples from anes surveys and saves in sample size folders.
## You need to enter N, average number of respondents per small area.
sample_anes <- function(N) {
  
  path <- if (N == 5){
    "../Data/Sample_5/"
  } else if (N == 10){
    "../Data/Sample_10/"
  } else if (N == 20){
    "../Data/Sample_20/"
    } else {"../Data/Sample_30/"}

  sample_size <- (N * 51)
  path_08 <- paste0(path, "anes_2008.RData")
  path_12 <- paste0(path, "anes_2012.RData")
  path_16 <- paste0(path, "anes_2016.RData")
  
  ## 2008
  # Calculate sample size for each area based on relative proportion of full sample size.
  # Min sample for each area is 1
  dat <- total_sample_2008 %>%
    group_by(Area) %>%
    mutate(num_rows = n()) %>%
    ungroup() %>%
    mutate(perc = num_rows/nrow(total_sample_2008), # % of sample distribution among areas
           Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
           Area_sample = round(Area_sample,0),
           wt_new = arm::rescale(wt))
  
  # Sample from each Area
  dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
    newdat <- subset(dat, Area == Area_code)
    N <- unique(newdat$Area_sample)
    set.seed(20042021)
    dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  }
  dat <- do.call("rbind", dat_sample)
  save(dat, file = path_08)

  ## 2012
  # Calculate sample size for each area based on relative proportion of full sample size.
  # Min sample for each area is 1
  dat <- total_sample_2012 %>%
    group_by(Area) %>%
    mutate(num_rows = n()) %>%
    ungroup() %>%
    mutate(perc = num_rows/nrow(total_sample_2012), # % of sample distribution among areas
           Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
           Area_sample = round(Area_sample,0))
  
  # Sample from each Area
  dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
    newdat <- subset(dat, Area == Area_code)
    N <- unique(newdat$Area_sample)
    set.seed(20042021)
    dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  }
  dat <- do.call("rbind", dat_sample)
  save(dat, file = path_12)
  
  ## 2016
  # Calculate sample size for each area based on relative proportion of full sample size.
  # Min sample for each area is 1
  dat <- total_sample_2016 %>%
    group_by(Area) %>%
    mutate(num_rows = n()) %>%
    ungroup() %>%
    mutate(perc = num_rows/nrow(total_sample_2016), # % of sample distribution among areas
           Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
           Area_sample = round(Area_sample,0))
  
  # Sample from each Area
  dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
    newdat <- subset(dat, Area == Area_code)
    N <- unique(newdat$Area_sample)
    set.seed(20042021)
    dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  }
  dat <- do.call("rbind", dat_sample)
  save(dat, file = path_16)
}

## Sample from anes
sample_anes(N = 5)
sample_anes(N = 10)
sample_anes(N = 20)
sample_anes(N = 30)

## quick check to see if sampling has worked correctly
# Function loads each dataframe a sample folder and prints year and N of frame
check_func <- function(path){
  check <- list.files(path, full.names = T)
  for (i in 1:length(check)){
    load(check[i])
    print(paste0("Year: ",(unique(dat$Year)), 
                 "| N: ", (nrow(dat)),
                 "| N areas: ", length(unique(dat$Area)),
                 "| N Campaign: ", length(unique(dat$Campaign_week)),
                 "| N Edu: ", length(unique(dat$Education)),
                 "| N Race: ", length(unique(dat$Ethnicity)),
                 "| N Age: ", length(unique(dat$Age)),
                 "| N Marital: ", length(unique(dat$Marital)),
                 "| N Region: ", length(unique(dat$Region))))
    
  }
}
check_func("../Data/Sample_5/")  
check_func("../Data/Sample_10/") 
check_func("../Data/Sample_20/") 
check_func("../Data/Sample_30/") 

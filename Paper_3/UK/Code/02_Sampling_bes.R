## Sampling from the total bes frames
## This script samples from total bes to get necessary sample sizes

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(rio)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)


## Create folders for diff sample sizes.
dir.create("../Data/Sample_5/")
dir.create("../Data/Sample_10/")
dir.create("../Data/Sample_20/")
dir.create("../Data/Sample_30/")

# Seed number: 7638884
runif(1, 0, 10000000)

## Sample respondents from bes surveys
## The below function samples from bes surveys and saves in sample size folders.
## You need to enter N, average number of respondents per small area.
sample_bes <- function(N) {
  
  path <- if (N == 5){
    "../Data/Sample_5/"
  } else if (N == 10){
    "../Data/Sample_10/"
  } else if (N == 20){
    "../Data/Sample_20/"
  } else {"../Data/Sample_30/"}
  
  sample_size <- (N * 632)
  # path_15 <- paste0(path, "bes_2015.RData")
  path_17 <- paste0(path, "bes_2017.RData")
  path_19 <- paste0(path, "bes_2019.RData")
   
  # ## 2015
  # # Calculate sample size for each area based on relative proportion of full sample size.
  # # Min sample for each area is 1
  # dat <- bes_2015 %>%
  #   group_by(Area) %>%
  #   mutate(num_rows = n()) %>%
  #   ungroup() %>%
  #   mutate(perc = num_rows/nrow(bes_2015), # % of sample distribution among areas
  #          Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
  #          Area_sample = round(Area_sample,0))
  # 
  # # Sample from each Area
  # dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
  #   newdat <- subset(dat, Area == Area_code)
  #   N <- unique(newdat$Area_sample)
  #   set.seed(2154314)
  #   dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  # }
  # dat <- do.call("rbind", dat_sample)
  # save(dat, file = path_15)
  
  ## 2017
  # Calculate sample size for each area based on relative proportion of full sample size.
  # Min sample for each area is 1
  dat <- bes_2017 %>%
    group_by(Area) %>%
    mutate(num_rows = n()) %>%
    ungroup() %>%
    mutate(perc = num_rows/nrow(bes_2017), # % of sample distribution among areas
           Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
           Area_sample = round(Area_sample,0))
  
  # Sample from each Area
  dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
    newdat <- subset(dat, Area == Area_code)
    N <- unique(newdat$Area_sample)
    set.seed(7638884)
    dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  }
  dat <- do.call("rbind", dat_sample)
  save(dat, file = path_17)
  
  ## 2019
  # Calculate sample size for each area based on relative proportion of full sample size.
  # Min sample for each area is 1
  dat <- bes_2019 %>%
    group_by(Area) %>%
    mutate(num_rows = n()) %>%
    ungroup() %>%
    mutate(perc = num_rows/nrow(bes_2019), # % of sample distribution among areas
           Area_sample = ifelse((perc*sample_size)<1, 1, perc*sample_size), # N for sample size, min = 1
           Area_sample = round(Area_sample,0))
  
  # Sample from each Area
  dat_sample <- foreach(Area_code = unique(dat$Area)) %do% {
    newdat <- subset(dat, Area == Area_code)
    N <- unique(newdat$Area_sample)
    set.seed(7638884)
    dat_sample <- sample_n(newdat, N, weight = newdat$wt)
  }
  dat <- do.call("rbind", dat_sample)
  save(dat, file = path_19)
}

## Load bes data
load("../Data/bes_2015.RData")
bes_2015 <- dat

load("../Data/bes_2017.RData")
bes_2017 <- dat

load("../Data/bes_2019.RData")
bes_2019 <- dat
rm(dat)

## Sample from anes
sample_bes(N = 5)
sample_bes(N = 10)
sample_bes(N = 20)
sample_bes(N = 30)

## Check that sampling has worked and variable category are identical
check_func <- function(path){
  check <- list.files(path, full.names = T)
  for (i in 1:length(check)){
    load(check[i])
    print(paste0("Year: ",(unique(dat$Year)), 
                 "| N: ", (nrow(dat)),
                 "| N areas: ", length(unique(dat$Area)),
                 "| N Campaign: ", length(unique(dat$Campaign_week)),
                 "| N Edu: ", length(unique(dat$Education)),
                 "| N Age: ", length(unique(dat$Age)),
                 "| N Region: ", length(unique(dat$Region))))
    
  }
}
check_func("../Data/Sample_5/")  
check_func("../Data/Sample_10/") 
check_func("../Data/Sample_20/") 
check_func("../Data/Sample_30/") 

## Check that each area has sample that matches expected N.
## Should all be 0
path <- list.files("../Data/Sample_10/", full.names = T)
holder <- list()
for (i in 1:length(path)) {
  load(path[i])
  holder[[i]] <- dat %>%
    group_by(Area) %>%
    summarise(N = n(),
              Expected_n = unique(Area_sample),
              diff = N - Expected_n) 
}
  
table(holder[[1]]$diff)
table(holder[[2]]$diff)



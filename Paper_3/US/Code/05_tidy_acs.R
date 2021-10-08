## Tidy ACS 5yr for 2012.

## Set dir
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)
library(data.table)
source("00_Utility.R")


## 2012 ACS for 2012 poststratification
## I've downloaded ACS 2012. Format with a csv for each st
ACS.files <- list.files("../Data/pums_2012/", full.names = TRUE)

## Load each state file and tidy using gather ACS function.
holder <- list()
for (i in 1:length(ACS.files)){
  holder[[i]] <- gather_ACS(path = ACS.files[i])
}

## Bind together all state frames
holder <- do.call("rbind", holder)

## Convert state fips to ST abbreviation
post_2012 <- holder %>%
  mutate(ST = FIPS_to_ST(ST)) %>%
  group_by(ST) %>%
  mutate(perc = sum(pop))%>%
  ungroup() %>%
  mutate(w = pop/perc) %>%
  dplyr::select(-pop, -perc)

save(post_2012, file = "../Data/post_2012.RData")

## 2016
## The data format is with 4 csv files, each with multiple state data
ACS.files <- list.files("../Data/pums_2016/", full.names = T)

holder <- list()
for (i in 1:length(ACS.files)){
  holder[[i]] <- gather_ACS(path = ACS.files[i])
}

## Bind together all state frames
holder <- do.call("rbind", holder)

## Convert state fips to ST abrreviation
post_2016 <- holder %>%
  mutate(ST = FIPS_to_ST(ST)) %>%
  group_by(ST) %>%
  mutate(perc = sum(pop))%>%
  ungroup() %>%
  mutate(w = pop/perc) %>%
  dplyr::select(-pop, -perc)

save(post_2016, file = "../Data/post_2016.RData")

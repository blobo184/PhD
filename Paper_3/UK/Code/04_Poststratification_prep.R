## Amend poststratificaiton frame

## Set working directory
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)
library(rio)

## Load poststratifcaiton frame
post <- read.csv("../Data/Post.csv") %>%
  rename(Area = GSSCode, Age = age0, Education = education) %>%
  mutate(Female = ifelse(sex == "Female", 1, 0)) %>%
  group_by(Area, Female, Age, Education) %>%
  summarise(weight = sum(weight)) %>% 
  ungroup()

## 2017 poststrat frame
# Load and append area vars
load("../Data/bes_2017.RData")
area_vars <- dat %>%
  group_by(Area) %>%
  summarise(Past_Lab = unique(Past_Lab),
            Long_term_unemployed = unique(Long_term_unemployed),
            Population_density = unique(Population_density),
            Industry_manufacturing = unique(Industry_manufacturing),
            leavehanretty = unique(leavehanretty),
            Region = unique(Region))

post_2017 <- left_join(post, area_vars, by = "Area")
rm(area_vars, dat)

## Append Turnout estimate
# load("../Results/Turnout/final_turnout17.RData")
# post_2017 <- cbind(post_2017, post_row_turnout[2])
# rm(post_row_turnout)

## Append actual Labour estimates
truth <- read.csv("../Data/Area_vars.csv") %>%
  dplyr::select(Area = ONSConstID, Turnout17, Lab17) %>%
  mutate(Truth = Turnout17*Lab17) %>%
  dplyr::select(Area, Truth, Turnout = Turnout17)

post_2017 <- left_join(post_2017, truth, by = "Area")
rm(truth)
# Save
save(post_2017, file = "../Data/post_2017.RData")

## 2019
load("../Data/bes_2019.RData")
area_vars <- dat %>%
  group_by(Area) %>%
  summarise(Past_Lab = unique(Past_Lab),
            Long_term_unemployed = unique(Long_term_unemployed),
            Population_density = unique(Population_density),
            Industry_manufacturing = unique(Industry_manufacturing),
            leavehanretty = unique(leavehanretty),
            Region = unique(Region))
post_2019 <- left_join(post, area_vars, by = "Area")
rm(dat, area_vars)

## Append Turnout estimate
# load("../Results/Turnout/final_turnout19.RData")
# post_2019 <- cbind(post_2019, post_row_turnout[2])
# rm(post_row_turnout)

## Append actual Labour estimates
truth <- read.csv("../Data/Area_vars.csv") %>%
  dplyr::select(Area = ONSConstID, Turnout19, lab19) %>%
  mutate(Truth = Turnout19*lab19) %>%
  dplyr::select(Area, Truth, Turnout = Turnout19)
post_2019 <- left_join(post_2019, truth, by = "Area")

# Save
save(post_2019, file = "../Data/post_2019.RData")

############################### End #####################################
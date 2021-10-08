## File to code BES data

## Set working directory as UK folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Libraries and functions
library(tidyverse)
library(data.table)
source("00_Utility.R")

## Vars
# Constituency
# Region
# Age
# Education
# Gender
# Campaign week
# wt

## Function to load and prepare bes data
## It should work with 2015, 2017 and 2019
## You need to specify names of vars and path
prepare_bes <- function(path, age_var, edu_var, gender_var, pcon_var, vote_var, turnout_var) {
  
  ## Necessary libraries and functions
  require(tidyverse)
  source("00_Utility.R")
  
  dat <- rio::import(path) %>%
      dplyr::select(age = age_var, edu = edu_var, gender = gender_var, pcon = pcon_var, 
                    vote = vote_var, turnout = turnout_var, starttime, wt) %>% 
      mutate(Vote = vote_recode(vote, turnout),
             Age = age_bucket(age),
             Education = educ_recode(edu),
             Female = ifelse(rio::characterize(gender) == "Female", 1, 0),
             pcon = rio::characterize(pcon),
             Campaign_week = date_bucket(starttime),
             Lab = ifelse(Vote == "Lab", 1, 0)) %>%
    dplyr::filter(Vote != "WNV", age > 17) 

  pcon_lookup <- read.csv("../Data/pcon_lookup.csv")
  dat <- left_join(dat, pcon_lookup, by = "pcon")

  dat <- dat %>%
    dplyr::select(Area = PCON_ID, Vote, Age, Education, Female, Campaign_week, Lab, wt) %>%
    drop_na()
  dat
}

## Read in area-level vars, to be appended to bes data
area_vars <- read.csv("../Data/area_vars.csv")

## Create dataframe for 2015
# Read and prepare 2015 bes
bes_2015 <- prepare_bes("../Data/bes_2015.sav",
                        vote_var = "generalElectionVote",
                        turnout_var = "turnoutUKGeneral",
                        age_var = "age",
                        edu_var = "p_education",
                        gender_var = "gender",
                        pcon_var = "pcon")

# Select necessary area-level vars
append_2015 <- area_vars %>%
  dplyr::select(Area = ONSConstID, Region, Long_term_unemployed, 
                Population_density, Industry_manufacturing, Past_Lab = Lab10) %>%
  mutate(Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing),
         Past_Lab = arm::rescale(Past_Lab))

# Join bes and area-level vars  
dat <- left_join(bes_2015, append_2015, by = "Area") %>%
  mutate(Year = 2015)

# Save
save(dat, file = "../Data/bes_2015.RData")

## Create dataframe for 2017
# Read and prepare 2017 bes
bes_2017 <- prepare_bes("../Data/bes_2017.sav",
                        vote_var = "generalElectionVote",
                        turnout_var = "turnoutUKGeneral",
                        age_var = "age",
                        edu_var = "p_education",
                        gender_var = "gender",
                        pcon_var = "pcon")

# Select necessary area-level vars
append_2017 <- area_vars %>%
  dplyr::select(Area = ONSConstID, Region, leavehanretty, Long_term_unemployed, 
                Population_density, Industry_manufacturing, Past_Lab = Lab15) %>%
  mutate(leavehanretty = arm::rescale(leavehanretty),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing),
         Past_Lab = arm::rescale(Past_Lab))

# Join bes and area-level vars  
dat <- left_join(bes_2017, append_2017, by = "Area") %>%
  mutate(Year = 2017)
save(dat, file = "../Data/bes_2017.RData")

## Create dataframe for 2019
# Read and prepare 2019 bes
bes_2019 <- prepare_bes("../Data/bes_2019.sav",
                        vote_var = "generalElectionVote",
                        turnout_var = "turnoutUKGeneral",
                        age_var = "age",
                        edu_var = "p_education",
                        gender_var = "gender",
                        pcon_var = "pcon")

# Select necessary area-level vars
append_2019 <- area_vars %>%
  dplyr::select(Area = ONSConstID, Region, leavehanretty, Long_term_unemployed, 
                Population_density, Industry_manufacturing, Past_Lab = Lab17) %>%
  mutate(leavehanretty = arm::rescale(leavehanretty),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing),
         Past_Lab = arm::rescale(Past_Lab))

# Join bes and area-level vars  
dat <- left_join(bes_2019, append_2019, by = "Area") %>%
  mutate(Year = 2019)

# Save
save(dat, file = "../Data/bes_2019.RData")

# Remove all append dataframes
rm(append_2015, append_2017, append_2019, area_vars)

################### End of script ###################
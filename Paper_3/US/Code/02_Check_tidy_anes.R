## Script to check if all coded ANES data frames have same 
## variable level.

## Setwd
setwd(dirname(rstudioapi::documentPath()))

## Libraries
library(tidyverse)


## Load data
load("../Data/anes_2008_tidy.RData")
anes_2008 <- dat
load("../Data/anes_2012_tidy.RData")
anes_2012 <- dat
load("../Data/anes_2016_tidy.RData")
anes_2016 <- dat

## Checks that all var levels are in each data frame

## State - 2008 only has 34 states.
#stopifnot(levels(as.factor(anes_2008$ST)) == levels(as.factor(anes_2012$ST)))
stopifnot(levels(as.factor(anes_2016$ST)) == levels(as.factor(anes_2012$ST)))

## Region
stopifnot(levels(as.factor(anes_2008$Region)) == levels(as.factor(anes_2012$Region)))
stopifnot(levels(as.factor(anes_2016$Region)) == levels(as.factor(anes_2012$Region)))

## Vote
stopifnot(levels(as.factor(anes_2008$Vote)) == levels(as.factor(anes_2012$Vote)))
stopifnot(levels(as.factor(anes_2016$Vote)) == levels(as.factor(anes_2012$Vote)))

## Female
stopifnot(levels(as.factor(anes_2008$Female)) == levels(as.factor(anes_2012$Female)))
stopifnot(levels(as.factor(anes_2016$Female)) == levels(as.factor(anes_2012$Female)))

## Ethnicity
stopifnot(levels(as.factor(anes_2008$Ethnicity)) == levels(as.factor(anes_2012$Ethnicity)))
stopifnot(levels(as.factor(anes_2016$Ethnicity)) == levels(as.factor(anes_2012$Ethnicity)))

## Age
stopifnot(levels(as.factor(anes_2008$Age)) == levels(as.factor(anes_2012$Age)))
stopifnot(levels(as.factor(anes_2016$Age)) == levels(as.factor(anes_2012$Age)))

## Education
stopifnot(levels(as.factor(anes_2008$Education)) == levels(as.factor(anes_2012$Education)))
stopifnot(levels(as.factor(anes_2016$Education)) == levels(as.factor(anes_2012$Education)))
table(anes_2012$Education)
table(anes_2016$Education)

## Campaign_week
stopifnot(levels(as.factor(anes_2008$Campaign_week)) == levels(as.factor(anes_2012$Campaign_week)))
stopifnot(levels(as.factor(anes_2016$Campaign_week)) == levels(as.factor(anes_2012$Campaign_week)))

## Check State N after dropping na for each survey
## I'm really only interested in low N states

# 2008
check_na_2008 <- anes_2008 %>%
  group_by(Area) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(Area) %>%
  summarise(N = unique(N),
            N_na = n())
view(check_na_2008)

# 2012
check_na_2012 <- anes_2012 %>%
  group_by(Area) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(Area) %>%
  summarise(N = unique(N),
            N_na = n())
view(check_na_2012)

# 2016
check_na_2016<- anes_2016 %>%
  group_by(Area) %>%
  mutate(N = n()) %>%
  ungroup() %>%
  drop_na() %>%
  group_by(Area) %>%
  summarise(N = unique(N),
            N_na = n())
view(check_na_2016)

## Script to sample from bes full sample

## First source 99_calculate_sample_dist.R 
## To calculate sample for groups. Need to set sample size
sample <- (632*20) # Here I calculate total sample size based on avg. 20 per small area
source("Code/99_Calculate_sample_dist.R") # Runs script that calculates distributions

## Create samples for vote choice
load("Data/bes_votechoice_dat.RData")

bes <- bes %>%
  dplyr::filter(generalElectionVote != "I would/did not vote")

## Calculate margin of victory for 2017 results and append 
## constituency grouping to bes data
results <- read.csv("Data/Results.csv")
results <- results %>%
  dplyr::select(pcon, GSSCode, Result_2017) %>%
  mutate(Result_2017 = abs(Result_2017-(1-Result_2017)),
         Result_2017 = round(Result_2017,2),
         Groups3_17 = cut(Result_2017, 
                          breaks = c(-Inf, 0.05, 0.1, Inf),
                          labels = c(1:3)),
         Groups2_17 = cut(Result_2017, 
                          breaks = c(-Inf, 0.1, Inf),
                          labels = c(1:2)))

bes <- left_join(bes, results, by ="pcon")
rm(results)

# Some NAs introduced b/c some false constituencies
bes <- bes %>%
  drop_na() ## This drops them, as they will have na for GSSCode

## Append area level variables with gsscode
area_level_vars <- read.csv("Data/Area_level_vars.csv")
bes <- left_join(bes, area_level_vars, by = c("GSSCode" = "ONSConstID"))
rm(area_level_vars)

## Create samples for ratio of 0.3:0.2:0.5 first
## First calculate sample per group

bes <- bes %>%
  mutate(Groups3_17 = case_when(
    Groups3_17 == 1 ~ 44,
    Groups3_17 == 2 ~ 29,
    Groups3_17 == 3 ~ 15))


dat <- foreach(const = unique(bes$GSSCode)) %do% {
  temp.dat <- subset(bes, GSSCode == const)
  N.target <- temp.dat$Groups3_17[1]
  N <- if (nrow(temp.dat) < N.target) {
    nrow(temp.dat)} else {N.target}
  set.seed(21092020)
  dat <- sample_n(temp.dat, N, replace = F, weight = wt)
  dat
}

dat <- do.call("rbind", dat)

## Check that majority of required sample sizes are met.
samples <- dat %>%
  group_by(GSSCode) %>%
  summarise(N = n(),
            G = unique(Groups3_17))
round(prop.table(table(samples$N, samples$G),2),4)
rm(samples)
save(dat, file = "Data/Bes_three.RData")

## Create samples for ratio of 2:1 next
bes <- bes %>%
  mutate(Groups2_17 = case_when(
    Groups2_17  == 1 ~ 32,
    Groups2_17  == 2 ~ 16))

dat <- foreach(const = unique(bes$GSSCode)) %do% {
  temp.dat <- subset(bes, GSSCode == const)
  N.target <- temp.dat$Groups2_17[1]
  N <- if (nrow(temp.dat) < N.target) {
    nrow(temp.dat)} else {N.target}
  set.seed(21092020)
  dat <- sample_n(temp.dat, N, replace = F, weight = wt)
  dat
}

dat <- do.call("rbind", dat)

## Check that majority of required sample sizes are met.
samples <- dat %>%
  group_by(GSSCode) %>%
  summarise(N = n(),
            G = unique(Groups2_17))
round(prop.table(table(samples$N, samples$G),2),4)
save(dat, file = "Data/Bes_two.RData")
rm(samples)

## Even sample
dat <- foreach(const = unique(bes$GSSCode)) %do% {
  temp.dat <- subset(bes, GSSCode == const)
  N <- ifelse(nrow(temp.dat) < 20, nrow(temp.dat), 20)
  set.seed(21092020)
  dat <- sample_n(temp.dat, N, replace = F, weight = wt)
  dat
}

dat <- do.call("rbind", dat)

## Check that majority of required sample sizes are met.
samples <- dat %>%
  group_by(GSSCode) %>%
  summarise(N = n(),
            G = 20)

round(prop.table(table(samples$N, samples$G),2),4)

save(dat, file = "Data/Bes_Even.RData")

################################# END ##############


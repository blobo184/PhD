## Sample CCES - script to create samples from total CCES samples
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

## First need to call 99_calulate_samples, which calculates 
## Sample size per area for 3:2:1 and 2:1 distributions. Based
## on sample size set. 
sample <- (51*30) # 51 is no. of States. Second no. is N per area.
source("Code/99_Calculate_sample_dist.R")

## Now Load CCES
load("Data/CCES_recoded.RData")

# Remove DNV and other
dat <- dat %>%
  dplyr::filter(Pres_vi != "DNV" & Pres_vi != "Other")

## Merge with past vote and Rep/Dem diff
Past_vote <- Past_vote %>%
  mutate(Rep_2012 = arm:::rescale(Rep))
dat <- left_join(dat, Past_vote, by = c("ST" = "state_po"))

## Recode day_from_elec into campaign week.
breaks <- c(-Inf, 7, 14, 21, 28, Inf)
labs <- c(1:5)
dat$campaign_week <- cut(as.numeric(dat$days_from_elec),
                         breaks = breaks,
                         labels = labs)

## Here you need to recode the sample per group. 
# Using group_2_dist and group_2_dist, set N per area
dat <- dat %>%
  mutate(Groups_2 = case_when(
    Groups_2 == 1 ~ 46,
    Groups_2 == 2 ~ 23),
         Groups_3 = case_when(
    Groups_3 == 1 ~ 63,
    Groups_3 == 2 ~ 42,
    Groups_3 == 3 ~ 21),
         date_w = 1/as.numeric(as.character(days_from_elec)),
           w_v2 = w*date_w) # Adjust weight to preference more recent sample


sample <- function(Groups) {
  model_sample <- foreach(const = unique(dat$ST)) %do% {
    newdat <- subset(dat, ST == const)
    N <- if(Groups == "Even") {
      unique(newdat$Even)
    } else if (Groups == "Two") {
      unique(newdat$Groups_2)
    } else if (Groups == "Three") {
      unique(newdat$Groups_3)
    } else {
      stop("Wront input, please put either Even, Two or Three")
    }
    set.seed(18122020)
    model_sample <- sample_n(newdat, N, replace = FALSE, weight = newdat$w)
  }
  
 sample <- do.call("rbind", model_sample)
}

sample_Even <- sample(Groups = "Even")
sample_Two <- sample(Groups = "Two")
sample_Three <- sample(Groups = "Three")

save(sample_Even, file = "Data/sample_Even.RData")
save(sample_Two, file = "Data/sample_Two.RData")
save(sample_Three, file = "Data/sample_Three.RData")

##  End
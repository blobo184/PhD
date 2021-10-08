## Creating fake data for multilevel simulation
## Ben Lobo
## 22/07/2020

## Read in poststrat frame
## Select three variables from poststrat frame
dat <- read.csv("Data/Post.csv")

## First create the three individual-level vairbles
## Each named X, with categories of 2,3 and 4
dat <- dat %>%
  mutate(X4 = case_when(
    education %in% "No qualifications" == "TRUE" ~ "1",
    education %in% c("Level 1", "Other") == "TRUE" ~ "2",
    education %in% "Level 2" == "TRUE" ~ "3",
    education %in% "Level 3" == "TRUE" ~ "4",
    education %in% "Level 4/5" == "TRUE" ~ "5"),
    X3 = case_when(
    age0 %in% c("16-19", "20-24") == "TRUE" ~ "1",
    age0 %in% c("25-29", "30-44") == "TRUE" ~ "2",
    age0 %in% c("45-59", "60-64") == "TRUE" ~ "3",
    age0 %in% c("65-74", "75+") == "TRUE" ~ "4"),
    X2 = case_when(
    hrsocgrd %in% "AB" == "TRUE" ~ "3",
    hrsocgrd %in% "C1" == "TRUE" ~ "2",
    hrsocgrd %in% c("C2", "DE") == "TRUE" ~ "1"),
    X1 = ifelse(sex == "Female", 1, 0))

dat <-  dat %>%
  dplyr::select(GSSCode, X1, X2, X3, X4, weight) %>%
  group_by(GSSCode, X1, X2, X3, X4) %>%
  summarise(weight = sum(weight))
dat <- as.data.frame(dat)

# Create individual-level error
set.seed(30072020)
dat <- dat %>%
  mutate(X_Err = rnorm(nrow(dat), mean = -0.25, sd = 0.5))

## Append area-level variables

## Append with different variables
appends <- read.csv("Data/Area_vars.csv")
names(appends)[1] <- "GSSCode"
set.seed(30072020)
appends <- appends %>%
  mutate(A2 = (1-A2),
         Err1 = rnorm(nrow(appends), mean = -0.2, sd = 1))
dat <- left_join(dat, appends, by ="GSSCode")
rm(appends)

## Generate the y variable
## First define different beta values
beta_X1 <- 0.5 # X1 coef
beta_X2 <- 1
beta_X3 <- 0.75
beta_X4 <- -1.25
beta_A1 <- 2.75 # A1 coef
beta_A2 <-  -2.5 # A2 coef
beta_A3 <- -2 # A3 coef
beta_E1 <-  1.5

## Create Y
set.seed(30072020)
dat <- dat %>%
  mutate(X2 = as.numeric(X2),
         X3 = as.numeric(X3),
         X4 = as.numeric(X4),
         Y = rbinom(nrow(dat), 1, prob = invlogit(beta_X1*X1 +
                                                    beta_X2*X2 +
                                                    beta_X3*X3 +
                                                    beta_X4*X4 +
                                                    beta_A1*A1 +
                                                    beta_A2*A2 +
                                                    beta_A3*A3 +
                                                    beta_E1*Err1)))

save(dat, file = "Data/Master.data.RData")
load("Data/Master.data.RData")

## Function to sample Areas from master which has 632 areas
rm(list=setdiff(ls(), "dat"))
dat_sample <- function(dat, Areas_no){
  areas <- unique(dat$GSSCode)
  set.seed(30072020)
  sampled.areas <- sample(areas, Areas_no, replace = FALSE)
  dat_sample <- dat[dat$GSSCode %in% sampled.areas,]
  
  dat_grouped <- dat_sample %>%
    group_by(GSSCode) %>%
    summarise(Est = sum(Y*weight)/sum(weight)) %>%
    mutate(Est_opp = (1-Est),
           Diff = abs(Est - Est_opp))
  
  ## Find the break points in Diff based on different quantiles.
  quant_2 <- quantile(dat_grouped$Diff, probs = seq(0,1, by =  (1/2))) # 2 groups
  quant_3 <- quantile(dat_grouped$Diff, probs = seq(0,1, by =  (1/3))) # 3 groups
  quant_4 <- quantile(dat_grouped$Diff, probs = seq(0,1, by =  (1/4))) # 4 groups 
  quant_5 <- quantile(dat_grouped$Diff, probs = seq(0,1, by =  (1/5))) # 5 groups
  
  
  ## Create groups based on number of breaks - 
  dat_grouped <- dat_grouped %>%
    mutate(Groups_2 = cut(Diff, breaks = quant_2, labels = c(1:2), include.lowest = TRUE),
           Groups_3 = cut(Diff, breaks = quant_3, labels = c(1:3),include.lowest = TRUE),
           Groups_4 = cut(Diff, breaks = quant_4, labels = c(1:4),include.lowest = TRUE),
           Groups_5 = cut(Diff, breaks = quant_5, labels = c(1:5),include.lowest = TRUE),
           Groups_1 = 1) %>%
    dplyr::select(c(-Est, -Est_opp, -Diff))
  
  dat <- left_join(dat_sample, dat_grouped, by = "GSSCode")
  
}

# Create samples for 50, 200, 400 and 600 Areas

dat_50_areas <- dat_sample(dat=dat, Areas_no = 50)
dat_200_areas <- dat_sample(dat=dat, Areas_no = 200)
dat_400_areas <- dat_sample(dat=dat, Areas_no = 400)
dat_600_areas <- dat_sample(dat=dat, Areas_no = 600)

save(dat_50_areas, file = "Data/dat_50_areas.RData")
save(dat_200_areas, file = "Data/dat_200_areas.RData")
save(dat_400_areas, file = "Data/dat_400_areas.RData")
save(dat_600_areas, file = "Data/dat_600_areas.RData")

rm(list=setdiff(ls(), "dat"))


## ## ## ## ## ## ## ## ## ## ## ## ## ##  END ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
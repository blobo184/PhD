## Create samples
## Script creates samples randomly drawn from full poststrat frame for each N areas.

## Sampler function - Generates sample depending on sample per area
## and the Ratios you need, referenced by number of groups

sampler <- function(Area_n, Groups, dat, s) {
  Areas_no <- length(unique(dat$GSSCode))
  total_sample <-  Areas_no* Area_n
  ratio <- if(Groups == 1){
    1
  } else if (Groups == 2) {
    c(2,1)
  } else if (Groups == 3) {
    c(3,2,1)
  } else if (Groups == 4) {
    c(4,3,2,1)
  } else if (Groups == 5) {
    c(5,4,3,2,1)
  }
  G <- if(Groups == 1){
    1
  } else if (Groups == 2) {
    3
  } else if (Groups == 3) {
    6
  } else if (Groups == 4) {
    10
  } else if (Groups == 5) {
    15
  }
  sample <- (round(total_sample/G,0)) * ratio
  sample_groups <- round(sample / (Areas_no/Groups),0)
  model_sample <- foreach(const = unique(dat$GSSCode)) %do% {
    newdat <- subset(dat, GSSCode == const)
    N <- if(Groups == 1) {
      sample_groups[unique(newdat$Groups_1)]
    } else if (Groups == 2) {
      sample_groups[unique(newdat$Groups_2)]
    } else if (Groups == 3) {
      sample_groups[unique(newdat$Groups_3)]
    } else if (Groups == 4) {
      sample_groups[unique(newdat$Groups_4)]
    } else if (Groups == 5) {
      sample_groups[unique(newdat$Groups_5)]
    }
    set.seed(07092020)
    model_sample <- sample_n(newdat, N, replace = TRUE, weight = newdat$weight)
    
  }
  model_sample <- do.call("rbind", model_sample)
}


## First create directories
# Function to create Area and ratio sub-directories
create_sub_folders<-function(x){
  if(dir.exists(x) == "TRUE") {
    print("dir already exist")}
  else {
    dir.create(x)
    dir.create(paste(x, "Even", sep = "/"))
    dir.create(paste(x, "2to1", sep = "/"))
    dir.create(paste(x, "3to1", sep = "/"))
  }
}

create_sub_folders("Data/Areas_50")
create_sub_folders("Data/Areas_200")
create_sub_folders("Data/Areas_400")
create_sub_folders("Data/Areas_600")

## Sampler to generate specific samples
## Need to specify:
## Sample per area (Area_n)
## Number of areas dataframe, (dat)
## The ratio (Groups)

#### Sampling for 50 areas.
load("Data/dat_50_areas.RData") # Dataframe for 50 Areas

## Create samples for 50 Areas, with even ratios 
Areas50_sample_5 <- sampler(Area_n = 5, dat = dat_50_areas, Groups = 1)
Areas50_sample_10 <- sampler(Area_n = 10, dat = dat_50_areas, Groups = 1)
Areas50_sample_20 <- sampler(Area_n = 20, dat = dat_50_areas, Groups = 1)
Areas50_sample_30 <- sampler(Area_n = 30, dat = dat_50_areas, Groups = 1)

save(Areas50_sample_5, file = "Data/Areas_50/Even/Areas50_sample_5.RData")
save(Areas50_sample_10, file = "Data/Areas_50/Even/Areas50_sample_10.RData")
save(Areas50_sample_20, file = "Data/Areas_50/Even/Areas50_sample_20.RData")
save(Areas50_sample_30, file = "Data/Areas_50/Even/Areas50_sample_30.RData")


## Create samples for 50 Areas, with 2 groups and ratio of 2:1
Areas50_sample_5 <- sampler(Area_n = 5, dat = dat_50_areas, Groups = 2)
Areas50_sample_10 <- sampler(Area_n = 10, dat = dat_50_areas, Groups = 2)
Areas50_sample_20 <- sampler(Area_n = 20, dat = dat_50_areas, Groups = 2)
Areas50_sample_30 <- sampler(Area_n = 30, dat = dat_50_areas, Groups = 2)

save(Areas50_sample_5, file = "Data/Areas_50/2to1/Areas50_sample_5.RData")
save(Areas50_sample_10, file = "Data/Areas_50/2to1/Areas50_sample_10.RData")
save(Areas50_sample_20, file = "Data/Areas_50/2to1/Areas50_sample_20.RData")
save(Areas50_sample_30, file = "Data/Areas_50/2to1/Areas50_sample_30.RData")

## Create samples for 50 Areas, with 3 groups and a ratio of 3:2:1
Areas50_sample_5 <- sampler(Area_n = 5, dat = dat_50_areas, Groups = 3)
Areas50_sample_10 <- sampler(Area_n = 10, dat = dat_50_areas, Groups = 3)
Areas50_sample_20 <- sampler(Area_n = 20, dat = dat_50_areas, Groups = 3)
Areas50_sample_30 <- sampler(Area_n = 30, dat = dat_50_areas, Groups = 3)

save(Areas50_sample_5, file = "Data/Areas_50/3to1/Areas50_sample_5.RData")
save(Areas50_sample_10, file = "Data/Areas_50/3to1/Areas50_sample_10.RData")
save(Areas50_sample_20, file = "Data/Areas_50/3to1/Areas50_sample_20.RData")
save(Areas50_sample_30, file = "Data/Areas_50/3to1/Areas50_sample_30.RData")

rm(list = ls(pattern = "Areas50"))

########## Sampling for 200 areas 
load("Data/dat_200_areas.RData") # Dataframe for 200 areas

## Create samples for 200 Areas, with even groups
Areas200_sample_5 <- sampler(Area_n = 5, dat = dat_200_areas, Groups = 1)
Areas200_sample_10 <- sampler(Area_n = 10, dat = dat_200_areas, Groups = 1)
Areas200_sample_20 <- sampler(Area_n = 20, dat = dat_200_areas, Groups = 1)
Areas200_sample_30 <- sampler(Area_n = 30, dat = dat_200_areas, Groups = 1)

save(Areas200_sample_5, file = "Data/Areas_200/Even/Areas200_sample_5.RData")
save(Areas200_sample_10, file = "Data/Areas_200/Even/Areas200_sample_10.RData")
save(Areas200_sample_20, file = "Data/Areas_200/Even/Areas200_sample_20.RData")
save(Areas200_sample_30, file = "Data/Areas_200/Even/Areas200_sample_30.RData")


## Create samples for 200 Areas, with 2 groups and a ratio of 2:1
Areas200_sample_5 <- sampler(Area_n = 5, dat = dat_200_areas, Groups = 2)
Areas200_sample_10 <- sampler(Area_n = 10, dat = dat_200_areas, Groups = 2)
Areas200_sample_20 <- sampler(Area_n = 20, dat = dat_200_areas, Groups = 2)
Areas200_sample_30 <- sampler(Area_n = 30, dat = dat_200_areas, Groups = 2)

save(Areas200_sample_5, file = "Data/Areas_200/2to1/Areas200_sample_5.RData")
save(Areas200_sample_10, file = "Data/Areas_200/2to1/Areas200_sample_10.RData")
save(Areas200_sample_20, file = "Data/Areas_200/2to1/Areas200_sample_20.RData")
save(Areas200_sample_30, file = "Data/Areas_200/2to1/Areas200_sample_30.RData")

## Create samples for 200 Areas, with 3 groups and a ratio of 3:2:1
Areas200_sample_5 <- sampler(Area_n = 5, dat = dat_200_areas, Groups = 3)
Areas200_sample_10 <- sampler(Area_n = 10, dat = dat_200_areas, Groups = 3)
Areas200_sample_20 <- sampler(Area_n = 20, dat = dat_200_areas, Groups = 3)
Areas200_sample_30 <- sampler(Area_n = 30, dat = dat_200_areas, Groups = 3)

save(Areas200_sample_5, file = "Data/Areas_200/3to1/Areas200_sample_5.RData")
save(Areas200_sample_10, file = "Data/Areas_200/3to1/Areas200_sample_10.RData")
save(Areas200_sample_20, file = "Data/Areas_200/3to1/Areas200_sample_20.RData")
save(Areas200_sample_30, file = "Data/Areas_200/3to1/Areas200_sample_30.RData")

rm(list = ls(pattern = "Areas200"))


######### Create data samples for 400 Areas
load("Data/dat_400_areas.RData") # Dataframe for 400 areas

## Create samples for 400 Areas, with even groups
Areas400_sample_5 <- sampler(Area_n = 5, dat = dat_400_areas, Groups = 1)
Areas400_sample_10 <- sampler(Area_n = 10, dat = dat_400_areas, Groups = 1)
Areas400_sample_20 <- sampler(Area_n = 20, dat = dat_400_areas, Groups = 1)
Areas400_sample_30 <- sampler(Area_n = 30, dat = dat_400_areas, Groups = 1)

save(Areas400_sample_5, file = "Data/Areas_400/Even/Areas400_sample_5.RData")
save(Areas400_sample_10, file = "Data/Areas_400/Even/Areas400_sample_10.RData")
save(Areas400_sample_20, file = "Data/Areas_400/Even/Areas400_sample_20.RData")
save(Areas400_sample_30, file = "Data/Areas_400/Even/Areas400_sample_30.RData")


## Create samples for 400 Areas, with 2 groups and a ratio of 2:1
Areas400_sample_5 <- sampler(Area_n = 5, dat = dat_400_areas, Groups = 2)
Areas400_sample_10 <- sampler(Area_n = 10, dat = dat_400_areas, Groups = 2)
Areas400_sample_20 <- sampler(Area_n = 20, dat = dat_400_areas, Groups = 2)
Areas400_sample_30 <- sampler(Area_n = 30, dat = dat_400_areas, Groups = 2)

save(Areas400_sample_5, file = "Data/Areas_400/2to1/Areas400_sample_5.RData")
save(Areas400_sample_10, file = "Data/Areas_400/2to1/Areas400_sample_10.RData")
save(Areas400_sample_20, file = "Data/Areas_400/2to1/Areas400_sample_20.RData")
save(Areas400_sample_30, file = "Data/Areas_400/2to1/Areas400_sample_30.RData")

## Create samples for 400 Areas, with 2 groups and a ratio of 3:2:1
Areas400_sample_5 <- sampler(Area_n = 5, dat = dat_400_areas, Groups = 3)
Areas400_sample_10 <- sampler(Area_n = 10, dat = dat_400_areas, Groups = 3)
Areas400_sample_20 <- sampler(Area_n = 20, dat = dat_400_areas, Groups = 3)
Areas400_sample_30 <- sampler(Area_n = 30, dat = dat_400_areas, Groups = 3)

save(Areas400_sample_5, file = "Data/Areas_400/3to1/Areas400_sample_5.RData")
save(Areas400_sample_10, file = "Data/Areas_400/3to1/Areas400_sample_10.RData")
save(Areas400_sample_20, file = "Data/Areas_400/3to1/Areas400_sample_20.RData")
save(Areas400_sample_30, file = "Data/Areas_400/3to1/Areas400_sample_30.RData")

rm(list = ls(pattern = "Areas400"))

######### Create data samples for 600 Areas
load("Data/dat_600_areas.RData") # Dataframe for 600 areas

## Create samples for 600 Areas, with even groups
Areas600_sample_5 <- sampler(Area_n = 5, dat = dat_600_areas, Groups = 1)
Areas600_sample_10 <- sampler(Area_n = 10, dat = dat_600_areas, Groups = 1)
Areas600_sample_20 <- sampler(Area_n = 20, dat = dat_600_areas, Groups = 1)
Areas600_sample_30 <- sampler(Area_n = 30, dat = dat_600_areas, Groups = 1)

save(Areas600_sample_5, file = "Data/Areas_600/Even/Areas600_sample_5.RData")
save(Areas600_sample_10, file = "Data/Areas_600/Even/Areas600_sample_10.RData")
save(Areas600_sample_20, file = "Data/Areas_600/Even/Areas600_sample_20.RData")
save(Areas600_sample_30, file = "Data/Areas_600/Even/Areas600_sample_30.RData")


## Create samples for 600 Areas, with 2 groups and a ratio of 2:1
Areas600_sample_5 <- sampler(Area_n = 5, dat = dat_600_areas, Groups = 2)
Areas600_sample_10 <- sampler(Area_n = 10, dat = dat_600_areas, Groups = 2)
Areas600_sample_20 <- sampler(Area_n = 20, dat = dat_600_areas, Groups = 2)
Areas600_sample_30 <- sampler(Area_n = 30, dat = dat_600_areas, Groups = 2)

save(Areas600_sample_5, file = "Data/Areas_600/2to1/Areas600_sample_5.RData")
save(Areas600_sample_10, file = "Data/Areas_600/2to1/Areas600_sample_10.RData")
save(Areas600_sample_20, file = "Data/Areas_600/2to1/Areas600_sample_20.RData")
save(Areas600_sample_30, file = "Data/Areas_600/2to1/Areas600_sample_30.RData")

## Create samples for 600 Areas, with 2 groups and a ratio of 3:2:1
Areas600_sample_5 <- sampler(Area_n = 5, dat = dat_600_areas, Groups = 3)
Areas600_sample_10 <- sampler(Area_n = 10, dat = dat_600_areas, Groups = 3)
Areas600_sample_20 <- sampler(Area_n = 20, dat = dat_600_areas, Groups = 3)
Areas600_sample_30 <- sampler(Area_n = 30, dat = dat_600_areas, Groups = 3)

save(Areas600_sample_5, file = "Data/Areas_600/3to1/Areas600_sample_5.RData")
save(Areas600_sample_10, file = "Data/Areas_600/3to1/Areas600_sample_10.RData")
save(Areas600_sample_20, file = "Data/Areas_600/3to1/Areas600_sample_20.RData")
save(Areas600_sample_30, file = "Data/Areas_600/3to1/Areas600_sample_30.RData")

rm(list = ls(pattern = "Areas600"))
rm(list=ls(pattern = "_areas"))
rm(sampler, dat)
################################# End #################################

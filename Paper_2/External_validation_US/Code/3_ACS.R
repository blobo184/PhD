library(tidyverse)
library(data.table)


## Function to import and tidy ACS data
## This saves importing v. large files with unnecessary vars.
gather_ACS <- function(path){
  age_breaks <- c(17, 34, 44, 64, Inf)
  age_labs <- c("18-34", "35-44", "45-64", "65+")
  psa <- fread(path,
                              sep = ",",
                              select = c("HISP", "ST", "AGEP", 
                                         "SEX", "SCHL", "RAC1P", "PWGTP", "MAR")) %>%
  dplyr::filter(AGEP > 17) %>%
  mutate(Age = cut(AGEP,
                    breaks = age_breaks,
                    labels = age_labs)) %>%
  mutate(Gender = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female",
    TRUE ~ NA_character_)) %>%
  mutate(Education = dplyr::case_when(
    SCHL %in% 1:17 ~ "HS or less",
    SCHL %in% 18:19 ~ "Some college",
    SCHL %in% 20:21 ~ "College grad",
    SCHL %in% 22:24 ~ "Postgrad")) %>%
  mutate(Race = dplyr::case_when(
    HISP > 1 ~ "Hispanic",
    RAC1P == 1 ~ "White",
    RAC1P == 2 ~ "Black",
    RAC1P %in% 3:9 ~ "Other")) %>%
    mutate(Marital = dplyr::case_when(
      MAR == 1 ~ "Married",
      MAR == 2 ~ "Widowed",
      MAR == 3 ~ "Sep-Divorced",
      MAR == 4 ~ "Sep-Divorced",
      MAR == 5 ~ "Single")) %>%
  group_by(ST, Age, Gender, Education, Race, Marital) %>%
  summarise(pop = sum(PWGTP))
  psa
}

## Gather ACS and summarise ACS data
ACS <- list.files("Data", pattern = "psam_pus", full.names = TRUE)
ACS1 <- gather_ACS(path = ACS[1])
ACS2 <- gather_ACS(path = ACS[2])
ACS3 <- gather_ACS(path = ACS[3])
ACS4 <- gather_ACS(path = ACS[4])

## Bind together 4 sep. frames
ACS <- rbind(ACS1, ACS2)
ACS <- rbind(ACS, ACS3)
ACS <- rbind(ACS, ACS4)
rm(paths, i, gather_ACS, ACS1, ACS2, ACS3, ACS4)

ACS <- ACS %>%
  mutate(ST = dplyr::recode(as.character(ST),
                            "1" = "AL",	"2" = "AK",	"4" = "AZ",	"5" = "AR",
                            "6" = "CA",	"8" = "CO",	"9" = "CT",	"10" = "DE",
                            "11" = "DC",	"12" = "FL",	"13" = "GA",	"15" = "HI",
                            "16" = "ID",	"17" = "IL",	"18" = "IN",	"19" = "IA",
                            "20" = "KS",	"21" = "KY",	"22" = "LA",	"23" = "ME",
                            "24" = "MD",	"25" = "MA",	"26" = "MI",	"27" = "MN",
                            "28" = "MS",	"29" = "MO",	"30" = "MT",	"31" = "NE",
                            "32" = "NV",	"33" = "NH",	"34" = "NJ",	"35" = "NM",
                            "36" = "NY",	"37" = "NC",	"38" = "ND",	"39" = "OH",
                            "40" = "OK",	"41" = "OR",	"42" = "PA",	"44" = "RI",
                            "45" = "SC",	"46" = "SD",	"47" = "TN",	"48" = "TX",
                            "49" = "UT",	"50" = "VT",	"51" = "VA",	"53" = "WA",
                            "54" = "WV",	"55" = "WI",	"56" = "WY"))
save(ACS, file = "Data/ACS.RData")

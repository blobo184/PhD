## Preperation of BES data
## BES data downloaded from:
## Wave 18 - campaign period of 2019 UK eleciton

## Load BES data
bes <- read.spss("Data/BES2019_W18_v0.5.sav", use.value.labels = TRUE, to.data.frame = T)

## Create gender variable
bes <- bes %>%
  mutate(sex = gender)
table(bes$sex, useNA = "ifany")

## ## Age
age_breaks <- c(-Inf, 19, 24, 29, 44, 59, 64, 74, Inf)
age_labels <- c("16-19", "20-24", "25-29", "30-44", "45-59", "60-64", "65-74", "75+")

bes$age.temp <- as.numeric(as.character(bes$age))
bes$age0 <- cut(bes$age.temp,
                breaks = age_breaks,
                labels = age_labels)

table(bes$age0, bes$age)# Check age has worked
rm(age_breaks, age_labels)


## Social grade - Does this exist for 2019??
table(bes$p_socgrade, useNA = "ifany")
bes$hrsocgrd <- dplyr::recode(bes$p_socgrade,
                              "A" = "AB",
                              "B" = "AB",
                              "C1" = "C1",
                              "C2" = "C2",
                              "D" = "DE",
                              "E" = "DE",
                              .default = NA_character_)

table(bes$hrsocgrd, bes$p_socgrade, useNA = "ifany") # Check

## Education
bes$education <- dplyr::recode(as.character(bes$p_education),
                               "City & Guilds certificate" = "Level 1",
                               "City & Guilds certificate - advanced" = "Level 2",
                               "Clerical and commercial" = "Level 1",
                               "CSE grade 1, GCE O level, GCSE, School Certificate" = "Level 2",
                               "CSE grades 2-5" = "Level 1",
                               "Don't know" = "No qualifications",
                               "GCE A level or Higher Certificate" = "Level 3",
                               "No formal qualifications" = "No qualifications",
                               "Nursing qualification (e.g. SEN, SRN, SCM, RGN)" = "Level 4/5",
                               "ONC" = "Level 2",
                               "Other technical, professional or higher qualification" = "Other",
                               "Prefer not to say" = "No qualifications",
                               "Recognised trade apprenticeship completed" = "Level 2",
                               "Scottish Higher Certificate" = "Level 3",
                               "Scottish Ordinary/ Lower Certificate" = "Level 2",
                               "Teaching qualification (not degree)" = "Level 4/5",
                               "University diploma" = "Level 4/5",
                               "University or CNAA first degree (e.g. BA, B.Sc, B.Ed)" = "Level 4/5",
                               "University or CNAA higher degree (e.g. M.Sc, Ph.D)" = "Level 4/5",
                               "Youth training certificate/skillseekers" = "Level 2")

table(bes$education, useNA = "ifany") # Check

## Recode housing and drop old housing variable
bes$housing <- dplyr::recode(bes$p_housing,
                             "Own â.... outright" = "Owns",
                             "Own â.... with a mortgage" = "Owns",
                             "Own (part-own) â.... through shared ownership scheme (i.e. pay part mortgage, part rent)" = "Owns",
                             "Rent â.... from a private landlord" = "Rents",
                             "Rent â.... from my local authority" = "Rents",
                             "Rent â.... from a housing association" = "Rents",
                             "It belongs to a Housing Association" = "Rents",
                             "Neither â.... I live with my parents, family or friends but pay some rent to them" = "Rents",
                             "Neither â.... I live rent-free with my parents, family or friends" = "Rents",
                             .default = NA_character_)
table(bes$p_housing, bes$housing)

## Create 
bes$Campaign_week <- cut(bes$CampaignDay,
               breaks = c(0, 7, 14, 21,29),
               labels = c(1:4))


## Select necessary variables
bes <- bes %>%
  dplyr::select(id, pcon, sex, age0, hrsocgrd, housing, education, Campaign_week, generalElectionVote, wt)

save(bes, file = "Data/bes_master.RData")
load("Data/bes_master.RData")

## Turnout data
bes <- bes %>%
  dplyr::select(id, pcon, sex, age0, education, Campaign_week, generalElectionVote, wt) %>%
  mutate(Turnout = ifelse(generalElectionVote == "I would/did not vote" | generalElectionVote == "Don't know", 0,1))

bes <- bes %>%
  drop_na()
save(bes, file = "Data/bes_turnout_dat.RData")

## Vote choice data
load("Data/bes_master.RData")
bes <- bes %>%
  dplyr::select(id, pcon, sex, age0, education, Campaign_week, generalElectionVote, wt) %>%
  dplyr::filter(generalElectionVote != "Don't know") %>% ## Exclude those with don't know for vote
  mutate(Con = ifelse(generalElectionVote == "Conservative", 1, 0)) ## Create vote conservation vote as binary

bes <- bes %>%
  drop_na()
save(bes, file = "Data/bes_votechoice_dat.RData")
rm(bes)
###############  ENd ###############

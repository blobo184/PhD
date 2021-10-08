## Poststratification of model estimates

library(xlsx)
## Call functions
source("Code/0_Functions.R")

# Load poststrat
load("Data/poststrat.rdata")

# poststrat
Results_Even <- poststrat(model = "Even", post= post)
Results_Two <- poststrat(model = "Two", post= post)
Results_Three <- poststrat(model = "Three", post= post)

## Write out xlsx with results on tabs
save(Results_Even, Results_Two, Results_Three, file ="Results/Results.RData")

write.xlsx(as.data.frame(Results_Even), file = "Results/Results.xlsx",
           sheetName = "Even",
           append = FALSE,
           row.names = FALSE)

write.xlsx(as.data.frame(Results_Two), file = "Results/Results.xlsx",
           sheetName = "Two",
           append = TRUE,
           row.names = FALSE)

write.xlsx(as.data.frame(Results_Three), file = "Results/Results.xlsx",
           sheetName = "Three",
           append = TRUE,
           row.names = FALSE)

## Merge into one frame
## Here I also append the sample distribution groupins

## Get State groups for 2:1 and 3:2:1 distributions
load("Data/sample_Even.RData")
G <- sample_Even %>%
  group_by(ST) %>%
  summarise(G3 = unique(Groups_3),
            G2 = unique(Groups_2))
rm(sample_Even)

## Need to create a variable for each frame with 
## Model reference 
Results_Even$Model <- "Even"
Results_Two$Model <- "Two"
Results_Three$Model <- "Three"

## Bind frames together
results <- rbind(Results_Even, Results_Two)
results <- rbind(results, Results_Three)
results <- left_join(results, G, by = "ST") ## Append distribution groupings
rm(Results_Even, Results_Two, Results_Three)

## save down
save(results, file = "Results/Results.RData")

################ End ######################
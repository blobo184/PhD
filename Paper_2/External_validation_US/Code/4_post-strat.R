## Poststratification prep

## Prepare poststrat sheet
load("Data/ACS.rdata")
post <- ACS %>%
  dplyr::filter(ST != "DC")
rm(ACS)

# Append Region and past vote
load("Data/sample_Even.RData")
appends <- sample_Even %>%
  group_by(ST) %>%
  summarise(Region = unique(Region),
            Rep_2012 = unique(Rep_2012))

post <- left_join(post, appends, by = "ST") 
rm(appends)

# Create row weight
post <- post %>%
  group_by(ST) %>%
  mutate(total_pop = sum(pop)) %>%
  ungroup() %>%
  mutate(w = pop/total_pop)

## Append true Rep vote share
load("Data/Pres_All.rdata")
Past_vote <- x %>%
  dplyr::filter(year == 2016) %>%
  dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
  dplyr::select(state_po, party, candidatevotes, totalvotes) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  dplyr::select(state_po, republican, democrat, totalvotes) %>%
  mutate(republican = as.numeric(as.character(republican)),
         democrat = as.numeric(as.character(democrat)),
         Rep = republican / (republican+democrat)) %>%
  dplyr::select(state_po, Rep) %>%
  dplyr::filter(state_po != "DC")

turnout <- read.csv("Data/2016_turnout.csv")
turnout <- turnout %>%
  dplyr::filter(State.Initials != "DC")
Past_vote <- left_join(Past_vote, turnout, by =c("state_po" = "State.Initials"))
Past_vote <- Past_vote %>%
  mutate(Rep = Rep * Turnout) %>%
  dplyr::select(ST = state_po, True_Rep = Rep, Turnout)
post <- left_join(post, Past_vote, by = "ST")
rm(x, Past_vote, turnout)

## Create campaign week variable
post$campaign_week <- 1

## Save
save(post, file = "Data/poststrat.rdata")


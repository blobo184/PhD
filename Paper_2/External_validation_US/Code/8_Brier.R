## Brier score
Brier_poststrat <- function(model) {
  
  mod <- if(model == "Even"){
    readRDS(file = "Results/Model_Even.RDS")} else if(model == "Two"){
      readRDS(file = "Results/Model_Two.RDS")} else {
        readRDS(file = "Results/Model_Three.RDS")}
  
  load("Data/poststrat.RData")   
  out <- rstanarm::posterior_epred(mod, 
                                   newdata = post,
                                   draws = 500)
  
  
  out.m <- reshape2::melt(out, varnames = c("iter", "post_row"))
  out.m$w <- post$w[out.m$post_row]
  out.m$ST <- post$ST[out.m$post_row]
  out.m$Turnout <- post$Turnout[out.m$post_row]
  out.m$True_Rep <- post$True_Rep[out.m$post_row]
  
  results <- out.m %>%
    dplyr::group_by(ST, iter) %>%
    summarise(Pred = (sum(value*w)),
              True_Rep = unique(True_Rep),
              Turnout = unique(Turnout)) %>%
    ungroup() %>%
    mutate(Pred = Pred * Turnout)
  
    ## Need to get 2016 Dem vote for each State
  load("Data/Pres_All.rdata")
  Past_vote <- x %>%
    dplyr::filter(year == 2016) %>%
    dplyr::filter(writein == "FALSE") %>%  # Exclude party writein votes
    dplyr::select(state_po, party, candidatevotes, totalvotes,year) %>%
    mutate(party = case_when(
      party == "democratic-farmer-labor" ~ "democrat",
      party == "democrat" ~ "democrat",
      party == "republican" ~ "republican")) %>%
    pivot_wider(names_from = party, values_from = candidatevotes) %>%
    dplyr::select(state_po, republican, democrat) %>%
    mutate(republican = as.numeric(as.character(republican)),
           democrat = as.numeric(as.character(democrat)),
           Rep = republican / (republican+democrat),
           Dem = democrat / (republican+democrat)) %>%
    dplyr::select(state_po, Dem) %>%
    dplyr::filter(state_po != "DC")
  rm(x)
  
  ## Apply turnout
  turnout <- read.csv("Data/2016_turnout.csv") %>%
    dplyr::filter(!is.na(Turnout) & State.Initials != "DC")
  Past_vote <- left_join(Past_vote, turnout, by = c("state_po"="State.Initials")) %>%
    mutate(Dem = Dem * Turnout) %>%
    dplyr::select(-Turnout) 
  rm(turnout)
  
  ## Append to results by iter
  results.exp <- left_join(results, Past_vote, by = c("ST" = "state_po")) %>%
    mutate(Rep_win = ifelse(True_Rep > Dem, 1, 0),
           Pr_win = ifelse(Pred > Dem, 1, 0)) %>%
    group_by(ST) %>%
    summarise(Pred = mean(Pred),
              True_Rep = unique(True_Rep),
              Dem = unique(Dem),
              Rep_win = unique(Rep_win),
              Pr_win = mean(Pr_win)) %>%
    mutate(Brier = (Rep_win - Pr_win)^2)
  results.exp
}

Brier_Even <- Brier_poststrat("Even")
Brier_Two <- Brier_poststrat("Two")
Brier_Three <- Brier_poststrat("Three")

Brier_Even$Model <- "Even"
Brier_Two$Model <- "Two"
Brier_Three$Model <- "Three"
Brier <- rbind(Brier_Even, Brier_Two)
Brier <- rbind(Brier, Brier_Three)

load("Data/Sample_even.Rdata")
G <- sample_Even %>%
  group_by(ST) %>%
  summarise(G2 = unique(Groups_2),
            G3 = unique(Groups_3))
rm(sample_Even)
Brier <- left_join(Brier, G, by = "ST")
save(Brier, file = "Results/Brier.RData")

Brier %>%
  group_by(Model) %>%
  summarise(Brier = mean(Brier))

Brier %>%
  dplyr::filter(Model != "Two") %>%
  group_by(Model, G3) %>%
  summarise(Brier = round(mean(Brier),3))

Brier %>%
  dplyr::filter(Model != "Three")%>%
  group_by(Model, G2) %>%
  summarise(Brier = round(mean(Brier),3))
list.files("Results/")

############
## Let's see what the seats would look like
## This is excluding DC and 5 CD seats.
ec <- read.csv("Data/ec_votes.csv")
ec <- ec %>%
  dplyr::select(-Notes)

Brier <- left_join(Brier, ec, by = c("ST" = "Unit"))
rm(ec)


Brier %>%
  mutate(Pr_dem = 1-Pr_win) %>%
  group_by(Model) %>%
  summarise(Rep = sum(Pr_win*ECVotes),
            Dem = sum(Pr_dem*ECVotes))



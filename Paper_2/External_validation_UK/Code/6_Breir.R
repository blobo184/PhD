## Script to get results with appended Breier score. 
## Assuming woking directory is set to UK folder

## Load poststrat frame
load("Data/Post_strat.RData")

## Function to poststrat results, 
## append max vote % for all other parties
## calcualte if Con win for each iter
## Calculate breir score for each seat

Brier_reuslts <- function(Model) {
  
## Load model either Even, Two or Three  
  mod <- if (Model == "Even"){ 
      readRDS("Results/Even.mod.rds")
    } else if (Model == "Two") {
      readRDS("Results/Two.mod.rds")
    } else {readRDS("Results/Three.mod.rds")}
    
  ## Poststrat results with 500 posterior draws
    out<- rstanarm::posterior_epred(mod,
                                  newdata=post,
                                  draws = 500)
  ## Melt out posterior draws by iter 
  ## And append other vars
    out.m <- melt(out, varnames = c("iter", "post_row"))
    out.m$weight <- post$weight[out.m$post_row]
    out.m$GSSCode <- post$GSSCode[out.m$post_row]
    out.m$turnout <- post$Turnout[out.m$post_row]
    out.m$Con_2019 <- post$Con_2019[out.m$post_row]
  
    ## Calculate preds for each iter
    results <- out.m %>%
    group_by(GSSCode, iter) %>%
    summarise(Pred = sum((value*turnout)*weight)) ## Also applying weight
    
    ## Read in csv with max vote % for all other parties
    ## and seat winner
    winner <- read.csv("Data/Winner_2019.csv") %>%
      rename(GSSCode = ons_id)
    
    ## Join with results frame
    results <- left_join(winner, results, by = "GSSCode")
    
    ## Group by GSSCode and summarise results for final frame
    results <- results %>%
      dplyr::filter(GSSCode != "E14000637") %>% ## remove spk seat
      mutate(Pr_win = ifelse(Pred > Other_max, 1, 0)) %>%
      group_by(GSSCode) %>%
      summarise(Pr = mean(Pr_win), ## Pr of Con win
                Winner = unique(Winner), # Actual winner
                Other_max = unique(Other_max), # Max Other vote %
                Con_mean = mean(Pred), # Mean Con est.
                Con_Hi = quantile(Pred, 0.95), # 95% CI est.
                Con_lo = quantile(Pred, 0.05)) %>% # 5$ CI est.
      mutate(Winner = ifelse(Winner == "Con", 1, 0),
             Br = (Pr - Winner)^2) # Calculate seat breier score
}

## Create frames for each sample distribution
Three <- Brier_reuslts("Three")
Two <- Brier_reuslts("Two")
Even <- Brier_reuslts("Even")

## Breir scores
Model_Brier <- left_join(Even, Two, by = "GSSCode") %>%
  left_join(., Three, by = "GSSCode") %>%
  dplyr::select(GSSCode, Even_Pr = Pr.x, Even_Br = Br.x, 
                Two_Pr = Pr.y, Two_Br = Br.y, 
                Three_Pr = Pr, Three_Br = Br) %>%
  summarise(Even_seats = sum(Even_Pr),
            Even_Br = sum(Even_Br)/631,
            Two_seats = sum(Two_Pr),
            Two_Br = sum(Two_Br)/631,
            Three_seats = sum(Three_Pr),
            Three_Br = sum(Three_Br)/631)


save(Even, Two, Three, Model_Brier, file = "Results/Brier.RData")

#################### End ##################

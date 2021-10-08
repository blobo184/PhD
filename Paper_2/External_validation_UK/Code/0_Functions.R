## Functions

################### Functions for script 5 ####################
## Function to gather accuracy figures from excel Accuracy_master.
gather_accuracy <- function(path) {
  require(readxl)
  require(tidyverse)
  paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:3) {
    holder[[i]] <- read_excel(path, range = "B1:G2",
                              col_names = TRUE, sheet = paths[[i]])
    holder[[i]]$Model <- paths[[i]]
  }
  dat <- do.call(rbind, holder)
  dat <- dat %>%
    relocate(Model)
  dat$order <- 1:3
  
  dat <- dat %>%
    mutate(Design = dplyr::case_when(
      grepl("Even", Model) ~ "Even",
      grepl("Two", Model) ~ "Two groups",
      grepl("Three", Model) ~ "Three groups"))
}

## Function to gather estimates and calculate correlation. 
get_correl <- function(path) {
  
  paths <- path %>% 
    excel_sheets()
  ## Read in estimates from results_master excel sheet.
  holder <- list()
  for (i in 1:3) {
    holder[[i]] <- read_excel(path, range = "B1:C633",
                              col_names = TRUE, sheet = paths[[i]])
    names(holder[[i]])[2] <- paths[[i]]
    
  }
  
  correl <- do.call(cbind, holder)
  correl <- correl[c(1,2,4,6)]
  
  ## Read in results and calcultae correlation
  results <- read.csv("~/Paper 2/Research/External validation/UK/Data/Results.csv")
  results <- results %>%
    dplyr::select(GSSCode, Con_2019) # select 2019 Conservative vote sahre
  
  
  correl <- left_join(correl, results, by = "GSSCode") # merge with estimates
  correl <- correl %>% # Calculate correl for each model 
    summarise(Even = cor(Even, Con_2019),
              Two = cor(Two, Con_2019),
              Three = cor(Three, Con_2019)) 
  
  correl <- correl %>% ## pivot wide to can merge with accuracy data
    pivot_longer(cols = c(Even, Two, Three))%>%
    rename(Model=name, Cor=value)
  
  correl
  
}

## Function to plot MAE, RMSE and Correl
overall.accuracy <- function(measure) {
  if (measure == "MAE") {
    plot <- Accuracy %>%
      ggplot(aes(x = reorder(Model,-MAE), y = MAE, color = Design)) +
      geom_point(size = 3) + coord_flip() +
      geom_errorbar(aes(ymin = MAE.Low, ymax = MAE.High), width = 0.1) +
      labs(y = "MAE",
           x = "") + 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "left")
  } else if (measure == "RMSE") {
    plot <- Accuracy %>%
      ggplot(aes(x = reorder(Model,-MAE), y = RMSE, color = Design)) +
      geom_point(size = 3) + coord_flip() +
      geom_errorbar(aes(ymin = RMSE.Low, ymax = RMSE.High), width = 0.1) +
      labs(y = "RMSE",
           x = "") + 
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none")
  } else {
    plot<- Accuracy %>%
      ggplot(aes(x = reorder(Model, -MAE), y = Cor, color = Design)) +
      geom_point(size = 3) + coord_flip() +
      labs(y = "Cor",
           x = "") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
      theme(
        axis.text.y = element_blank(),
        legend.position = "none")}
}

## Function to gather MAE and width and append small area groups
gather_MAE_and_width <- function(path) {
  require(readxl)
  require(tidyverse)
  ## First collect groups by gsscode  
  
  load("~/Paper 2/Research/External validation/UK/Data/Bes_Even.RData")
  groups <- dat %>%
    dplyr::select(GSSCode, Groups3_17, Groups2_17) %>%
    group_by(GSSCode) %>%
    summarise(Groups3_17 = unique(Groups3_17),
              Groups2_17 = unique(Groups2_17)) %>%
    ungroup()
    
  paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:3) {
    holder[[i]] <- read_excel(path, range = "B1:f633",
                              col_names = TRUE, sheet = paths[[i]])
    holder[[i]]$Width <- holder[[i]]$Pred_hi - holder[[i]]$Pred_lo
    holder[[i]] <- holder[[i]][c(1,5,6)]
    holder[[i]]$Model <- paths[[i]]
  }
  
  dat <- do.call("rbind", holder)
  dat <- left_join(dat, groups, by = "GSSCode")
  
}

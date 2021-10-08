## Script to gather accuracy figures and plot into graph
source("Code/0_Functions.R")


## Gather accuracy
Accuracy <- gather_accuracy(path = "Results/Accuracy_master.xlsx")

## Calculate and append correlation
correl <- get_correl(path = "Results/Results_master.xlsx")

## Combine accuracy and correl
Accuracy <- left_join(Accuracy, correl, by = "Model")
rm(correl)


##  Creat plots and combine into one graph
MAE <- overall.accuracy(measure = "MAE")
RMSE <- overall.accuracy(measure = "RMSE")
Cor <- overall.accuracy(measure = "Cor")

# Combine
Acc_plot <- ggarrange(MAE, RMSE, Cor,
                  nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom")
Acc_plot <- annotate_figure(Acc_plot,
                        top = text_grob("Accuracy of models"))
ggsave(Acc_plot, file = "Results/Plots/Acc_plots.png") # Save


##### Now let's create tables for MAE and Width by small area group

## Use function to gather MAE and Width for all small areas
## With small area groups appended. 
dat <- gather_MAE_and_width(path = "Results/Results_master.xlsx")

## Create table for Even and Three comparison
Three <- dat %>%
  dplyr::filter(Model != "Two") %>%
  group_by(Groups3_17, Model) %>%
  summarise(MAE = mean(MAE.temp),
            Width = mean(Width))

## Create table for Two and Even comparison
Two <- dat %>%
    dplyr::filter(Model != "Three") %>%
  group_by(Groups2_17, Model) %>%
    summarise(MAE = mean(MAE.temp),
              Width = mean(Width))

## Write out to xlsx sheet in Results folder
write.xlsx(as.data.frame(Three), file = "Results/MAE_&_width.xlsx",
           append = F, sheetName = "Three", row.names = F)
write.xlsx(as.data.frame(Two), file = "Results/MAE_&_width.xlsx",
           append = T, sheetName = "Two", row.names = F)


###################### End #############################
## Script to create three scatter plots with accuracy figures as subtitle.

## First gather accuracy figures and create one frame
## Script to gather accuracy figures and plot into graph
source("~/Paper 2/Research/External validation/UK/Code/0_Functions.R")

## Gather accuracy
Accuracy <- gather_accuracy(path = "~/Paper 2/Research/External validation/UK/Results/Accuracy_master.xlsx")

## Calculate and append correlation
correl <- get_correl(path = "~/Paper 2/Research/External validation/UK/Results/Results_master.xlsx")

## Combine accuracy and correl
Accuracy <- left_join(Accuracy, correl, by = "Model")
rm(correl)

## Gather estiamtes at small area level
path <- "~/Paper 2/Research/External validation/UK/Results/Results_master.xlsx"

## Gather all estimates for each model (Even, 2:1, 3:2:1)  
paths <- path %>% 
  excel_sheets()
holder <- list()
  for (i in 1:3) {
  holder[[i]] <- read_excel(path, range = "B1:E633",
                            col_names = TRUE, sheet = paths[[i]])
  holder[[i]]$Model <- paths[[i]]
  
  }

dat <- do.call(rbind, holder) ## Bind into one frame

## Read in and append actual Con 2019 results
results <- read.csv("~/Paper 2/Research/External validation/UK/Data/Results.csv")
results <- results %>%
  dplyr::select(GSSCode, Con_2019) # select 2019 Conservative vote sahre
results <- left_join(dat, results, by = "GSSCode")
rm(dat)

## Create accuracy labels
C1 <- paste0("MAE:",round(Accuracy$MAE[1]*100,1),"%",
             "\nRMSE:",round(Accuracy$RMSE[1]*100,1), "%",
             "\nCor:",round(Accuracy$Cor[1]*100,1), "%")
C2 <- paste0("MAE:",round(Accuracy$MAE[2]*100,1), "%",
             "\nRMSE:",round(Accuracy$RMSE[2]*100,1),"%",
             "\nCor:",round(Accuracy$Cor[2]*100,1),"%")
C3 <- paste0("MAE:",round(Accuracy$MAE[3]*100,1),"%",
             "\nRMSE:",round(Accuracy$RMSE[3]*100,1),"%",
             "\nCor:",round(Accuracy$Cor[3]*100,1),"%")

## Save results and accuracy labels to use in Rmarkdown.
save(results, C1, C2, C3, file = "~/Paper 2/Research/External validation/UK/Results/scatter_data.RData")


p1 <- results %>%
  dplyr::filter(GSSCode != "E14000637") %>% ## exclude spk seat
  mutate(Model = case_when(
    Model == "Even" ~ "Even",
    Model == "Three" ~ "3:2:1",
    Model == "Two" ~ "2:1"))%>%
  ggplot(aes(x = Con_2019, y = Pred)) + 
  geom_point(size = 1, alpha= 4/10) +
  geom_errorbar(aes(ymin = Pred_lo, ymax=Pred_hi),
                width = 1, alpha = 1/10) + 
  geom_smooth(method = "lm", color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "grey",
              linetype = "dashed") +
  labs(y="",
       x="") + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.0,0.55)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.0,0.55)) +
    facet_grid(~Model) + 
  geom_text(aes(x, y, label= lab),
            data = data.frame(x=.15, y = Inf, lab = c(C1, C2, C3),
                              Model = c("Even", "3:2:1", "2:1")), vjust=1)
p1
ggsave(p1, file= "~/Paper 2/Research/External validation/UK/Results/Plots/Accuracy.png")

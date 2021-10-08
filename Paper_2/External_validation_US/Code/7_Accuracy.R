## Plots and tables
library(ggpubr)


load("~/Paper 2/Research/External validation/US/Results/Results.RData")
load("~/Paper 2/Research/External validation/US/Data/poststrat.RData")
post <- post %>%
  group_by(ST) %>%
  summarise(Truth = unique(True_Rep))
results <- left_join(results, post, by = "ST")
rm(post)

## Scatter accuracy plot
# Need to get accuracy figures to put onto the plots
acc <- results %>%
  group_by(Model) %>%
  mutate(MAE = round(mean(MAE)*100,1),
         RMSE = round(mean(RMSE)*100,1),
         Cor = round(cor(Pred,Truth)*100,1)) %>%
  ungroup() %>%
  group_by(Model) %>%
  summarise(MAE = unique(MAE),
            RMSE = unique(RMSE),
            Cor = unique(Cor))

C1 <- paste0("MAE:",acc[1,2],"%",
             "\nRMSE:",acc[1,3],"%",
             "\nCor:",acc[1,4],"%")

C2 <- paste0("MAE:",acc[2,2],"%",
             "\nRMSE:",acc[2,3],"%",
             "\nCor:",acc[2,4],"%")

C3 <- paste0("MAE:",acc[3,2],"%",
             "\nRMSE:",acc[3,3],"%",
             "\nCor:",acc[3,4],"%")

## Plot
p1 <- results %>%
  mutate(Model = case_when(
    Model == "Two" ~ "2:1",
    Model == "Three" ~ "3:2:1",
    Model == "Even" ~ "Even")) %>%
  ggplot(aes(x = Truth, y = Pred)) + 
  geom_point(size = 1, alpha= 4/10) +
  geom_errorbar(aes(ymin = Pred_lo, ymax=Pred_hi),
                width = 1, alpha = 3/10) + 
  geom_smooth(method = "lm", color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "grey",
              linetype = "dashed") +
  labs(y="",
       x="") + 
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.1,0.5)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.1,0.5)) +
  facet_wrap(~Model) +
  geom_text(aes(x, y, label= lab),
            data = data.frame(x=0.15, y = Inf, lab = c(C1, C2, C3),
                              Model = c("Even", "3:2:1", "2:1")), vjust=1) 
p1  
ggsave(p1, file= "~/Paper 2/Research/External validation/US/Results/Plots/Accuracy.png")

## Save down data and function to plot.
## I'll use this later in the Rmarkdown. 
save(results, C1, C2, C3, file = "~/Paper 2/Research/External validation/US/Results/scatter_data.RData")

## Tables with accuracy by distribution grouping.

results %>%
  dplyr::filter(Model != "Two") %>%
  mutate(Width = Pred_hi - Pred_lo) %>%
  group_by(Model, G2) %>%
  summarise(MAE = round(mean(MAE),2), 
            Width = round(mean(Width),2))

results %>%
  dplyr::filter(Model != "Three") %>%
  mutate(Width = Pred_hi - Pred_lo) %>%
  group_by(Model, G3) %>%
  summarise(MAE = round(mean(MAE),2), 
            Width = round(mean(Width),2))



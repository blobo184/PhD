## Collate accuracy figures and plot into graphs

## Function to first collate figures from excel sheets.
source("~/Paper 2/Research/Code/0_Functions.R")

Areas_50_acc <- gather_accuracy(path = "~/Paper 2//Research/Results/Accuracy_50.xlsx")
Areas_200_acc <- gather_accuracy(path = "~/Paper 2//Research/Results/Accuracy_200.xlsx")
Areas_400_acc <- gather_accuracy(path = "~/Paper 2//Research/Results/Accuracy_400.xlsx")
Areas_600_acc <- gather_accuracy(path = "~/Paper 2//Research/Results/Accuracy_600.xlsx")
save(Areas_50_acc, Areas_200_acc, Areas_400_acc, Areas_600_acc, file = "Results/Accuracy_combined.RData")

## If you've already gathered data and saved files you can load below
load("~/Paper 2/Research/Results/Accuracy_combined.Rdata")
combined_acc <- rbind(Areas_50_acc, Areas_200_acc, Areas_400_acc, Areas_600_acc)

## Plots
plot_MAE <- combined_acc %>%
  mutate(Areas = factor(Areas,
                        levels = c(50, 200, 400, 600),
                        labels = c("50 Areas", "200 Areas", "400 Areas", "600 Areas")),
         Sample = factor(Sample,
                         levels = c(30, 20, 10, 5),
                         labels = c("N = 30", "N = 20", "N = 10", "N = 5")),
         Design =case_when(
           Design == "Even" ~ "Even", 
           Design == "Two groups" ~ "2:1",
           Design == "Three groups" ~ "3:2:1")) %>%
  ggplot(aes(x = Model, y = MAE, color = Design)) + 
  geom_point(size = 1.5) + coord_flip() +
  geom_errorbar(aes(ymin = MAE.Low, ymax = MAE.High), width = 0.1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0.04,.32)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 15),
        legend.position = "bottom") +
  facet_grid(Sample ~ Areas, scales = "free")
plot_MAE
ggsave(plot_MAE, file = "~/Paper 2//Research/Results/Plots/Model_MAE.png")

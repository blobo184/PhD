## Preliminary analysis of results

## set directory
#setwd("C:/Users/benlo/Dropbox (Royal Holloway)/RStudio_AMI/UK/Code")

## Libraries
library(tidyverse)
library(ggplot2)
library(tidybayes)
library(ggthemes)
source("~/Paper 3/Research/UK/Code/00_Utility.R")

## Load results Either UK or US
load("~/Paper 3/Research/UK/Results/Results.RData")

## Create master file
results_sample_5 <- results_sample_5 %>%
  mutate(Sample = 5)
results_sample_10 <- results_sample_10 %>%
  mutate(Sample = 10)
results_sample_20 <- results_sample_20 %>%
  mutate(Sample = 20)
results_sample_30 <- results_sample_30 %>%
  mutate(Sample = 30)

compare <- do.call("rbind", list(results_sample_5, results_sample_10, results_sample_20, results_sample_30))
rm(results_sample_5, results_sample_10, results_sample_20, results_sample_30)

## Fixed effects - Table 1 showing improvements on baseline
Table_1 <- compare %>%
  group_by(Area, Year, Sample) %>%
  mutate(MAE = MAE.temp - MAE.temp[match("weak", Model)]) %>%
  mutate(Type = case_when(
    grepl("inf", Model)=="TRUE" ~ "Random effect",
    grepl("fe", Model)=="TRUE" ~ "Fixed effect",
    grepl("weak", Model)=="TRUE" ~ "Weak"),
    Model = case_when(
      grepl("_a", Model)=="TRUE" ~ "Lambda = 1",
      grepl("_b", Model)=="TRUE" ~ "Lambda = 1.5",    
      grepl("_c", Model)=="TRUE" ~ "Lambda = 2",
      TRUE  ~ "Weakly inf.")) %>%
  dplyr::filter(Type == "Fixed effect") %>%
  group_by(Year, Sample, Model) %>%
  summarise(MAE = round(mean(MAE),3),
            MAE = paste0(MAE*100, "%")) %>%
  ungroup() %>%
  pivot_wider(values_from = MAE, names_from = Sample) %>%
  dplyr::select(-Year)

## Table 3 showing improvement on Baseline
Table_3 <- compare %>%
  group_by(Area, Year, Sample) %>%
  mutate(MAE = MAE.temp - MAE.temp[match("weak", Model)]) %>%
  mutate(Type = case_when(
    grepl("inf", Model)=="TRUE" ~ "Random effect",
    grepl("fe", Model)=="TRUE" ~ "Fixed effect",
    grepl("weak", Model)=="TRUE" ~ "Weak"),
    Model = case_when(
      grepl("_a", Model)=="TRUE" ~ "Lambda = 1",
      grepl("_b", Model)=="TRUE" ~ "Lambda = 1.5",    
      grepl("_c", Model)=="TRUE" ~ "Lambda = 2",
      TRUE  ~ "Weakly inf.")) %>%
  dplyr::filter(Type == "Random effect") %>%
  group_by(Year, Sample, Type, Model) %>%
  summarise(MAE = round(mean(MAE),3),
            MAE = paste0(MAE*100, "%")) %>%
  ungroup() %>%
  pivot_wider(values_from = MAE, names_from = Sample) %>%
  dplyr::select(-Type, -Year)

## Table 5 showing improvement on Baseline of bigger sample
Table_5 <- compare %>%
  mutate(Sample_2 = case_when(
    Sample == 5 ~ 10,
    Sample == 10 ~ 20,
    Sample == 20 ~ 30,
    Sample == 30 ~ NA_real_)) %>%
  group_by(Area, Year, Model) %>%
  mutate(weak_MAE = MAE.temp[match(Sample_2, Sample)]) %>%
  ungroup() %>%
  group_by(Area, Year, Sample) %>%
  mutate(weak_MAE = weak_MAE[match("weak", Model)],
         MAE = MAE.temp - weak_MAE) %>%
  ungroup() %>%
  dplyr::filter(Sample != 30, Model != "weak") %>%
  group_by(Year, Sample, Model) %>%
  summarise(MAE = round(mean(MAE),3),
            MAE = paste0(MAE*100, "%")) %>%
  ungroup() %>%
  mutate(Type = case_when(
    grepl("inf", Model)=="TRUE" ~ "Random effect",
    grepl("fe", Model)=="TRUE" ~ "Fixed effect",
    grepl("weak", Model)=="TRUE" ~ "Weak"),
    Model = case_when(
      grepl("_a", Model)=="TRUE" ~ "Lamba = 1",
      grepl("_b", Model)=="TRUE" ~ "Lamba = 1.5",
      grepl("_c", Model)=="TRUE" ~ "Lamba = 2")) %>%
  dplyr::filter(Type == "Random effect") %>%
  pivot_wider(values_from = MAE, names_from = Sample) %>%
  dplyr::select(-Year)

## I've already run below. If you have not uncomment and run
# ## Gather parameter posteriors
# # List all models saved in results
# files_5 <- list.files("../Results/Sample_5/", full.names = T)
# files_10 <- list.files("../Results/Sample_10/", full.names = T)
# files_20 <- list.files("../Results/Sample_20/", full.names = T)
# files_30 <- list.files("../Results/Sample_30/", full.names = T)
# 
# # Exclude base models and back-up files
# files_5 <- files_5[grepl(c("inf|weak"), files_5)==TRUE]
# files_10 <- files_10[grepl(c("inf|weak"), files_10)==TRUE]
# files_20 <- files_20[grepl(c("inf|weak"), files_20)==TRUE]
# files_30 <- files_30[grepl(c("inf|weak"), files_30)==TRUE]
# 
# # Gather and combined posteriors with function
# posteriors_5 <- extract_posteriors(paths = files_5) 
# posteriors_10 <- extract_posteriors(paths = files_10)  
# posteriors_20 <- extract_posteriors(paths = files_20)  
# posteriors_30 <- extract_posteriors(paths = files_30)  
# 
# total <- do.call("rbind", list(posteriors_5, posteriors_10, posteriors_20, posteriors_30))
# rm(posteriors_5, posteriors_10, posteriors_20, posteriors_30)
# save(total, file = "~/Paper 3/Research/UK/Results/posterior_draws.RData")
load(file = "~/Paper 3/Research/UK/Results/posterior_draws.RData")

## Plot showing posterior differences
plot_1 <- total %>%
  dplyr::filter(Variable == "Age", Year == 2017, Sample == "30") %>%
  mutate(Lab = exp(Effect)/ (1+exp(Effect)),
         Lambda = case_when(
           Lambda == 1 ~ "Lambda = 1",
           Lambda == 1.5 ~ "Lambda = 1.5",
           Lambda == 2 ~ "Lambda = 2",
           TRUE ~ "Baseline")) %>%
  group_by(Lambda, condition) %>%
  summarise(Pred = median(Lab),
            Pred_lo = quantile(Lab, 0.05),
            Pred_hi = quantile(Lab, 0.95),
            Width = abs(round(Pred_hi - Pred_lo,2)),
            Y = 0.5) %>%
  ungroup() %>%
  group_by(condition) %>%
  mutate(Base = Pred[match("Baseline", Lambda)]) %>%
  ungroup() %>%
  ggplot(aes(x = Pred, y = Y)) +
  geom_point() + geom_errorbar(aes(xmin = Pred_lo, xmax = Pred_hi), width = 0.1) +
  geom_vline(aes(xintercept = Base), linetype = "dashed", color = "red", alpha = 1/3) +
  geom_text(aes(label = Width), size = 4, x = -Inf, y = Inf, hjust = 0, vjust = 1) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank())+
  ylim(0,1)+
  xlab("Marginal effects for Age categories") +
  facet_grid(Lambda~condition)
plot_1
## Table_7: Computational efficiency as measured by model run time
load("~/Paper 3/Research/UK/Results/UK_model_times.RData")
Table_7 <- UK_model_times %>%
  dplyr::select(-Chain) %>%
  mutate(Lambda = case_when(
    grepl("_a", Model)==TRUE ~ 1,
    grepl("_b", Model)==TRUE ~ 1.5,
    grepl("_c", Model)==TRUE ~ 2,
    TRUE ~ NA_real_),
    Year = case_when(
      grepl("_2012", Model)==TRUE ~ 2012,
      grepl("_2016", Model)==TRUE ~ 2016,
      grepl("_2017", Model)==TRUE ~ 2017,
      grepl("_2019", Model)==TRUE ~ 2019,
      TRUE ~ NA_real_),
    Model = case_when(
      grepl("inf_", Model)==TRUE ~ "Random",
      grepl("fe_", Model)==TRUE ~ "Fixed",
      grepl("weak", Model)==TRUE ~ "Weakly inf.",
      TRUE ~ NA_character_)) %>%
  group_by(Year, Sample) %>%
  mutate(Time_diff = (Total - Total[match("Weakly inf.", Model)])/ Total[match("Weakly inf.", Model)],
         Time_diff = round(Time_diff, 2)) %>%
  ungroup() %>%
  dplyr::filter(Model != "Weakly inf.") %>%
  group_by(Sample, Year, Lambda) %>%
  summarise(Random = Time_diff[match("Random", Model)],
            Fixed = Time_diff[match("Fixed", Model)]) %>%
  ungroup() %>%
  pivot_wider(names_from = Sample, values_from = c(Random, Fixed)) %>%
  dplyr::select(Year, Model = Lambda, Random_5, Random_10, Random_20, Random_30,
                Fixed_5, Fixed_10, Fixed_20, Fixed_30)

## Plot showing average width across constituencies
## Currently set on 2019 election
plot_3 <- compare %>%
  mutate(Width = Pred_hi - Pred_lo,
         Lambda = case_when(
           Model == "weak" ~ "Baseline",
           Model == "inf_a" ~ "Lambda = 1",
           Model == "inf_b" ~ "Lambda = 1.5",
           Model == "inf_c" ~ "Lambda = 2")) %>%
  dplyr::filter(grepl(c("inf|weak"), Model), Year == 2019) %>% 
  group_by(Lambda, Sample) %>%
  mutate(width_label = round(median(Width),2)) %>%
  ungroup() %>%
  ggplot(aes(x = Width, colour = Lambda)) +
  stat_pointinterval(.width = c(0.25, 0.75)) +
  theme_igray() +
  scale_x_continuous(
    labels = scales::number_format(accuracy = 0.01)) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(size = 8)) +
  geom_text(aes(label = width_label), size = 4, x = -Inf, y = Inf, hjust = 0, vjust = 1) +
  facet_grid(Lambda~Sample)

## Save all tables
save(Table_1, Table_3, Table_5, Table_7, plot_1, plot_3, file = "~/Paper 3/Write-up/Figures/UK.RData")


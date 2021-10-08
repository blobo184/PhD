## Poststratification


## Read in psotstrat sheet and merge some categories
post <- read.csv("Data/Post.csv") %>%
  group_by(GSSCode, age0, education, sex) %>%
    summarise(weight = sum(weight))
turnout <- rio::import("Data/Results.csv", format = ",") %>%
  dplyr::select(GSSCode, Turnout, Con_2019)
post <- left_join(post, turnout, by = "GSSCode")

## Append area-level variables used
load("Data/Bes_Even.RData")
dat <- dat %>%
  dplyr::select(GSSCode, Con_GE2017, Leave_2016, Long_term_unemployed, 
                Population_density, Industry_manufacturing, Region) %>%  
  mutate(Con_GE2017 = arm::rescale(Con_GE2017),
         Leave_2016 = arm::rescale(Leave_2016),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing)) %>%
  group_by(GSSCode) %>%
  summarise(Con_GE2017 = unique(Con_GE2017),
            Leave_2016 = unique(Leave_2016),
            Long_term_unemployed = unique(Long_term_unemployed),
            Population_density = unique(Population_density),
            Industry_manufacturing = unique(Industry_manufacturing),
            Region = unique(Region))

post <- left_join(post,dat, by = "GSSCode")
rm(dat)

## Add in campaign week as most recent in model
post <- post %>%
  mutate(Campaign_week = 4)

## Save final poststrat down.
save(post, file = "Data/Post_strat.RData")

## poststrat
load("Data/Post_strat.RData")

post_strat <- function(Model){
  mod <- if (Model == "Even"){
    readRDS("Results/Even.mod.rds")
  } else if (Model == "Two") {
    readRDS("Results/Two.mod.rds")
  } else {readRDS("Results/Three.mod.rds")}
  
  out<- rstanarm::posterior_epred(mod,
                                newdata=post,
                                draws = 500)
  out.m <- melt(out, varnames = c("iter", "post_row"))
  out.m$weight <- post$weight[out.m$post_row]
  out.m$GSSCode <- post$GSSCode[out.m$post_row]
  out.m$turnout <- post$Turnout[out.m$post_row]
  out.m$Con_2019 <- post$Con_2019[out.m$post_row]

  results <- out.m %>%
  group_by(GSSCode, iter) %>%
  summarise(temp.Pred = sum((value*turnout)*weight),
            Con_2019 = unique(Con_2019),
            err = (temp.Pred - Con_2019))

  result.export <- results %>%
  group_by(GSSCode) %>%
  summarise(Pred = mean(temp.Pred),
            Pred_lo = quantile(temp.Pred, 0.05),
            Pred_hi = quantile(temp.Pred, 0.95),
            MAE.temp = mean(abs(err)),
            RMSE.temp = sqrt(mean(err^2)))


  if (file.exists("Results/Results_master.xlsx") == "FALSE"){
  write.xlsx(result.export, file = "Results/Results_master.xlsx",
             append = F,
             sheetName = Model)} else {
               write.xlsx(result.export, file = "Results/Results_master.xlsx",
                          append = T,
                          sheetName = Model)}


  Accuracy <- result.export %>%
  dplyr::filter(GSSCode != "E14000637") %>% ## exclude speakers seat
  summarise(MAE = mean(MAE.temp),
            MAE.Low = quantile(MAE.temp,0.05),
            MAE.High = quantile(MAE.temp,0.95),
            RMSE = mean(RMSE.temp),
            RMSE.Low = quantile(RMSE.temp,0.05),
            RMSE.High = quantile(RMSE.temp,0.95))


  if (file.exists("Results/Accuracy_master.xlsx") == "FALSE"){
    write.xlsx(Accuracy, file = "Results/Accuracy_master.xlsx",
               append = F,
               sheetName = Model)} else {
                 write.xlsx(Accuracy, file = "Results/Accuracy_master.xlsx",
                            append = T,
                            sheetName = Model)}
}
  
post_strat(Model = "Even")
post_strat(Model = "Two")
post_strat(Model = "Three")

##### End
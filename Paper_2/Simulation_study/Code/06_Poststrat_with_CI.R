## Poststratification
# Function to poststratify model to create estimates. 
# Funciton writes estiamtes and accuracy to excel sheets.


results <- function(model_path, post_path, tabname){

  model <- readRDS(model_path)
  post <- local({
    load(post_path)
    stopifnot(length(ls())==1)
    environment()[[ls()]]
  })
    post <- post %>%
  dplyr::select(GSSCode, X1, X2, X3, X4, A1, A2, A3, weight, Vote = Y)


    out<- rstanarm::posterior_epred(model,
                                  newdata=post,
                                  draws = 500)

    out.m <- melt(out, varnames = c("iter", "post_row"))
    out.m$weight <- post$weight[out.m$post_row]
    out.m$GSSCode <- post$GSSCode[out.m$post_row]
    out.m$Vote <- post$Vote[out.m$post_row]

    results <- out.m %>%
      group_by(GSSCode, iter) %>%
      summarise(temp.Pred = sum(value*weight),
                True.value = sum(Vote*weight),
                err = (temp.Pred-True.value))

    result.export <- results %>%
      group_by(GSSCode) %>%
      summarise(Pred = mean(temp.Pred),
              low.Pred = quantile(temp.Pred, 0.05),
              hi.Pred = quantile(temp.Pred, 0.95),
              True.value = mean(True.value),
              MAE.temp = mean(abs(err)),
              RMSE.temp = sqrt(mean(err^2)),
              Pred.median = median(temp.Pred))

    excel_path <- if (str_detect(model_path, "50") == "TRUE") {
      "Results/Estimates_50.xlsx"
    } else if (str_detect(model_path, "200") == "TRUE") {
      "Results/Estimates_200.xlsx"
    } else if (str_detect(model_path, "400") == "TRUE") {
      "Results/Estimates_400.xlsx"
    } else {"Results/Estimates_600.xlsx"}

    if (file.exists(excel_path) == "FALSE"){
      write.xlsx(result.export, file = excel_path, sheetName = tabname)   
    } else {
      write.xlsx(result.export, file = excel_path, sheetName = tabname, append = TRUE)    
    }
    
    Accuracy <- result.export %>%
      summarise(MAE = mean(MAE.temp),
                MAE.Low = quantile(MAE.temp,0.05),
                MAE.High = quantile(MAE.temp,0.95),
                RMSE = mean(RMSE.temp),
                RMSE.Low = quantile(RMSE.temp,0.05),
                RMSE.High = quantile(RMSE.temp,0.95))
    
    excel_path2 <- if (str_detect(model_path, "50") == "TRUE") {
      "Results/Accuracy_50.xlsx"
    } else if (str_detect(model_path, "200") == "TRUE") {
      "Results/Accuracy_200.xlsx"
    } else if (str_detect(model_path, "400") == "TRUE") {
      "Results/Accuracy_400.xlsx"
    } else {"Results/Accuracy_600.xlsx"}
    
    if (file.exists(excel_path2) == "FALSE"){
      write.xlsx(Accuracy, file = excel_path2, sheetName = tabname)   
    } else {
      write.xlsx(Accuracy, file = excel_path2, sheetName = tabname, append = TRUE)    
    }
      
}


## 50 areas
# Models with (avg) 30 respondents per areas
results(model_path = "Results/Models/Areas_50/Model_30_Even.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M30_E")

results(model_path = "Results/Models/Areas_50/Model_30_Two.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M30_2")

results(model_path = "Results/Models/Areas_50/Model_30_Three.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M30_3")

## Models with (avg) 20 respondents per area
results(model_path = "Results/Models/Areas_50/Model_20_Even.rds",
          post_path = "Data/dat_50_areas.RData",
          tabname = "M20_E")
results(model_path = "Results/Models/Areas_50/Model_20_Two.rds",
          post_path = "Data/dat_50_areas.RData",
          tabname = "M20_2")
results(model_path = "Results/Models/Areas_50/Model_20_Three.rds",
          post_path = "Data/dat_50_areas.RData",
          tabname = "M20_3")

# Models with (avg) 10 respondents per areas
results(model_path = "Results/Models/Areas_50/Model_10_Even.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M10_E")
results(model_path = "Results/Models/Areas_50/Model_10_Two.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M10_2")
results(model_path = "Results/Models/Areas_50/Model_10_Three.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M10_3")

# Models with (avg) 5 respondents per areas
results(model_path = "Results/Models/Areas_50/Model_5_Even.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M5_E")
results(model_path = "Results/Models/Areas_50/Model_5_Two.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M5_2")
results(model_path = "Results/Models/Areas_50/Model_5_Three.rds",
        post_path = "Data/dat_50_areas.RData",
        tabname = "M5_3")

## 200 Areas
# Models with (avg) 30 respondents per areas
results(model_path = "Results/Models/Areas_200/Model_30_Even.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M30_E")

results(model_path = "Results/Models/Areas_200/Model_30_Two.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M30_2")

results(model_path = "Results/Models/Areas_200/Model_30_Three.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M30_3")

## Models with (avg) 20 respondents per area
results(model_path = "Results/Models/Areas_200/Model_20_Even.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M20_E")
results(model_path = "Results/Models/Areas_200/Model_20_Two.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M20_2")
results(model_path = "Results/Models/Areas_200/Model_20_Three.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M20_3")

# Models with (avg) 10 respondents per areas
results(model_path = "Results/Models/Areas_200/Model_10_Even.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M10_E")
results(model_path = "Results/Models/Areas_200/Model_10_Two.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M10_2")
results(model_path = "Results/Models/Areas_200/Model_10_Three.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M10_3")

# Models with (avg) 5 respondents per areas
results(model_path = "Results/Models/Areas_200/Model_5_Even.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M5_E")
results(model_path = "Results/Models/Areas_200/Model_5_Two.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M5_2")
results(model_path = "Results/Models/Areas_200/Model_5_Three.rds",
        post_path = "Data/dat_200_areas.RData",
        tabname = "M5_3")

## 400 Areas 
# Models with (avg) 30 respondents per areas
results(model_path = "Results/Models/Areas_400/Model_30_Even.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M30_E")

results(model_path = "Results/Models/Areas_400/Model_30_Two.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M30_2")

results(model_path = "Results/Models/Areas_400/Model_30_Three.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M30_3")

## Models with (avg) 20 respondents per area
results(model_path = "Results/Models/Areas_400/Model_20_Even.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M20_E")
results(model_path = "Results/Models/Areas_400/Model_20_Two.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M20_2")
results(model_path = "Results/Models/Areas_400/Model_20_Three.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M20_3")

# Models with (avg) 10 respondents per areas
results(model_path = "Results/Models/Areas_400/Model_10_Even.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M10_E")
results(model_path = "Results/Models/Areas_400/Model_10_Two.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M10_2")
results(model_path = "Results/Models/Areas_400/Model_10_Three.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M10_3")

# Models with (avg) 5 respondents per areas
results(model_path = "Results/Models/Areas_400/Model_5_Even.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M5_E")
results(model_path = "Results/Models/Areas_400/Model_5_Two.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M5_2")
results(model_path = "Results/Models/Areas_400/Model_5_Three.rds",
        post_path = "Data/dat_400_areas.RData",
        tabname = "M5_3")

## 600 Areas 
# Models with (avg) 30 respondents per areas
results(model_path = "Results/Models/Areas_600/Model_30_Even.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M30_E")

results(model_path = "Results/Models/Areas_600/Model_30_Two.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M30_2")

results(model_path = "Results/Models/Areas_600/Model_30_Three.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M30_3")

## Models with (avg) 20 respondents per area
results(model_path = "Results/Models/Areas_600/Model_20_Even.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M20_E")
results(model_path = "Results/Models/Areas_600/Model_20_Two.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M20_2")
results(model_path = "Results/Models/Areas_600/Model_20_Three.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M20_3")

# Models with (avg) 10 respondents per areas
results(model_path = "Results/Models/Areas_600/Model_10_Even.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M10_E")
results(model_path = "Results/Models/Areas_600/Model_10_Two.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M10_2")
results(model_path = "Results/Models/Areas_600/Model_10_Three.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M10_3")

# Models with (avg) 5 respondents per areas
results(model_path = "Results/Models/Areas_600/Model_5_Even.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M5_E")
results(model_path = "Results/Models/Areas_600/Model_5_Two.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M5_2")
results(model_path = "Results/Models/Areas_600/Model_5_Three.rds",
        post_path = "Data/dat_600_areas.RData",
        tabname = "M5_3")
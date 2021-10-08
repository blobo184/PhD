## Script to generate tables for MAE and size of credible interval width

## Function collects MAE, calculates width and prints out results
## by shows MAE and CI width for all, groups 2 and groups 3
Create_data <- function(path, data) {
  
    paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:12) {
    holder[[i]] <- read_excel(path, range = cell_cols(c("B", "H")),
                              col_names = TRUE, sheet = paths[[i]])
    holder[[i]]$Areas <- str_remove((str_split(path, "_")[[1]][2]), ".xlsx")
    holder[[i]] <- holder[[i]] %>%
      dplyr::select(GSSCode, True.value, MAE=MAE.temp, RMSE = RMSE.temp, low.Pred, hi.Pred)
    temp <- str_split(paths[[i]], "M")[[1]][2]
    holder[[i]]$Sample <- str_split(temp, "_")[[1]][1]
    holder[[i]]$Design <- str_split(temp, "_")[[1]][2]
    holder
  } 
  dat <- do.call(rbind, holder)
  path_2 <- paste("Data/", data, ".RData", sep = "")
  d <- local({
    load(path_2)
    stopifnot(length(ls())==1)
    environment()[[ls()]]
  })
  groups <- d %>%
    dplyr::select(GSSCode, Groups_2, Groups_3) %>%
    distinct()
  
  dat <- left_join(dat, groups, by = "GSSCode")
  
  dat <- dat %>%
    mutate(Width = hi.Pred-low.Pred)

  Overall <- dat %>%
    group_by(Sample, Design) %>%
    summarise(MAE = round(mean(MAE),2),
              Width = round(mean(Width),2)) %>%
    mutate(Groups = "Overall", 
           Ref = 1)
  
  groups_2 <- dat %>%
    dplyr::filter(Design != "3") %>%
    rename(Groups = Groups_2) %>%
    group_by(Sample, Design, Groups) %>%
    mutate(Width = hi.Pred - low.Pred) %>%
    summarise(MAE = round(mean(MAE),2),
              Width = round(mean(Width),2)) %>%
    mutate(Ref = 2)
    

  groups_3 <- dat %>%
    dplyr::filter(Design != "2") %>%
    rename(Groups = Groups_3) %>%
    group_by(Sample, Design, Groups) %>%
    mutate(Width = hi.Pred - low.Pred) %>%
    summarise(MAE = round(mean(MAE),2),
              Width = round(mean(Width),2)) %>%
    mutate(Ref = 3)
  final <- bind_rows(Overall, groups_2, groups_3)
}
view(Areas_600)
Areas_600 <- Create_data(path = "Results/Estimates_600.xlsx", # Name of excel estimates sheet
                        data = "dat_600_areas") # name of master data frame
Areas_400 <- Create_data(path = "Results/Estimates_400.xlsx",
                         data = "dat_400_areas")
Areas_200 <- Create_data(path = "Results/Estimates_200.xlsx",
                         data = "dat_200_areas")
Areas_50 <- Create_data(path = "Results/Estimates_50.xlsx",
                         data = "dat_50_areas")
save(Areas_600, Areas_400, Areas_200, Areas_50, file = "Results/MAE_and_width.RData")

## Convert to data frame as xlsx can't work with tibbles.
Areas_600 <- as.data.frame(Areas_600)
Areas_400 <- as.data.frame(Areas_400)
Areas_200 <- as.data.frame(Areas_200)
Areas_50 <- as.data.frame(Areas_50)
write.xlsx(Areas_600, file = "Results/MAE_and_width.xlsx", sheetName = "600")
write.xlsx(Areas_400, file = "Results/MAE_and_width.xlsx", sheetName = "400", append = T)
write.xlsx(Areas_200, file = "Results/MAE_and_width.xlsx", sheetName = "200", append = T)
write.xlsx(Areas_50, file = "Results/MAE_and_width.xlsx", sheetName = "50", append = T)


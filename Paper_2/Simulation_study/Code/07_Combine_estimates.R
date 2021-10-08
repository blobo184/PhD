## Combine estimates into one RData file
Create_data <- function(path){
  paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:12) {
    holder[[i]] <- read_excel(path, range = cell_cols(c("B", "C", "D", "E", "F")),
                              col_names = TRUE, sheet = paths[[i]])
    holder[[i]]$Areas <- str_remove((str_split(path, "_")[[1]][2]), ".xlsx")
    temp <- str_split(paths[[i]], "M")[[1]][2]
    holder[[i]]$Sample <- str_split(temp, "_")[[1]][1]
    holder[[i]]$Design <- str_split(temp, "_")[[1]][2]
    holder
  } 
  dat <- do.call(rbind, holder)
}

## Extract data from excel sheets
Areas_600 <- Create_data(path = "Results/Estimates_600.xlsx")
Areas_400 <- Create_data(path = "Results/Estimates_400.xlsx")
Areas_200 <- Create_data(path = "Results/Estimates_200.xlsx")
Areas_50 <- Create_data(path = "Results/Estimates_50.xlsx")

## Create combined data set
Combined <- rbind(Areas_600, Areas_400, Areas_200, Areas_50)
save(Combined, file = "Results/Combined_results.RData")

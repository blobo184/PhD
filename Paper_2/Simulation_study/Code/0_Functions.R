## Functions for simulation study

gather_accuracy <- function(path) {
  paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:12) {
    holder[[i]] <- read_excel(path, range = "B1:G2",
                              col_names = TRUE, sheet = paths[[i]])
    holder[[i]]$Model <- paths[[i]]
  }
  dat <- do.call(rbind, holder)
  dat <- dat %>%
    relocate(Model)
  dat$order <- 1:12
  
  dat <- dat %>%
    mutate(Sample = dplyr::case_when(
      grepl("M30", Model) ~ "30",
      grepl("M20", Model) ~ "20",
      grepl("M10", Model) ~ "10",
      grepl("M5", Model) ~ "5")) 
  
  dat <- dat %>%
    mutate(Design = dplyr::case_when(
      grepl("_E", Model) ~ "Even",
      grepl("_2", Model) ~ "Two groups",
      grepl("_3", Model) ~ "Three groups"))
  dat$Areas <- str_remove(str_split(path, "_")[[1]][2], ".xlsx")
  dat
}
## Script to calculate coverage of MRP estimates. 
## Collate into single file and write to xlsx doc.

## Fucntion to gather data and calculate coverage.
Print_accuracy <- function(path, data) {
  
  paths <- path %>%
    excel_sheets()
  holder <- list()
  for (i in 1:12) {
    holder[[i]] <- read_excel(path, range = cell_cols(c("B", "F", "C", "D", "E")),
                              col_names = TRUE, sheet = paths[[i]])
  } 
  
dat <- holder %>%
    reduce(left_join, by = c("GSSCode", "True.value")) %>%
    relocate(GSSCode, True.value)
names(dat)[3:38] <- c("Mean_M30_E",	"Lo_M30_E",	"Hi_M30_E",	"Mean_M30_2",	"Lo_M30_2",	"Hi_M30_2",	"Mean_M30_3",	"Lo_M30_3",	"Hi_M30_3",	"Mean_M20_E",	"Lo_M20_E",	"Hi_M20_E",
"Mean_M20_2",	"Lo_M20_2",	"Hi_M20_2",	"Mean_M20_3",	"Lo_M20_3",	"Hi_M20_3",	"Mean_M10_E",	"Lo_M10_E",	"Hi_M10_E",	"Mean_M10_2",	"Lo_M10_2",	"Hi_M10_2",
"Mean_M10_3",	"Lo_M10_3",	"Hi_M10_3",	"Mean_M5_E",	"Lo_M5_E",	"Hi_M5_E",	"Mean_M5_2",	"Lo_M5_2",	"Hi_M5_2",	"Mean_M5_3",	"Lo_M5_3",	"Hi_M5_3")

dat <- dat %>%
  dplyr::select(GSSCode, True.value,contains(c("Lo", "Hi")))
dat <- dat %>%
  mutate(M30_E = ifelse(True.value > Lo_M30_E & True.value < Hi_M30_E, 1, 0),
         M30_2 = ifelse(True.value > Lo_M30_2 & True.value < Hi_M30_2, 1, 0),
         M30_3 = ifelse(True.value > Lo_M30_3 & True.value < Hi_M30_3, 1, 0),
         M20_E = ifelse(True.value > Lo_M20_E & True.value < Hi_M20_E, 1, 0),
         M20_2 = ifelse(True.value > Lo_M20_2 & True.value < Hi_M20_2, 1, 0),
         M20_3 = ifelse(True.value > Lo_M20_3 & True.value < Hi_M20_3, 1, 0),
         M10_E = ifelse(True.value > Lo_M10_E & True.value < Hi_M10_E, 1, 0),
         M10_2 = ifelse(True.value > Lo_M10_2 & True.value < Hi_M10_2, 1, 0),
         M10_3 = ifelse(True.value > Lo_M10_3 & True.value < Hi_M10_3, 1, 0),
         M5_E = ifelse(True.value > Lo_M5_E & True.value < Hi_M5_E, 1, 0),
         M5_2 = ifelse(True.value > Lo_M5_2 & True.value < Hi_M5_2, 1, 0),
         M5_3 = ifelse(True.value > Lo_M5_3 & True.value < Hi_M5_3, 1, 0)) %>%
  dplyr::select(GSSCode, starts_with("M"))

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
dat
  Total <- dat %>%
    dplyr::select(contains("M")) %>%
    mutate(Group = "Overall") %>%
    group_by(Group) %>%
    summarise_all(funs(sum(.)/n()))
  
  Group_3 <- dat %>%
    dplyr::select(Group = Groups_3, contains("M")) %>%
    mutate(Group = (dplyr::recode(Group, 
                                  "1" = "Primary",
                                  "2" = "Secondary",
                                  "3" = "Tertiary"))) %>% 
    group_by(Group) %>%
    summarise_all(funs(sum(.)/n()))
  
  Group_2 <- dat %>%
    dplyr::select(Group = Groups_2, contains("M")) %>%
    mutate(Group = dplyr::recode(Group,
                                 "1" = "Primary",
                                 "2" = "Secondary")) %>%
    group_by(Group) %>%
    summarise_all(funs(sum(.)/n()))
  Merged <- bind_rows(Total, Group_3, Group_2)
  Merged$Areas <- strsplit(data,"_")[[1]][2]
  Merged
}

##
Merged <- bind_rows(Print_accuracy(path = "Results/Estimates_600.xlsx",
                                 data = "dat_600_areas"),
                    Print_accuracy(path = "Results/Estimates_400.xlsx",
                                              data = "dat_400_areas"),
                    Print_accuracy(path = "Results/Estimates_200.xlsx",
                                              data = "dat_200_areas"),
                    Print_accuracy(path = "Results/Estimates_50.xlsx",
                                             data = "dat_50_areas"))

save(Merged, file = "Results/Coverage.RData")
write.xlsx(Merged, file = "Results/Coverage.xlsx", sheetName = "All")


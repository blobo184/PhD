## Script to investigate the raw Y for each small area for all 48 data frames

get_raw_Y <- function(Areas, Type) {
area <- if (Areas == 50){
  "Areas_50/"} else if (Areas == 200) {
    "Areas_200/"} else if (Areas == 400) {
      "Areas_400/"} else {
        "Areas_600/"}

type <- if (Type == "E") {
  "Even"} else if (Type == 2) {
    "2to1"} else {
      "3to1"}

path <- paste0("Data/",area,type)
files <- list.files(path, full.names = T)

holder <- list()
for(i in 1:length(files)){
  holder[[i]] <- local({
  load(files[[i]])
  stopifnot(length(ls())==1)
  environment()[[ls()]]
  })
holder[[i]] <- holder[[i]] %>%
  dplyr::select(GSSCode, Y) %>%
  group_by(GSSCode) %>%
  summarise(Y = sum(Y)/n())
samples <- if (str_detect(files[[i]], "sample_5") == "TRUE") {
  5
} else if (str_detect(files[[i]], "sample_10") == "TRUE") {
  10
} else if (str_detect(files[[i]], "sample_20") == "TRUE") {
  20
} else {30}
  holder[[i]] <- holder[[i]] %>%
    mutate(Area = Areas,
           Design = as.character(Type),
           Sample = samples)
}
  dat <- do.call(rbind, holder)
  
  path_truth <- if (Areas == 50){
  "Data/dat_50_areas.RData"} else if (Areas == 200) {
    "Data/dat_200_areas.RData"} else if (Areas == 400) {
      "Data/dat_400_areas.RData"} else {
        "Data/dat_600_areas.RData"}
  
  truth <- local({
    load(path_truth)
    stopifnot(length(ls())==1)
    environment()[[ls()]]
  })
  truth <- truth %>%
    group_by(GSSCode) %>%
    summarise(Truth = sum(Y*weight),
              Groups_2 = unique(Groups_2),
              Groups_3 = unique(Groups_3))

  dat <- left_join(dat, truth, by = "GSSCode")
  
  dat <- dat %>%
    mutate(Diff = abs(Truth - Y))
}


## 600 Areas
Area_600 <- bind_rows(get_raw_Y(Areas = 600, Type ="E"),
                    get_raw_Y(Areas = 600, Type =2),
                    get_raw_Y(Areas = 600, Type =3))

Grouped_3_600 <- Area_600 %>%
  group_by(Sample, Design, Groups_3) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_3_600 <- as.data.frame(Grouped_3_600)
Grouped_3_600 <- pivot_wider(Grouped_3_600, names_from = Groups_3, values_from = Diff)
write.xlsx(Grouped_3_600, file = "Raw_figures.xlsx", sheetName = "Grouped_3_600")


Grouped_2_600 <- Area_600 %>%
  group_by(Sample, Design, Groups_2) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_2_600 <- as.data.frame(Grouped_2_600)
Grouped_2_600 <- pivot_wider(Grouped_2_600, names_from = Groups_2, values_from = Diff)
write.xlsx(Grouped_2_600, file = "Raw_figures.xlsx", sheetName = "Grouped_2_600", append = T)


## 400 Areas
Area_400 <- bind_rows(get_raw_Y(Areas = 400, Type ="E"),
                      get_raw_Y(Areas = 400, Type =2),
                      get_raw_Y(Areas = 400, Type =3))

Grouped_3_400 <- Area_400 %>%
  group_by(Sample, Design, Groups_3) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_3_400 <- as.data.frame(Grouped_3_400)
Grouped_3_400 <- pivot_wider(Grouped_3_400, names_from = Groups_3, values_from = Diff)
write.xlsx(Grouped_3_400, file = "Raw_figures.xlsx", sheetName = "Grouped_3_400", append = T)


Grouped_2_400 <- Area_400 %>%
  group_by(Sample, Design, Groups_2) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_2_400 <- as.data.frame(Grouped_2_400)
Grouped_2_400 <- pivot_wider(Grouped_2_400, names_from = Groups_2, values_from = Diff)
write.xlsx(Grouped_2_400, file = "Raw_figures.xlsx", sheetName = "Grouped_2_400", append = T)


## 200 Areas
Area_200 <- bind_rows(get_raw_Y(Areas = 200, Type ="E"),
                      get_raw_Y(Areas = 200, Type =2),
                      get_raw_Y(Areas = 200, Type =3))

Grouped_3_200 <- Area_200 %>%
  group_by(Sample, Design, Groups_3) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_3_200 <- as.data.frame(Grouped_3_200)
Grouped_3_200 <- pivot_wider(Grouped_3_200, names_from = Groups_3, values_from = Diff)
write.xlsx(Grouped_3_200, file = "Raw_figures.xlsx", sheetName = "Grouped_3_200", append = T)


Grouped_2_200 <- Area_200 %>%
  group_by(Sample, Design, Groups_2) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_2_200 <- as.data.frame(Grouped_2_200)
Grouped_2_200 <- pivot_wider(Grouped_2_200, names_from = Groups_2, values_from = Diff)
write.xlsx(Grouped_2_200, file = "Raw_figures.xlsx", sheetName = "Grouped_2_200", append = T)

## 50 Areas
Area_50 <- bind_rows(get_raw_Y(Areas = 50, Type ="E"),
                      get_raw_Y(Areas = 50, Type =2),
                      get_raw_Y(Areas = 50, Type =3))

Grouped_3_50 <- Area_50 %>%
  group_by(Sample, Design, Groups_3) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_3_50 <- as.data.frame(Grouped_3_50)
Grouped_3_50 <- pivot_wider(Grouped_3_50, names_from = Groups_3, values_from = Diff)
write.xlsx(Grouped_3_50, file = "Raw_figures.xlsx", sheetName = "Grouped_3_50", append = T)


Grouped_2_50 <- Area_50 %>%
  group_by(Sample, Design, Groups_2) %>%
  summarise(Diff = round(mean(Diff),2))
Grouped_2_50 <- as.data.frame(Grouped_2_50)
Grouped_2_50 <- pivot_wider(Grouped_2_50, names_from = Groups_2, values_from = Diff)
write.xlsx(Grouped_2_50, file = "Raw_figures.xlsx", sheetName = "Grouped_2_50", append = T)


## Script to download and unzip all ACS 5-year files for eact state
load("../Data/anes_2016_tidy.RData")
st <- unique(anes_2016$ST)
st <- tolower(st)
st <- sort(st)
rm(anes_2016)

#dir.create("../Data/pums_2012/")

## Base url
base_url <- "https://www2.census.gov/programs-surveys/acs/data/pums/2012/5-Year/csv_p"

for (i in 1:length(st)){
  
  url <- paste0(base_url, st[i], ".zip")
  destfile <- paste0("../Data/pums_2012/csv_p", st[i],".zip")
  download.file(url, destfile, mode="wb")
  
}

## All zip folders downloaded
## I extracted in linux with - unzip '*.zip' 'ss12p*'
## Utility

## FIPS to ST abreviation
FIPS_to_ST <- function(x) {
  x <- dplyr::recode(x,
                     "1" = "AL",	"2" = "AK",	"4" = "AZ",	"5" = "AR",
                     "6" = "CA",	"8" = "CO",	"9" = "CT",	"10" = "DE",
                     "11" = "DC",	"12" = "FL",	"13" = "GA",	"15" = "HI",
                     "16" = "ID",	"17" = "IL",	"18" = "IN",	"19" = "IA",
                     "20" = "KS",	"21" = "KY",	"22" = "LA",	"23" = "ME",
                     "24" = "MD",	"25" = "MA",	"26" = "MI",	"27" = "MN",
                     "28" = "MS",	"29" = "MO",	"30" = "MT",	"31" = "NE",
                     "32" = "NV",	"33" = "NH",	"34" = "NJ",	"35" = "NM",
                     "36" = "NY",	"37" = "NC",	"38" = "ND",	"39" = "OH",
                     "40" = "OK",	"41" = "OR",	"42" = "PA",	"44" = "RI",
                     "45" = "SC",	"46" = "SD",	"47" = "TN",	"48" = "TX",
                     "49" = "UT",	"50" = "VT",	"51" = "VA",	"53" = "WA",
                     "54" = "WV",	"55" = "WI",	"56" = "WY", .default = NA_character_)
}

State_to_Region <- function(x){
  x <- dplyr::recode(x,
                     "Alabama" = "South",	"Alaska" = "West",	"Arizona" = "West",	"Arkansas" = "South",
                     "California" = "West",	"Colorado" = "West",	"Connecticut" = "Northeast",	"Delaware" = "South",
                     "Florida" = "South",	"Georgia" = "South",	"Hawaii" = "West",	"Idaho" = "West",
                     "Illinois" = "Midwest",	"Indiana" = "Midwest",	"Iowa" = "Midwest",	"Kansas" = "Midwest",
                     "Kentucky" = "South",	"Louisiana" = "South",	"Maine" = "Northeast",	"Maryland" = "South",
                     "Massachusetts" = "Northeast",	"Michigan" = "Midwest",	"Minnesota" = "Midwest",	"Mississippi" = "South",
                     "Missouri" = "Midwest",	"Montana" = "West",	"Nebraska" = "Midwest",	"Nevada" = "West",
                     "New Hampshire" = "Northeast",	"New Jersey" = "Northeast",	"New Mexico" = "West",	"New York" = "Northeast",
                     "North Carolina" = "South",	"North Dakota" = "Midwest",	"Ohio" = "Midwest",	"Oklahoma" = "South",
                     "Oregon" = "West",	"Pennsylvania" = "Northeast",	"Rhode Island" = "Northeast",	"South Carolina" = "South",
                     "South Dakota" = "Midwest",	"Tennessee" = "South",	"Texas" = "South",	"Utah" = "West",
                     "Vermont" = "Northeast",	"Virginia" = "South",	"Washington" = "West",	"West Virginia" = "South",
                     "Wisconsin" = "Midwest",	"Wyoming" = "West")
  x
  
}

FIPS_to_State <- function(x) {
  x <- dplyr::recode(x, 
                     "1" = "Alabama",	"2" = "Alaska",	"4" = "Arizona",	"5" = "Arkansas",
                     "6" = "California",	"8" = "Colorado",	"9" = "Connecticut",	"11" = "DC",
                     "10" = "Delaware",	"12" = "Florida",	"13" = "Georgia",	"15" = "Hawaii",
                     "16" = "Idaho",	"17" = "Illinois",	"18" = "Indiana",	"19" = "Iowa",
                     "20" = "Kansas",	"21" = "Kentucky",	"22" = "Louisiana",	"23" = "Maine",
                     "24" = "Maryland",	"25" = "Massachusetts",	"26" = "Michigan",	"27" = "Minnesota",
                     "28" = "Mississippi",	"29" = "Missouri",	"30" = "Montana",	"31" = "Nebraska",
                     "32" = "Nevada",	"33" = "New Hampshire",	"34" = "New Jersey",	"35" = "New Mexico",
                     "36" = "New York",	"37" = "North Carolina",	"38" = "North Dakota",	"39" = "Ohio",
                     "40" = "Oklahoma",	"41" = "Oregon",	"42" = "Pennsylvania",	"44" = "Rhode Island",
                     "45" = "South Carolina",	"46" = "South Dakota",	"47" = "Tennessee",	"48" = "Texas",
                     "49" = "Utah",	"50" = "Vermont",	"51" = "Virginia",	"53" = "Washington",
                     "54" = "West Virginia",	"55" = "Wisconsin",	"56" = "Wyoming")
}

get_State_to_ST <- function(){
  dat <- data.frame(no = 1:100)
  dat$ST <- FIPS_to_ST(dat$no)
  dat$State <- FIPS_to_State(dat$no)
  dat <- dat %>%
    drop_na() %>%
    dplyr::select(-no)
  dat$State <- recode(dat$State,
                      "DC" = "District of Columbia")
  dat
}


get_ST_to_Region <- function(){
  dat <- data.frame(no = 1:100)
  dat$Area <- FIPS_to_ST(dat$no)
  dat$State <- FIPS_to_State(dat$no)
  dat$Region <- State_to_Region(dat$State)
  dat <- dat %>%
    drop_na() %>%
    dplyr::select(-no, -State)
  
  dat
}


## Function to import and tidy ACS data
## This saves importing v. large files with unnecessary vars.
gather_ACS <- function(path){
  age_breaks <- c(17, 34, 44, 64, Inf)
  age_labs <- c("18-34", "35-44", "45-64", "65+")
  psa <- fread(path,
               sep = ",",
               select = c("HISP", "ST", "AGEP", 
                          "SEX", "SCHL", "RAC1P", "PWGTP", "MAR")) %>%
    dplyr::filter(AGEP > 17) %>%
    mutate(Age = cut(AGEP,
                     breaks = age_breaks,
                     labels = age_labs)) %>%
    mutate(Gender = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ NA_character_)) %>%
    mutate(Education = dplyr::case_when(
      SCHL %in% 1:15 ~ "No HS diploma",
      SCHL %in% 16:17 ~ "HS diploma",
      SCHL %in% 18:19 ~ "Some college",
      SCHL %in% 20:21 ~ "College grad",
      SCHL %in% 22:24 ~ "Postgrad")) %>%
    mutate(Race = dplyr::case_when(
      HISP > 1 ~ "Hispanic",
      RAC1P == 1 ~ "White",
      RAC1P == 2 ~ "Black",
      RAC1P %in% 3:9 ~ "Other")) %>%
    mutate(Marital = dplyr::case_when(
      MAR == 1 ~ "Married",
      MAR == 2 ~ "Widowed",
      MAR == 3 ~ "Sep-Divorced",
      MAR == 4 ~ "Sep-Divorced",
      MAR == 5 ~ "Single")) %>%
    group_by(ST, Age, Gender, Education, Race, Marital) %>%
    summarise(pop = sum(PWGTP))
  psa
}


get_inf_priors <- function(lambda, mod){
  require(tidyverse)
  require(brms)
  require(tidybayes)
  # Get mu and sd for all fixed effects
  b_est <- mod %>%
    gather_draws(`b_.*`, regex = T) %>%
    group_by(.variable) %>%
    summarise(mu = round(median(.value),2),
              SD = round(sd(.value)*lambda,2)) %>%
    ungroup() %>%
    mutate(var = str_remove(.variable, "b_"),
           priors = paste0("normal(", mu, ",",SD, ")")) %>%
    dplyr::select(var, priors)
  
  # Set Intercept prior
  priors <- set_prior(b_est$priors[b_est$var=="Intercept"],
                      class = "Intercept")
  # Set priors for all fixed effects
  vars <- unique(b_est$var)
  vars <- vars[grepl("Intercept", vars)==FALSE]# exclude intercept
  for (v in vars) {
    
    priors <- priors +
      set_prior(b_est$priors[b_est$var==v],
                class = "b",
                coef = v)
  }
  
  # Get mu and sd for all sd priors
  mod_sd <- mod %>%
    gather_draws(`sd_.*`, regex = TRUE) %>%
    group_by(.variable) %>%
    summarise(mu = round(median(.value),2),
              SD = round(sd(.value)*lambda,2)) %>%
    ungroup() %>%
    mutate(var = str_remove(.variable,"sd_"),
           var = str_remove(var, "__Intercept"),
           priors = paste0("student_t(5,", mu, ",",SD,")"))
  
  vars <- unique(mod_sd$var)
  for (v in vars) {
    if (is.null(priors)) {
      priors <- set_prior(mod_sd$priors[mod_sd$var==v],
                          class = "sd",
                          group = v)
    } else {
      priors <- priors +
        set_prior(mod_sd$priors[mod_sd$var==v],
                  class = "sd",
                  group = v)
    }
  }
  priors
}


## Function used to get the fixed effect informative priors
get_inf_priors_FE <- function(mod, lambda) {
  # Function to get coef est mu and sd from model
  ## and print to set_prior brms for new model.
  
  # Extract b mu and sd from model and convert 
  # brms set_prior format
  b_est <- mod %>%
    gather_draws(`b_.*`, regex = T) %>%
    group_by(.variable) %>%
    summarise(mu = round(median(.value),2),
              SD = round(sd(.value)*lambda,2)) %>%
    ungroup() %>%
    mutate(var = str_remove(.variable, "b_"),
           priors = paste0("normal(", mu, ",", SD, ")")) %>%
    dplyr::select(var, priors)
  
  # Do the same for r levels
  mod_re <- mod %>%
    gather_draws(`r_.*`, regex = TRUE) %>% 
    group_by(.variable) %>%
    summarise(mu = round(median(.value),2),
              SD = round(sd(.value)*lambda,2)) %>%
    ungroup() %>%
    mutate(temp.var = .variable) %>%
    separate(temp.var, into = c(NA, "group")) %>%
    group_by(group) %>%
    mutate(var_row = 1:n(),
           mu = ifelse(var_row != 1, (mu+mu[1]), mu),
           mu = round(mu,2)) %>%
    ungroup() %>%  
    dplyr::filter(var_row != 1) %>%
    mutate(var = str_remove(.variable, "r_"),
           var = str_remove(var, ",Intercept]"),
           var = str_remove(var, "\\["),
           var = str_replace(var, "-", "M"),
           var = str_replace(var, "\\+", "P"),
           var = str_replace_all(var, "\\.", ""),
           var = str_replace(var, "/", "D"),
           priors = paste0("normal(", mu, ",", SD, ")"))
  
  # Set Intercept prior
  priors <- c(set_prior(b_est$priors[b_est$var=="Intercept"],
                        class = "Intercept"),
              set_prior("student_t(5, 0, 5)", class = "sd", group = "Area"))
  
  # Set priors for all fixed effects
  vars <- unique(b_est$var)
  vars <- vars[grepl("Intercept", vars)==FALSE]# exclude intercept
  
  ## Set priors for fixed effects in original model
  for (v in vars) {
    if (is.null(priors)) {
      priors <- set_prior(b_est$priors[b_est$var==v],
                          class = "b",
                          coef = v)
    } else {
      
      priors <- priors +
        set_prior(b_est$priors[b_est$var==v],
                  class = "b",
                  coef = v)
    }
  }
  
  ## Set sd priors for small area
  
  
  ## Set priors for all levels of random effects
  
  vars <- unique(mod_re$var)
  vars <- vars[grepl("Area|ST|PCON_ID", vars)==FALSE]# Exclude either State or PCON
  for (v in vars) {
    if (is.null(priors)) {
      priors <- set_prior(mod_re$priors[mod_re$var==v],
                          class = "b",
                          coef = v)
    } else {
      priors <- priors +
        set_prior(mod_re$priors[mod_re$var==v],
                  class = "b",
                  coef = v)
    }
  }
  
  priors  
}



## Poststratification function
post_strat <- function(mod, post, iters){
  
  out<- brms::posterior_epred(mod,
                              newdata=post,
                              draws = iters)
  out.m <- melt(out, varnames = c("iter", "post_row"))
  out.m$weight <- post$w[out.m$post_row]
  out.m$Area <- post$Area[out.m$post_row]
  out.m$Turnout <- post$Turnout[out.m$post_row]
  out.m$Truth <- post$Truth[out.m$post_row]
  
  results <- out.m %>%
    group_by(Area, iter) %>%
    summarise(temp.Pred = sum((value*Turnout)*weight),
              Truth = unique(Truth),
              err = (temp.Pred - Truth))
  
  result.export <- results %>%
    group_by(Area) %>%
    summarise(Pred = mean(temp.Pred),
              Pred_lo = quantile(temp.Pred, 0.05),
              Pred_hi = quantile(temp.Pred, 0.95),
              MAE.temp = mean(abs(err)),
              RMSE.temp = sqrt(mean(err^2)))
  result.export
}

## Poststratification function
demog_post_strat <- function(mod, post, iters){
  
  out<- brms::posterior_epred(mod,
                              newdata=post,
                              draws = iters)
  out.m <- melt(out, varnames = c("iter", "post_row"))
  out.m$weight <- post$w[out.m$post_row]
  out.m$Age <- post$Age[out.m$post_row]
  out.m$Education <- post$Education[out.m$post_row]
  out.m$Marital <- post$Marital[out.m$post_row]
  out.m$Ethnicity <- post$Ethnicity[out.m$post_row]
  out.m$Turnout <- post$Turnout[out.m$post_row]
  
  results <- out.m %>%
    group_by(Age, Education, Marital, Ethnicity, iter) %>%
    summarise(temp.Pred = sum((value*Turnout)*weight))
  
  result.export <- results %>%
    group_by(Age, Education, Marital, Ethnicity) %>%
    summarise(Pred = mean(temp.Pred),
              Pred_lo = quantile(temp.Pred, 0.05),
              Pred_hi = quantile(temp.Pred, 0.95))
  result.export
}
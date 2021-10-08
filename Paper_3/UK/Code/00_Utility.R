## Utility functions

# Recode education categories
educ_recode <- function(x){
  x <- rio::characterize(x)
  x <- str_replace(x, "&", "and")
  x <- str_remove_all(x, "[,']")
  x <- tolower(x)
  x <- case_when(
    grepl("certificate - advanced", x)=="TRUE" ~ "Level 2",
    grepl("city and guilds",x)=="TRUE" ~ "Level 1",
    grepl("clerical",x)=="TRUE" ~ "Level 1",
    grepl("cse grade 1", x)=="TRUE" ~ "Level 2",
    grepl("cse grades 2-5", x)=="TRUE" ~ "Level 1",
    grepl("dont know", x)=="TRUE" ~ "No qualifications",
    grepl("gce a level", x)=="TRUE" ~ "Level 3",
    grepl("no formal", x)=="TRUE" ~ "No qualifications",
    grepl("nursing", x)=="TRUE" ~ "Level 4/5",
    grepl("onc", x)=="TRUE" ~ "Level 2",
    grepl("other tech", x)=="TRUE" ~ "Other",
    grepl("prefer not", x)=="TRUE" ~ "No qualifications",
    grepl("recognised trade", x)=="TRUE" ~ "Level 2",
    grepl("scottish higher", x)=="TRUE" ~ "Level 3",
    grepl("scottish ordinary", x)=="TRUE" ~ "Level 2",
    grepl("teaching", x)=="TRUE" ~ "Level 4/5",
    grepl("university", x)=="TRUE" ~ "Level 4/5",
    grepl("youth", x)=="TRUE" ~ "Level 2",
    TRUE ~ NA_character_)
  
  x
}

# Bucket age into bins
age_bucket <- function(x) {
  age_breaks <- c(-Inf, 19, 24, 29, 44, 59, 64, 74, Inf)
  age_labels <- c("16-19", "20-24", "25-29", "30-44", "45-59", "60-64", "65-74", "75+")
  
  x <- as.numeric(as.character(x))
  x <- cut(x,
           breaks = age_breaks,
           labels = age_labels)
  x
}

# Bucket date into bins
date_bucket <- function(x){
  x <- as.Date(x, format = "%Y%m%d")
  x <- as.numeric(abs(x - max(x)))
  
  campaign_break <- c(-Inf, 6, 13, 20, Inf)
  campaign_labels <- c(1,2,3,4)
  x <- cut(x, breaks = campaign_break,
           labels = campaign_labels)
  
  x
}

# Recode vote intention
vote_recode <- function(x, y) {
  x <- rio::characterize(x)
  x <- str_remove_all(x, "[,']")
  x <- tolower(x)
  x <- case_when(
    y %in% 1:4 ~ "WNV",
    y == 9999 ~ "WNV",
    grepl("not vote", x)=="TRUE" ~ "WNV",
    grepl("independent", x)=="TRUE" ~ "Other",
    grepl("brexit", x)=="TRUE" ~ "BXP",
    grepl("con", x)=="TRUE" ~ "Con",
    grepl("lab", x)=="TRUE" ~ "Lab",
    grepl("liberal", x)=="TRUE" ~ "LD",
    grepl("green", x)=="TRUE" ~ "Green",
    grepl("plaid", x)=="TRUE" ~ "PC",
    grepl("snp", x)=="TRUE" ~ "SNP",
    grepl("ukip", x)=="TRUE" ~ "UKIP",
    grepl("other", x)=="TRUE" ~ "Other",
    grepl("dont", x)=="TRUE" ~ "WNV",
    TRUE ~ "WNV")
  
  x
}

# Poststratification function
post_strat <- function(mod, post, iters){
  
  out<- brms::posterior_epred(mod,
                                  newdata=post,
                                  draws = iters)
  out.m <- melt(out, varnames = c("iter", "post_row"))
  out.m$weight <- post$weight[out.m$post_row]
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


# Get informative priors function
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
  if(("leavehanretty" %in% priors$coef)==FALSE){
    priors <- priors +
      set_prior("normal(0, 5)",
                class = "b",
                coef = "leavehanretty")
  } else (priors <- priors)
  priors
}


# Function used to get the fixed effect informative priors
get_inf_priors_FE <- function(mod, lambda) {
  # Function needed for prior specification for 
  # alternative model
  
  # Get mu and sd for all fixed effects
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
           mu = round(mu, 2)) %>%
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
  
  # Set Intercept prior and area SD prior
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
  
  if(("leavehanretty" %in% priors$coef)==FALSE){
    priors <- priors +
      set_prior("normal(0, 5)",
                class = "b",
                coef = "leavehanretty")
  } else (priors <- priors)
  
  priors  
}

## Extract variable parameter posteriors 
extract_posteriors <- function(paths){
  
  
  holder <- list()
  for (i in 1:length(paths)){
    
    path <- paths[i]
    sample <- str_split(str_split(path, "Sample_")[[1]][2], "/")[[1]][1]
    year <- if(grepl("2017", path)==TRUE){2017} else{2019}
    model <- case_when(
      grepl("weak", path)=="TRUE" ~ "Weakly inf.",
      grepl("inf_", path)=="TRUE" ~ "Random effect",
      grepl("fe_", path)=="TRUE" ~ "Fixed effect")
    lambda <- case_when(
      model == "Weakly inf." ~ NA_real_,
      grepl("_a", path)=="TRUE" ~ 1,
      grepl("_b", path)=="TRUE" ~ 1.5,
      grepl("_c", path)=="TRUE" ~ 2)
    
    mod <- readRDS(path)
    
    # Intercept
    mod_Intercept <- mod %>%
      spread_draws(b_Intercept) %>%
      mutate(Sample = sample,
             Model = model,
             Lambda = lambda,
             Year = year,
             Variable = "Intercept") %>%
      rename(Effect = b_Intercept)
    
    # Age
    mod_Age <- mod %>%
      spread_draws(r_Age[condition,]) %>%
      mutate(Sample = sample,
             Model = model,
             Lambda = lambda,
             Year = year,
             Variable = "Age") %>%
      rename(Effect = r_Age)
    
    # Education
    mod_Education <- mod %>%
      spread_draws(r_Education[condition,]) %>%
      mutate(Sample = sample,
             Model = model,
             Lambda = lambda,
             Year = year,
             Variable = "Education") %>%
      rename(Effect = r_Education)
    
    ## Past Labour vote
    past_Lab <- mod %>%
      spread_draws(b_Past_Lab) %>%
      mutate(Sample = sample,
             Model = model,
             Lambda = lambda,
             Year = year,
             Variable = "Lab_vote",
             condition = "Fixef") %>%
      rename(Effect = b_Past_Lab)
    
    past_Leave <- mod %>%
      spread_draws(b_leavehanretty) %>%
      mutate(Sample = sample,
             Model = model,
             Lambda = lambda,
             Year = year,
             Variable = "Leave",
             condition = "Fixef") %>%
      rename(Effect = b_leavehanretty)
    
    holder[[i]] <- do.call("rbind", list(mod_Age, past_Lab, past_Leave, mod_Intercept, mod_Education))
  }
  dat <- do.call("rbind", holder)
  dat
}
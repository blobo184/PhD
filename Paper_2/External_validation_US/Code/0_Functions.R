## Functions

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

Number_to_State <- function(x) {
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

Number_to_ST <- function(x) {
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
                     "54" = "WV",	"55" = "WI",	"56" = "WY")
}

## Poststratification
poststrat <- function(model, post) {
  ## First let's read in the model depending on the model
  mod <- if(model == "Even"){
    readRDS(file = "../Results/Model_Even.RDS")} else if(model == "Two"){
      readRDS(file = "../Results/Model_Two.RDS")} else {
        readRDS(file = "../Results/Model_Three.RDS")}
  
  ## Poststratification
  out <- brms::posterior_epred(mod, 
                                   newdata = post,
                                   draws = 100)
  
  
  out.m <- reshape2::melt(out, varnames = c("iter", "post_row"))
  out.m$w <- post$w[out.m$post_row]
  out.m$ST <- post$ST[out.m$post_row]
  out.m$Turnout <- post$Turnout[out.m$post_row]
  out.m$True_Rep <- post$True_Rep[out.m$post_row]
  
  results <- out.m %>%
    group_by(ST, iter) %>%
    summarise(temp.pred = sum((value*Turnout)*w),
              True_Rep = unique(True_Rep),
              err = (temp.pred - True_Rep))
    
    result.export <- results %>%
    group_by(ST) %>%
    summarise(Pred = mean(temp.pred),
              Pred_hi = quantile(temp.pred, 0.95),
              Pred_lo = quantile(temp.pred, 0.05),
              MAE = mean(abs(err)),
              RMSE = sqrt(mean(err^2)))
  result.export
}

###########################
## Models 
## Even Model

load("Data/Bes_Even.RData")
dat <- dat %>%
  mutate(Con_GE2017 = arm::rescale(Con_GE2017),
         Leave_2016 = arm::rescale(Leave_2016),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing))

Even <- brm(Con ~ (1|GSSCode) + (1|Region) + (1|age0) + (1|education) + (1|sex) + (1|Campaign_week) +
            Con_GE2017 + Leave_2016 + Long_term_unemployed + Population_density +
            Industry_manufacturing,
            data = dat,
            family = bernoulli(link = "logit"),
            prior = c(set_prior("student_t(5, 0, 10)", class = "b"),
                      set_prior("student_t(5, 0, 10)", class = "Intercept"),
                      set_prior("student_t(5, 0, 5)", class = "sd")),
            chains = 2, iter = 1000,
            seed = 12345, 
            control = list(adapt_delta = 0.95))

saveRDS(Even, file = "Results/Even.mod.rds")
rm(Even, dat)

## Two-group Model
load("Data/Bes_Two.RData")
dat <- dat %>%
  mutate(Con_GE2017 = arm::rescale(Con_GE2017),
         Leave_2016 = arm::rescale(Leave_2016),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing))

Two <- brm(Con ~ (1|GSSCode) + (1|Region) + (1|age0) + (1|education) + (1|sex) + (1|Campaign_week) +
             Con_GE2017 + Leave_2016 + Long_term_unemployed + Population_density +
             Industry_manufacturing,
           data = dat,
           family = bernoulli(link = "logit"),
           prior = c(set_prior("student_t(5, 0, 10)", class = "b"),
                     set_prior("student_t(5, 0, 10)", class = "Intercept"),
                     set_prior("student_t(5, 0, 5)", class = "sd")),
           chains = 2, iter = 1000,
           seed = 12345, 
           control = list(adapt_delta = 0.95))

saveRDS(Two, file = "Results/Two.mod.rds")
rm(dat, Two)

## Three-group Model
load("Data/Bes_Three.RData")
dat <- dat %>%
  mutate(Con_GE2017 = arm::rescale(Con_GE2017),
         Leave_2016 = arm::rescale(Leave_2016),
         Long_term_unemployed = arm::rescale(Long_term_unemployed),
         Population_density = arm::rescale(Population_density),
         Industry_manufacturing = arm::rescale(Industry_manufacturing))

Three <- brm(Con ~ (1|GSSCode) + (1|Region) + (1|age0) + (1|education) + (1|sex) + (1|Campaign_week) +
               Con_GE2017 + Leave_2016 + Long_term_unemployed + Population_density +
               Industry_manufacturing,
             data = dat,
             family = bernoulli(link = "logit"),
             prior = c(set_prior("student_t(5, 0, 10)", class = "b"),
                       set_prior("student_t(5, 0, 10)", class = "Intercept"),
                       set_prior("student_t(5, 0, 5)", class = "sd")),
             chains = 2, iter = 1000,
             seed = 12345, 
             control = list(adapt_delta = 0.95))

saveRDS(Three, file = "Results/Three.mod.rds")
rm(Three,dat)

d <- dat %>%
  group_by(GSSCode) %>%
  summarise(G2 = unique(Groups2_17),
            G3 = unique(Groups3_17))
write.xlsx(d, file = "groups.xlsx")


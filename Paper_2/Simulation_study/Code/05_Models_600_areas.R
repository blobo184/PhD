## Models - For samples with 600 areas.

## Models for Even spread sample
## Load data
load("Data/Areas_600/Even/Areas600_sample_5.RData")
load("Data/Areas_600/Even/Areas600_sample_10.RData")
load("Data/Areas_600/Even/Areas600_sample_20.RData")
load("Data/Areas_600/Even/Areas600_sample_30.RData")


# Model with 5 respondents per areas
Model_5_Even <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                             X1 + A1 + A2 + A3,
                           data = Areas600_sample_5,
                           family = binomial(link = "logit"),
                           prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                           prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                           chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_5_Even, file = "Results/Models/Areas_600/Model_5_Even.rds")
rm(Model_5_Even)

# Models with 10 respondents per areas
Model_10_Even <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_10,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_10_Even, file = "Results/Models/Areas_600/Model_10_Even.rds")
rm(Model_10_Even)

# Models with 20 respondents per areas
Model_20_Even <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_20,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_20_Even, file = "Results/Models/Areas_600/Model_20_Even.rds")
rm(Model_20_Even)

# Models with 30 respondents per areas
Model_30_Even <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_30,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_30_Even, file = "Results/Models/Areas_600/Model_30_Even.rds")
rm(Model_30_Even)
rm(list=ls(pattern="Areas600"))

## Models with two groups and a ratio of 2:1
## Load data
load("Data/Areas_600/2to1/Areas600_sample_5.RData")
load("Data/Areas_600/2to1/Areas600_sample_10.RData")
load("Data/Areas_600/2to1/Areas600_sample_20.RData")
load("Data/Areas_600/2to1/Areas600_sample_30.RData")


# Model with 5 avg. respondents per areas
Model_5_Two <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                             X1 + A1 + A2 + A3,
                           data = Areas600_sample_5,
                           family = binomial(link = "logit"),
                           prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                           prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                           chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_5_Two, file = "Results/Models/Areas_600/Model_5_Two.rds")
rm(Model_5_Two)

# Models with 10 respondents per areas
Model_10_Two <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_10,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_10_Two, file = "Results/Models/Areas_600/Model_10_Two.rds")
rm(Model_10_Two)

# Models with 20 respondents per areas
Model_20_Two <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_20,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_20_Two, file = "Results/Models/Areas_600/Model_20_Two.rds")
rm(Model_20_Two)

# Models with 30 respondents per areas
Model_30_Two <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                              X1 + A1 + A2 + A3,
                            data = Areas600_sample_30,
                            family = binomial(link = "logit"),
                            prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                            prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                            chains = 2, iter = 1000, QR = TRUE, seed = 12345, adapt_delta = 0.99)
saveRDS(object = Model_30_Two, file = "Results/Models/Areas_600/Model_30_Two.rds")
rm(Model_30_Two)
rm(list=ls(pattern="Areas600"))

## Models with three groups and a ratio of 3:2:1
## Load data
load("Data/Areas_600/3to1/Areas600_sample_5.RData")
load("Data/Areas_600/3to1/Areas600_sample_10.RData")
load("Data/Areas_600/3to1/Areas600_sample_20.RData")
load("Data/Areas_600/3to1/Areas600_sample_30.RData")


# Model with 5 avg. respondents per areas
Model_5_Three <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                            X1 + A1 + A2 + A3,
                          data = Areas600_sample_5,
                          family = binomial(link = "logit"),
                          prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                          prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                          chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_5_Three, file = "Results/Models/Areas_600/Model_5_Three.rds")
rm(Model_5_Three)

# Models with 10 respondents per areas
Model_10_Three <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                             X1 + A1 + A2 + A3,
                           data = Areas600_sample_10,
                           family = binomial(link = "logit"),
                           prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                           prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                           chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_10_Three, file = "Results/Models/Areas_600/Model_10_Three.rds")
rm(Model_10_Three)

# Models with 20 respondents per areas
Model_20_Three <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                             X1 + A1 + A2 + A3,
                           data = Areas600_sample_20,
                           family = binomial(link = "logit"),
                           prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                           prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                           chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_20_Three, file = "Results/Models/Areas_600/Model_20_Three.rds")
rm(Model_20_Three)

# Models with 30 respondents per areas
Model_30_Three <- stan_glmer(Y ~ (1|GSSCode) + (1|X4) + (1|X3) + (1|X2) + 
                             X1 + A1 + A2 + A3,
                           data = Areas600_sample_30,
                           family = binomial(link = "logit"),
                           prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                           prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                           chains = 2, iter = 1000, QR = TRUE, seed = 12345)
saveRDS(object = Model_30_Three, file = "Results/Models/Areas_600/Model_30_Three.rds")
rm(Model_30_Three)
rm(list=ls(pattern="Areas600"))

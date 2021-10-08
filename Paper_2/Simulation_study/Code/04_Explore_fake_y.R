## Code to explore the Fake Y

## Check the distribution of Y == 1
unweighted_Y <- dat %>%
  group_by(GSSCode) %>%
  summarise(Y = mean(Y))
summary(unweighted_Y)
hist(unweighted_Y$Y)

weighted_Y <- dat %>%
  group_by(GSSCode) %>%
  summarise(Y = sum(Y * weight)/ sum(weight))
summary(weighted_Y)
hist(weighted_Y$Y) 
table(dat$Y)
weighted_Y$winner <- ifelse(weighted_Y$Y >=0.5, 1, 0)
table(weighted_Y$winner)

buckets <- c(-Inf, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)
Labels <- c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50-60%", "60-70%", "70-80%", "80-90%", "90-100%")
Grouped_Y <- weighted_Y %>%
  mutate(grouped = cut(Y,
                       breaks = buckets,
                       labels = Labels)) %>%
  group_by(grouped) %>%
  summarise(n())
Grouped_Y

## Now to run a quick mlm model to test whether we are able 
## to predcit Y from the available variables

sample_dat <- dat %>%
  group_by(GSSCode) %>%
  sample_n(50)
mod_test <- glmer(Y ~ (1|GSSCode)  + (1|X4) + (1|X3) + (1|X2) +
                    X1 + A1 + A2 + A3, 
                  data = dat,
                  family = binomial(link = "logit"))
summary(mod_test)
fixef(mod_test) # Check the fixed effects
ranef(mod_test) ## Check the random effects

# Extract the fitted values
dat$pred <- predict(mod_test, newdata = dat, type = "response")
dat$pred <- ifelse(dat$pred >= 0.5, 1, 0)
prop.table(table(dat$Y, dat$pred),2) # Check that the fitted values are the same as actual Y
dat$pred <- fitted.values(mod_test)

#' Weighted constituency estimates
weighted_preds <- dat %>%
  group_by(GSSCode) %>%
  summarise(Y_preds = sum(pred*weight)/sum(weight),
            Y_actual = sum(Y*weight)/sum(weight))
plot(weighted_preds$Y_actual, weighted_preds$Y_preds) # Plotting constituency estimates
plot((dat$Y/1000),fitted.values(mod_test)) # Plotting individual fitted values vs. odds of Y ==1
rm(mod_test, unweighted_Y, weighted_preds, weighted_Y, Grouped_Y)
weighted_preds <- weighted_preds %>%
  mutate(err = abs(Y_preds - Y_actual))
mean(weighted_preds$err)
view(weighted_preds)

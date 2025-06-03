# logit models

library(modelsummary)
library(marginaleffects)
library(tidyverse)
load("class_data/titanic.RData")
load("class_data/politics.RData")
load("class_data/earnings.RData")
load("class_data/popularity.RData")


# logit models of the titanic --------------------------------------------

# change some reference values
titanic$survival <- relevel(titanic$survival, "Died")
titanic$sex <- relevel(titanic$sex, "Male")

model1 <- glm(survival ~ sex + fare, data = titanic, family = binomial)
model2 <- update(model1, .~. + pclass)
model3 <- update(model2, .~. + sex*pclass)

modelsummary(list(model1, model2, model3), stars = TRUE)

modelsummary(list(model1, model2, model3), stars = TRUE,
             exponentiate = TRUE)

prop.table(table(titanic$pclass, titanic$sex), 2)

# LPM of the titanic ---------------------------------------------------

model1_lpm <- lm((survival == "Survived") ~ sex + fare,
                 data = titanic)
model2_lpm <- update(model1_lpm, .~. + pclass)
model3_lpm <- update(model2_lpm, .~. + sex * pclass)

modelsummary(list(model1_lpm, model2_lpm, model3_lpm),
             stars = TRUE)

# logit model of global warming beliefs ---------------------------------

# global warming attitude predictors
levels(politics$globalwarm)

model1 <- glm(globalwarm ~ relig, data = politics,
              family = binomial)
# political party
# education
# income
# age
# race
#model1.5 <- update(model1, .~. + party)
model2 <- update(model1, .~. + age + I(age^2))
model3 <- update(model2, .~. + educ + income)
model4 <- update(model3, .~. + party)

modelsummary(list(model1, model1.5, model2, model3, model4), stars = TRUE)

# marginal effects --------------------------------------------------------

model_wages <- lm(wages ~ age + I(age^2), data = earnings)

model1 <- glm(survival ~ I(sex == "Female") + fare,
              data = titanic, family = binomial)
model2 <- update(model1, .~. + pclass)
model3 <- update(model2, .~. + sex*pclass)

# any marginal effect on the probability from a logit model is given by:
# p * (1-p) * beta
# where:
# p: probability of success at current values of X
# beta: slope (log odds ratio) for variable we are interested in

# so if we wanted to get the marginal effect of fare on the probability of
# survival for a male passenger paying 20 pounds in fare:
# 1. First we calculate the predicted log odds of success
lodds <- predict(model1, newdata = data.frame(fare = 20, sex = "Male"))
# 2. convert to probability
p <- exp(lodds)/(1+exp(lodds))
# 3. apply marginal effects formula
p * (1-p) * coef(model1)["fare"]
# So for a male passenger paying 20 pounds, we expect that paying an additional
# pound of fare would have increased the probability of survival by 0.137%

# the slopes command from the marginal effects package can be used to do the
# same thing, but it also calculates a standard error for this estimate
slopes(model1, variables = "fare",
       newdata =  data.frame(fare = c(20, 100), sex = "Male"))

slopes(model1, variables = "fare",
       newdata =  data.frame(fare = c(20, 100), sex = "Female"))


# what about the marginal effect of gender at this level?
# We do this a little differently by just switching the category of sex
lodds_f <- predict(model1, newdata = data.frame(fare = 20, sex = "Female"))
p_f <- exp(lodds_f)/(1+exp(lodds_f))
# what is the difference?
p_f - p
# At a fare of 20 pounds, a woman's probability of survival was 51.4% higher
# than a man's probability

# we can also do this one with the slopes command
slopes(model1, variables = "sex", newdata =  data.frame(fare = 20))

## Marginal Effects at the Mean (MEM) ##

# what is the marginal effect of fare at the mean value of all x?
mean_fare <- mean(titanic$fare)
mean_sex <- mean(titanic$sex == "Female")
lodds <- predict(model1, newdata = data.frame(fare = mean_fare, sex = mean_sex))
p <- exp(lodds)/(1+exp(lodds))
p*(1-p)*coef(model1)["fare"]

# what is the marginal effect of sex at the mean value of all other x?
lodds <- predict(model1, newdata = data.frame(fare = mean_fare,
                                              sex = c("Male", "Female")))
p <- exp(lodds)/(1+exp(lodds))
diff(p)

avg_slopes(model1, newdata = "mean")
avg_predictions(model1, variables = "sex", newdata = "mean")

## average marginal effects (AME) ##

# for fare
lodds <- predict(model1)
p <- exp(lodds)/(1+exp(lodds))
mean(p*(1-p)*coef(model1)["fare"])

# for gender
titanic_f <- titanic |> mutate(sex = "Female")
titanic_m <- titanic |> mutate(sex = "Male")
lodds_f <- predict(model1, newdata = titanic_f)
p_f <- exp(lodds_f)/(1+exp(lodds_f))
lodds_m <- predict(model1, newdata = titanic_m)
p_m <- exp(lodds_m)/(1+exp(lodds_m))
mean(p_f)
mean(p_m)
mean(p_f)-mean(p_m)

avg_slopes(model1)
avg_predictions(model1, variables = "sex")

# lets calculate AMEs for all three of our models
avg_slopes(model1)
avg_slopes(model2)
avg_slopes(model3)

# notice that the interaction terms disappear in model 3
# but we can get those back out by separating average effects by another
# variable
avg_slopes(model3, variables = "sex", by = "pclass")
avg_slopes(model3, variables = "pclass", by = "sex")

# compare to model 2
avg_slopes(model2, variables = "sex", by = "pclass")
avg_slopes(model2, variables = "pclass", by = "sex")

# in model 2 the only differences across groups were just do differences in
# distributions across the other variables, while in model 3 we explicitly allow
# the effects to be different. Notice also that the AMEs for sex were bigger
# in second class than first. Why would this be the case?


# we can use marginal effects in all kinds of interesting ways:
# even for basic linear models, marginal effects can be useful for complex stuff
# on the right hand side. Lets take the polynomial case:
model_wages_poly <- lm(wages ~ age + I(age^2), data = earnings)

avg_slopes(model_wages_poly)
slopes(model_wages_poly, newdata = data.frame(age = c(20, 30, 40, 50, 60)))
predictions(model_wages_poly, newdata = data.frame(age = c(20, 30, 40, 50, 60)))


model_wages_log <- lm(wages ~ log(age), data = earnings)

avg_slopes(model_wages_log)
slopes(model_wages_log, newdata = data.frame(age = c(20, 30, 40, 50, 60)))
predictions(model_wages_log, newdata = data.frame(age = c(20, 30, 40, 50, 60)))

# if you use a glm to log DV you can even recover marginal effects on the
# original scale
model_wages_lm <- lm(log(wages)~log(age), data = earnings)

model_wages_ldv <- glm(wages ~ log(age), data = earnings,
                       family = gaussian(link = "log"))
modelsummary(list(model_wages_lm, model_wages_ldv))
coef(model_wages_ldv)
avg_slopes(model_wages_ldv)

# the elasticity model says that a 1% increase in age is associated with about
# a 0.45% increase in wages. Across the whole sample this works out to be about
# $0.286/hour more for a one year increase in age


# Smoking! ----------------------------------------------------------------

model1 <- glm(smoker ~ I(grade-9) + alcohol_use + gender * honor_society,
              data = popularity, family = binomial)

summary(model1)

model2 <- glm(smoker ~ I(grade-9) + alcohol_use + gender + honor_society,
              data = popularity, family = binomial)

model3 <- glm(smoker ~ I(grade-9) + alcohol_use + gender + honor_society+nsports,
             data = popularity, family = binomial)

model4 <- glm(smoker ~ I(grade-9) + alcohol_use + gender + honor_society+nsports +
                race,
              data = popularity, family = binomial)

modelsummary(list(model1, model2, model3, model4), stars = TRUE)

BIC.null.glm <- function(model) {
  n <- length(model$resid)
  p <- length(model$coef)-1
  return((model$deviance-model$null.deviance)+p*log(n))
}

BIC.null.glm(model1)
BIC.null.glm(model2)

BIC.null.glm(model2)-BIC.null.glm(model1)

(deviance(model2) - deviance(model1)) + (-1) * log(nrow(popularity))

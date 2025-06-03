library(VGAM)
library(MASS)
library(nnet)
library(modelsummary)
library(tidyverse)


load("class_data/politics.RData")

levels(politics$gaymarriage)
politics$gender <- relevel(politics$gender, "Female")

model_mnom1 <- multinom(gaymarriage ~ age + gender + race, data = politics)
model_mnom2 <- update(model_mnom1, .~. + relig)
model_mnom3 <- update(model_mnom2, .~. + party)


modelsummary(list(model_mnom1, model_mnom2, model_mnom3),
             shape = term + statistic ~ model + response,
             stars = TRUE)

model_mnom1_vglm <- vglm(cbind(gaymarriage == "No legal recognition",
                               gaymarriage == "Civil unions",
                               gaymarriage == "Support gay marriage") ~ age + gender + race,
                         data = politics,
                         family = multinomial(refLevel = 1))
model_mnom2_vglm <- update(model_mnom1_vglm, .~. + relig)
model_mnom3_vglm <- update(model_mnom2_vglm, .~. + party)


modelsummary(list(model_mnom1_vglm, model_mnom2_vglm, model_mnom3_vglm),
             stars = TRUE)


model_acat1_vglm <- vglm(cbind(gaymarriage == "No legal recognition",
                               gaymarriage == "Civil unions",
                               gaymarriage == "Support gay marriage") ~ age + gender + race,
                         data = politics,
                         family = acat(parallel = FALSE))
model_acat2_vglm <- update(model_acat1_vglm, .~. + relig)
model_acat3_vglm <- update(model_acat2_vglm, .~. + party)

modelsummary(list(model_acat1_vglm, model_acat2_vglm, model_acat3_vglm),
             stars = TRUE)

model_cuml1_vglm <- vglm(cbind(gaymarriage == "No legal recognition",
                               gaymarriage == "Civil unions",
                               gaymarriage == "Support gay marriage") ~ age + gender + race,
                         data = politics,
                         family = cumulative(parallel = FALSE, reverse = TRUE))
model_cuml2_vglm <- update(model_cuml1_vglm, .~. + relig)
model_cuml3_vglm <- update(model_cuml2_vglm, .~. + party)

modelsummary(list(model_cuml1_vglm, model_cuml2_vglm, model_cuml3_vglm),
             stars = TRUE)

model_ordl1_vglm <- vglm(cbind(gaymarriage == "No legal recognition",
                               gaymarriage == "Civil unions",
                               gaymarriage == "Support gay marriage") ~ age + gender + race,
                         data = politics,
                         family = cumulative(parallel = TRUE, reverse = TRUE))
model_ordl2_vglm <- update(model_ordl1_vglm, .~. + relig)
model_ordl3_vglm <- update(model_ordl2_vglm, .~. + party)

modelsummary(list(model_ordl1_vglm, model_ordl2_vglm, model_ordl3_vglm),
             stars = TRUE)

model_ordl1 <- polr(gaymarriage ~ age + gender + race, data = politics)
model_ordl2 <- update(model_ordl1, .~. + relig)
model_ordl3 <- update(model_ordl2, .~. + party)

modelsummary(list(model_ordl1, model_ordl2, model_ordl3),
             stars = TRUE)

model_linear1 <- lm(as.numeric(gaymarriage) ~ age + gender + race, data = politics)
model_linear2 <- update(model_ordl1, .~. + relig)
model_linear3 <- update(model_ordl2, .~. + party)

modelsummary(list(model_linear1, model_linear2, model_linear3),
             stars = TRUE)


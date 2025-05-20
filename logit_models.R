# logit models

library(modelsummary)
load("class_data/titanic.RData")
load("class_data/politics.RData")

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

# LPM
model1_lpm <- lm((survival == "Survived") ~ sex + fare,
                 data = titanic)
model2_lpm <- update(model1_lpm, .~. + pclass)
model3_lpm <- update(model2_lpm, .~. + sex * pclass)

modelsummary(list(model1_lpm, model2_lpm, model3_lpm),
             stars = TRUE)

# global warming attitude predictors
levels(politics$globalwarm)

model1 <- glm(globalwarm ~ relig, data = politics,
              family = binomial)

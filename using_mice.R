library(mice)
library(tidyverse)
library(survey)
library(broom)
load("class_data/addhealth.RData")


# Chained equations -------------------------------------------------------

# lets remove sampling stuff
imputed <- addhealth |>
  select(-cluster, -sweight) |>
  mice(1)

addhealth_complete <- complete(imputed, 1) |>
  as_tibble() |>
  add_column(cluster = addhealth$cluster,
             sweight = addhealth$sweight)

imputed_linear <- addhealth |>
  select(-cluster, -sweight) |>
  mice(1, defaultMethod = c("norm", "logreg", "polyreg", "polr"))

summary(complete(imputed, 1)$pseudo_gpa)
summary(complete(imputed_linear, 1)$pseudo_gpa)

# Full imputation ------------------------------------------------------

## using pool

imputations <- addhealth |>
  select(-cluster, -sweight) |>
  mice(5)

models <- with(imputations, lm(nominations~parent_income+nsports))
model_pooled <- pool(models)
summary(model_pooled)
glance(model_pooled)

## doing it by hand
b <- NULL
se <- NULL
for(i in 1:5) {
  design <- svydesign(ids = ~ addhealth$cluster,
                      weights = ~ addhealth$sweight,
                      data = complete(imputations, i))
  model <- svyglm(nominations ~ parent_income + nsports, design)
  model <- tidy(model)
  b <- cbind(b, model$estimate)
  se <- cbind(se, model$std.error)
}

b_pool <- apply(b, 1, mean)
between_var <- apply(b, 1, var)
within_var <- apply(se^2, 1, mean)
se_pool <- sqrt(within_var + between_var + between_var / 5)
tstat <- b_pool / se_pool

models <- map(1:5, function(i) {

  design <- svydesign(ids = ~ addhealth$cluster,
                      weights = ~ addhealth$sweight,
                      data = complete(imputations, i))

  model <- svyglm(nominations ~ parent_income + nsports, design)
  tidy(model)
})






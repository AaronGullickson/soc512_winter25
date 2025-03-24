# Week 10
# Interaction terms

library(tidyverse)
library(modelsummary)

load("class_data/movies.RData")



# Quant-Categorical Interaction -------------------------------------------

model1 <- lm(metascore ~ runtime*maturity_rating+box_office+year,
            data = movies)

model2 <- lm(metascore ~ I(year-2000)*maturity_rating,
            data = movies)

modelsummary(model2, stars = TRUE)

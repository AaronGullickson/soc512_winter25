# Models in R
# Soc 412/512
# Aaron Gullickson
# Winter 2025

library(tidyverse)
library(modelsummary)
load("class_data/movies.RData")

# Specifying models -------------------------------------------------------

model1 <- lm(metascore ~ I(year-2000) + maturity_rating, data = movies)
model2 <- update(model1, . ~ . + genre)
model3 <- update(model2, . ~ . + box_office + runtime)

# Contrasts ---------------------------------------------------------------

contrasts(movies$genre)

# modelsummary ------------------------------------------------------------

modelsummary(list(model1, model2, model3))


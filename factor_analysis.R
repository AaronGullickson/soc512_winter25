# Fun with factor analysis

library(tidyverse)
library(psych)
library(here)
library(corrgram)

load(here("class_data", "pew.RData"))

pew <- pew |>
  select(starts_with("moral_")) |>
  mutate_all(as.numeric)

pew |>
  alpha()

pew_r <- cor(pew, use = "pairwise.complete.obs")
corrgram(pew_r, order = TRUE, lower.panel = "panel.cor")

fa_model1 <- fa(pew_r, 1)
fa_model2 <- fa(pew_r, 2)
fa_model3 <- fa(pew_r, 3)

loadings(fa_model1)
loadings(fa_model2)
loadings(fa_model3)

fa.diagram(fa_model1)
fa.diagram(fa_model2)
fa.diagram(fa_model3)

pew |>
  select(-moral_divorce, -moral_fertility, -moral_polygamy) |>
  alpha()

pew_r <- pew |>
  select(-moral_divorce, -moral_fertility, -moral_polygamy) |>
  cor(use = "pairwise.complete.obs")

fa_model1 <- fa(pew_r, 1)
fa_model2 <- fa(pew_r, 2)

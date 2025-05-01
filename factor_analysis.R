# Fun with factor analysis

library(tidyverse)
library(psych)
library(here)

load(here("class_data", "pew.RData"))

pew <- pew |>
  select(starts_with("moral_")) |>
  mutate_all(as.numeric)

pew |>
  select(starts_with("moral_")) |>
  alpha()

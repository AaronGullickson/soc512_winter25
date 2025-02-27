############################
# Week 8                   #
# Reshape/Merge Data       #
# Soc 412/512 Winter 2025  #
# Aaron Gullickson         #
############################

library(tidyverse)
library(ggvenn)

# Read in data ------------------------------------------------------------

load("class_data/earnings.RData")

world_bank <- read_csv("class_data/world_bank/world_bank.csv",
                       n_max = 651,
                       na = "..",
                       skip = 1,
                       col_names = c("country_name", "country_code",
                                     "series_name", "series_code",
                                     "year2018", "year2019"))


vdem <- read_csv("class_data/vdem/V-Dem-CY-Full+Others-v13.csv.gz") |>
  select(country_name, country_text_id, year, v2x_libdem) |>
  filter(year == 2018 | year ==2019)


# Fix World Bank data -----------------------------------------------------

world_bank <- world_bank |>
  select(!series_name) |>
  mutate(series_code = case_when(
    series_code == "NY.GDP.MKTP.CD" ~ "gdp",
    series_code == "SP.DYN.LE00.IN" ~ "life_exp",
    series_code == "EN.ATM.CO2E.PC" ~ "co2_percap"
  )) |>
  pivot_longer(cols = starts_with("year"), names_prefix = "year",
               names_to = "year") |>
  pivot_wider(names_from = series_code, values_from = value)


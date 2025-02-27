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
  pivot_wider(names_from = series_code, values_from = value) |>
  mutate(year = as.numeric(year))


# Merge world bank and VDEM -----------------------------------------------

# inspect keys

country_name_wb <- unique(world_bank$country_name)
country_name_vdem <- unique(vdem$country_name)

list(WorldBank = country_name_wb, VDEM = country_name_vdem) |>
  ggvenn(auto_scale = TRUE, fill_color = c("navy","seagreen"))

c("Bob", "Mary") %in% c("Bob", "Harry", "June")
country_name_wb[!(country_name_wb %in% country_name_vdem)]
country_name_vdem[!(country_name_vdem %in% country_name_wb)]

country_code_wb <- unique(world_bank$country_code)
country_code_vdem <- unique(vdem$country_text_id)

list(WorldBank = country_code_wb, VDEM = country_code_vdem) |>
  ggvenn(auto_scale = TRUE, fill_color = c("navy","seagreen"))

country_name_vdem[!(country_code_vdem %in% country_code_wb)]
country_name_wb[!(country_code_wb %in% country_code_vdem)]

vdem <- vdem |>
  rename(country_code = country_text_id)

# full_join - all data in both original datasets
temp <- full_join(world_bank, vdem, by = c("country_code", "year"))

# what happens if you accidentally also merge on country_name BAD
#temp <- full_join(world_bank, vdem)

# inner_join - only data that matches
temp <- inner_join(world_bank, vdem, by = c("country_code", "year"))

# left_join - only keep data from first dataset
temp <- left_join(world_bank, vdem, by = c("country_code", "year"))

# you can also pipe
full_data <- world_bank |>
  left_join(vdem, by = c("country_code", "year"))

full_data <- vdem |>
  select(!country_name) |>
  right_join(world_bank)

# Aggregate and reshape ---------------------------------------------------

earnings |>
  group_by(education, gender) |>
  summarize(mean_wages = mean(wages),
            n = n()) |>
  ungroup() |>
  pivot_wider(names_from = gender, values_from = c(mean_wages, n)) |>
  mutate(wage_diff = mean_wages_Male - mean_wages_Female,
         wage_ratio = mean_wages_Female / mean_wages_Male)

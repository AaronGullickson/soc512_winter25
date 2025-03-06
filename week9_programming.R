############################
# Week 9                   #
# Programming              #
# Soc 412/512 Winter 2025  #
# Aaron Gullickson         #
############################


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Load data ---------------------------------------------------------------

tracts <- read_csv("class_data/social_explorer/R13598833_SL140.csv",
                   col_types = cols(.default = "i",
                                    Geo_QName = "c",
                                    Geo_NAME = "c",
                                    Geo_STUSAB = "c",
                                    Geo_FIPS = "c")) |>
  mutate(pop_race_indigenous = SE_B04001_005 + SE_B04001_007,
         county_id = Geo_STATE * 1000 + Geo_COUNTY,
         county_name = str_remove(Geo_QName, paste0(Geo_NAME, ", ")),
         tract_id = as.numeric(Geo_FIPS)) |>
  rename(pop_total = SE_B04001_001,
         pop_race_white = SE_B04001_003,
         pop_race_black = SE_B04001_004,
         pop_race_asian = SE_B04001_006,
         pop_race_other = SE_B04001_008,
         pop_race_multi = SE_B04001_009,
         pop_race_latino = SE_B04001_010) |>
  select(tract_id, starts_with("county_"), starts_with("pop_")) |>
  filter(pop_total > 0)

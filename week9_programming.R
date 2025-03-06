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

# Calculate Theil's H for a case ------------------------------------------

tracts_sanb <- tracts |> filter(county_name == "San Bernardino County, California")

# reshape for easier processing
tracts_sanb <- tracts_sanb |>
  pivot_longer(cols = starts_with("pop_race_"),
               names_prefix = "pop_race_",
               names_to = "race", values_to = "pop")

county <- tracts_sanb |>
  group_by(county_id, race) |>
  summarize(pop = sum(pop), pop_total = sum(pop_total)) |>
  mutate(prop = pop / pop_total,
         e = prop * log(1 / prop, 7)) |>
  group_by(county_id) |>
  summarize(e = sum(e, na.rm = TRUE),
            pop_total = sum(pop))

tracts_sanb <- tracts_sanb |>
  mutate(prop = pop / pop_total,
         e = prop * log(1 / prop, 7)) |>
  group_by(tract_id) |>
  summarize(e = sum(e, na.rm = TRUE),
            pop_total = sum(pop))

1-sum(tracts_sanb$pop_total * tracts_sanb$e) / (county$pop_total * county$e)

# Basic functions ---------------------------------------------------------
y <- 5

print_value <- function(x, exclamation = FALSE) {
  if(exclamation) {
    return("!!!!!!!!!!!!!!!!")
  }
  return(print(x))
}


# Function for Theil h ----------------------------------------------------

calc_theil_h <- function(tracts_county) {

  tracts_county <- tracts_county |>
    pivot_longer(cols = starts_with("pop_race_"),
                 names_prefix = "pop_race_",
                 names_to = "race", values_to = "pop")

  county <- tracts_county |>
    group_by(county_id, race) |>
    summarize(pop = sum(pop), pop_total = sum(pop_total),
              .groups = "drop_last") |>
    mutate(prop = pop / pop_total,
           e = prop * log(1 / prop, 7)) |>
    group_by(county_id) |>
    summarize(e = sum(e, na.rm = TRUE),
              pop_total = sum(pop),
              .groups = "drop_last") |>
    ungroup()

  tracts_county <- tracts_county |>
    mutate(prop = pop / pop_total,
           e = prop * log(1 / prop, 7)) |>
    group_by(tract_id) |>
    summarize(e = sum(e, na.rm = TRUE),
              pop_total = sum(pop)) |>
    ungroup()

  1-sum(tracts_county$pop_total * tracts_county$e) /
    (county$pop_total * county$e)

}

# try it out!

tracts |>
  filter(county_name == "San Bernardino County, California") |>
  calc_theil_h()

tracts |>
  filter(county_name == "King County, Washington") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Lewis County, Washington") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Broward County, Florida") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Wells County, Indiana") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Wayne County, Michigan") |>
  calc_theil_h()

tracts |>
  filter(county_name == "Cook County, Illinois") |>
  calc_theil_h()


# For loop ----------------------------------------------------------------

county_names <- unique(tracts$county_name)

for(i in 1:length(county_names)) {
  print(county_names[i])
}

for(county_name in county_names) {
  print(county_name)
}

theil_h <- NULL
for(name in county_names) {
  h <- tracts |>
    filter(county_name == name) |>
    calc_theil_h()
  theil_h <- theil_h |>
    bind_rows(tibble(county_name = name, h))
}

# Mapping -----------------------------------------------------------------

# lapply or map (from the purrr package)
tracts_list <- tracts |>
  group_by(county_id, county_name) |>
  group_split()

# map_dbl(tracts_list, calc_theil_h)

theil_h <- map(tracts_list, function(x) {
  h <- calc_theil_h(x)
  county_id <- x$county_id[1]
  county_name <- x$county_name[1]
  tibble(county_id, county_name, theil_h = h)
}) |>
  bind_rows()


# Compare how long with system.time ---------------------------------------

theil_h <- NULL
system.time(
  for(name in county_names) {
    h <- tracts |>
      filter(county_name == name) |>
      calc_theil_h()
    theil_h <- theil_h |>
      bind_rows(tibble(county_name = name, h))
  }
)

system.time(
  theil_h <- map(tracts_list, function(x) {
    h <- calc_theil_h(x)
    county_id <- x$county_id[1]
    county_name <- x$county_name[1]
    tibble(county_id, county_name, theil_h = h)
  }) |>
    bind_rows()
)

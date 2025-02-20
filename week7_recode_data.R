############################
# Week 7                   #
# Recoding Data            #
# Soc 412/512 Winter 2025  #
# Aaron Gullickson         #
############################


# Load libraries ----------------------------------------------------------

library(tidyverse)

# Read ACS data -----------------------------------------------------------

acs <- read_fwf("class_data/ipums_data/usa_00131.dat.gz",
                col_positions = fwf_cols(SEX     = c(53, 53),
                                         AGE     = c(54, 56),
                                         MARST   = c(57, 57),
                                         RACE    = c(58, 58),
                                         HISPAN  = c(62, 62),
                                         HCOVANY = c(66, 66),
                                         EDUCD   = c(69, 71),
                                         SEI     = c(72, 73)),
                col_types = cols(.default = "i"))

# example of selecting and renaming
#acs <- acs |>
#  rename(gender = SEX, age = AGE, marst = MARST) |>
#  select(gender, age, marst, SEI, starts_with("R"), ends_with("N"))


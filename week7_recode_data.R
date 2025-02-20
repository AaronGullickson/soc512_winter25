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

# Using ifelse to insert values -------------------------------------------

# we need to convert zero values in the SEI variable into NA values
# acs$sei <- ifelse(acs$SEI ==0, NA, acs$SEI)

# lets do this via mutating!
acs <- acs |>
  mutate(sei = ifelse(SEI == 0, NA, SEI))

# lets check ourselves
table(acs$SEI, is.na(acs$sei), exclude=NULL)

# Encoding a factor variable ----------------------------------------------

# start with sex
acs <- acs |>
  mutate(sex = factor(SEX, levels = 1:2, labels = c("Male", "Female")))

# check ourselves
table(acs$SEX, acs$sex, exclude = NULL)

# more complicated variable using case_when

acs <- acs |>
  mutate(mar_stat = case_when(MARST == 1 | MARST == 2 ~ "Married",
                              MARST == 3 | MARST == 4 ~ "Separated/Divorced",
                              MARST == 5 ~ "Widowed",
                              MARST == 6 ~ "Never Married",
                              TRUE ~ NA),
         mar_stat = factor(mar_stat,
                           levels = c("Never Married", "Married",
                                      "Separated/Divorced", "Widowed")))

# check ourselves
table(acs$MARST, acs$mar_stat, exclude = NULL)




# Create final analytical data --------------------------------------------

analytical_data <- acs |>
  select(sei, sex)

save(analytical_data, file = "analytical_data.RData")

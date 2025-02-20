# Reading in Data
# Soc 412/512
# Aaron Gullickson
# Winter 2025

# loading libraries -------------------------------------------------------

# we want the package `readr`
library(tidyverse)
library(haven)
library(readxl)

# Use relative paths! ------------------------------------------------------

relative_path <- "class_data/popularity.RData"
load(relative_path)

# DON"T DO THIS!!
absolute_path <- "/Users/aarong/Desktop/soc512_winter25/class_data/popularity.RData"
load(absolute_path)

# OR THIS!!
absolute_path_tilde <- "~/Desktop/soc512_winter25/class_data/popularity.RData"
load(absolute_path_tilde)


# Reading in delimited text files -----------------------------------------

sports <- read_csv("makebelieve_data/sports.csv",  # relative path
                   col_names = TRUE,               # file has headers
                   na = c("na", "NA", "..", "."),  # missing value codes
                   n_max = 6,                      # stop reading after n obs
                   #skip = 2,                      # skip lines at the top
                   comment = "#")                  # ignore lines starting with comment char

# Reading in fixed-width text files ---------------------------------------

# width, and starting, ending position for each variable
# sport = 21 (1, 21)
# team = 3 (22, 24)
# fun_watch = 7 (25, 31)
# number_players = 2 (32, 33)

sports <- read_fwf("makebelieve_data/sports.txt",
                   na = "..",
                   col_positions = fwf_widths(widths = c(21, 3, 7, 2),
                                              col_names = c("sports",
                                                            "team",
                                                            "fun_watch",
                                                            "number_players")))

sports <- read_fwf("makebelieve_data/sports.txt",
                   na = "..",
                   col_positions = fwf_cols(
                     sports = c(1, 21),
#                     team = c(22, 24),
                     fun_watch = c(25, 31),
                     number_players = c(32, 33)
                   ))

# Read in binary data -----------------------------------------------------

# write our data as dta
write_dta(sports, path = "makebelieve_data/sports.dta")

# read in a dta
sports <- read_dta("makebelieve_data/sports.dta")

# read in an excel file
sports <- read_excel("makebelieve_data/sports.xlsx",
                     skip = 1, na = "..", sheet = 1)

# Saving and loading RData ------------------------------------------------

save(sports, file = "makebelieve_data/sports.RData")

load("makebelieve_data/sports.RData")


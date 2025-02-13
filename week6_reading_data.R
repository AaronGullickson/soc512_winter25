# Reading in Data
# Soc 412/512
# Aaron Gullickson
# Winter 2025

# loading libraries -------------------------------------------------------

# we want the package `readr`
library(tidyverse)
library(haven)


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

sports <- read_fwf("makebelieve_data/sports.txt")

# Making Pictures
# Soc 412/512
# Winter 2025


# Load stuff --------------------------------------------------------------

# load packages
library(tidyverse)
#library(ggplot2) - don't need this because its party of the tidyverse

# load data
load(url("https://github.com/AaronGullickson/practical_analysis/raw/master/data/nyc.RData"))


# Make a picture ----------------------------------------------------------

ggplot(nyc, aes(x = poverty, y = amtcapita))+
  geom_point(mapping = aes(color = borough, size = popn), alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, color = "black", alpha = 0.6)+
  scale_y_log10()

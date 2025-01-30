# Making Pictures
# Soc 412/512
# Winter 2025


# Load stuff --------------------------------------------------------------

# load packages
library(tidyverse)
#library(ggplot2) - don't need this because its party of the tidyverse

# fun color palettes
library(wesanderson)
library(MoMAColors)

# load data
load(url("https://github.com/AaronGullickson/practical_analysis/raw/master/data/nyc.RData"))
load("class_data/movies.RData")


# Make a picture ----------------------------------------------------------

ggplot(nyc, aes(x = poverty, y = amtcapita))+
  geom_point(mapping = aes(color = borough, size = popn), alpha = 0.5)+
  geom_smooth(method = "lm", se = FALSE, color = "black")+
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000, 10000),
                labels = c("$0.1", "$1", "$10", "$100", "$1,000", "$10,000"))+
  #scale_color_brewer(palette = "Pastel1")+
  #scale_color_viridis_d()+
  #scale_color_manual(values = c("lemonchiffon3", "thistle", "turquoise4",
  #                              "violet", "goldenrod"))+
  scale_color_manual(values = wes_palette("AsteroidCity1"))+
  #scale_color_manual(values = moma.colors("OKeeffe"))+
  scale_x_continuous(labels = scales::percent)+
  #facet_wrap(~borough)+
  labs(x = "poverty rate",
       y = "amount of social service funding per capita",
       size = "population",
       #title = "This is a title", subtitle = "this is a subtitle",
       caption = "Source: New York City Contract Data 1997-2001")+
  theme_bw()+
  #theme(panel.grid = element_line(color = "tomato"))
  theme(panel.grid = element_blank(),
        legend.position = "right")#+
  #theme(text = element_text(family = "Kablammo-Regular"))


ggplot(movies, aes(x=reorder(genre, runtime, median), y=runtime))+
  geom_boxplot(fill="grey", outlier.color = "red")+
  labs(x=NULL, y="movie runtime (minutes)")+
  coord_flip()+
  theme_bw()


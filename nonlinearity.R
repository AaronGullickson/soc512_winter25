# Nonlinearity

library(tidyverse)
library(broom)

load("class_data/movies.RData")

ggplot(movies, aes(x = metascore, y = box_office))+
  geom_point(alpha = 0.3)+
  geom_smooth(se = FALSE)+
  labs(x = "metascore rating", y = "box office returns")+
  theme_bw()

ggplot(movies, aes(x = rating_imdb, y = box_office))+
  geom_jitter(alpha = 0.3)+
  geom_smooth(se = FALSE)+
  geom_smooth(method = "loess", color = "goldenrod",
              se = FALSE, span = 1, size = 2)+
  geom_smooth(method = "loess", color = "slateblue",
              se = FALSE, span = 0.25, size = 2)+
  geom_smooth(method = "loess", color = "plum",
              se = FALSE, size =2)+
  geom_smooth(method = "lm", se = FALSE,
              color = "darkgreen", size = 2)+
  labs(x = "IMDB rating", y = "box office returns")+
  theme_bw()

model <- lm(box_office ~ rating_imdb, data = movies)
summary(model)

ggplot(augment(model), aes(x = .fitted, y = .resid))+
  geom_point()+
  geom_smooth(se = FALSE, size = 3)+
  geom_hline(yintercept = 0, linetype = 2)+
  labs(x = "predicted box office returns",
       y = "model residuals")+
  theme_bw()


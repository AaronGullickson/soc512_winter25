# Nonlinearity

library(tidyverse)
library(broom)
library(modelsummary)

load("class_data/movies.RData")
load("class_data/earnings.RData")


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

model <- lm(wages ~ log(age), data = earnings)
pred_df <- tibble(age = 18:65)
pred_wages <- predict(model, newdata = pred_df)
pred_wages <- tibble(age = pred_df$age, wages = pred_wages)

model_elasticity <- lm(log(wages) ~ log(age), data = earnings)
pred_wages_elastic <- predict(model_elasticity, newdata = pred_df)
pred_wages_elastic <- tibble(age = pred_df$age,
                             wages = exp(pred_wages_elastic))

ggplot(earnings, aes(x=age, y=wages))+
  geom_jitter(alpha=0.01, width=1)+
  geom_smooth(se=FALSE,
              method="lm",
              formula=y~x+I(x^2), , size =2)+
  geom_smooth(se=FALSE,
              method="lm", color = "purple", size =2)+
  geom_line(data = pred_wages, color = "yellow", size =2)+
  geom_line(data = pred_wages_elastic, color = "darkgreen", size =2)+
  geom_smooth(se = FALSE, color = "red", size =2)+
  labs(x="age", y="hourly wages")+
  theme_bw()


# splines

earnings <- earnings |>
  mutate(spline35 = ifelse(age < 35, 0, age - 35),
         spline40 = ifelse(age < 40, 0, age - 40),
         spline45 = ifelse(age < 45, 0, age - 45))

model_baseline <- lm(wages ~ age, data = earnings)
model_spline35 <- update(model_baseline, .~. + spline35)
model_spline40 <- update(model_baseline, .~. + spline40)
model_spline45 <- update(model_baseline, .~. + spline45)
model_squared <- update(model_baseline, .~. + I(age^2))

modelsummary(list(model_spline35, model_spline40, model_spline45,
                  model_squared))

# lets graph predicted values from the model
pred_df <- pred_df |>
  mutate(spline35 = ifelse(age < 35, 0, age - 35),
         spline40 = ifelse(age < 40, 0, age - 40),
         spline45 = ifelse(age < 45, 0, age - 45))

wages_pred35 <- predict(model_spline35, newdata = pred_df)
wages_pred40 <- predict(model_spline40, newdata = pred_df)
wages_pred45 <- predict(model_spline45, newdata = pred_df)
wages_predsq <- predict(model_squared, newdata = pred_df)

pred35 <- tibble(age = 18:65, wages = wages_pred35)
pred40 <- tibble(age = 18:65, wages = wages_pred40)
pred45 <- tibble(age = 18:65, wages = wages_pred45)
predsq <- tibble(age = 18:65, wages = wages_predsq)

ggplot(earnings, aes(x = age, y = wages))+
  geom_jitter(alpha = 0.02)+
  geom_line(data = pred35, color = "tomato", linewidth = 2, alpha =0.7)+
  geom_line(data = pred40, color = "turquoise", linewidth = 2, alpha =0.7)+
  geom_line(data = pred45, color = "royalblue", linewidth = 2, alpha =0.7)+
  geom_line(data = predsq, color = "wheat", linewidth = 2, alpha =0.7)+
  theme_bw()



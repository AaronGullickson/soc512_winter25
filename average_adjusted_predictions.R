# From a logit model, we can calculate "average adjusted predictions" - the
# predicted values of y across a variable or set of variables we care about
# accounting for differences across other variables in the model.

library(tidyverse)
library(marginaleffects)
load("class_data/politics.RData")


# lets take the example of religion predicting beliefs about climate change

# lets start by just estimating proportions of those who believe in man
# made climate change by religion using a basic group_by and summarize
politics |>
  group_by(relig) |>
  summarize(mean(globalwarm == "Yes"))

# now lets do this using a logit model
model1 <- glm(globalwarm ~ relig, data = politics, family = binomial)

# we can use the avg_predictions command to get predicted proportions for
# each religious group from our model
avg_predictions(model1, variables = "relig")

# notice that these results are identical because our model is simple and has
# no other covariates

# we could also get the marginal effects from our models
avg_slopes(model1, variables = "relig")

# if you compare these marginal effects, you will see that they are just the
# differences you would get by subtracting the mainline protest proportion from
# each of the other proportions. Nothing fancy here.

# now lets control for party affiliation. We know this differs by groups
# and also that it predicts climate change belief, so I would expect it to
# reduce those overall differences across groups
model2 <- update(model1, .~. +party)

# now lets calculage average adjusted predictions
avg_predictions(model2, variables = "relig")

# these numbers are different. It may not be immediately obvious but the numbers
# are closer to each other than they were before. Lets try visualizing this:

pred_m1 <- avg_predictions(model1, variables = "relig") |> as_tibble() |>
  mutate(control = "none")
pred_m2 <- avg_predictions(model2, variables = "relig") |> as_tibble() |>
  mutate(control = "party")
pred_m1 |>
  bind_rows(pred_m2) |>
  ggplot(aes(x = relig, y = estimate, color = control, group = control, ymin = conf.low, ymax = conf.high))+
  geom_point(position = position_dodge(width = 0.25))+
  geom_linerange(alpha = 0.5, position = position_dodge(width = 0.25))+
  scale_color_manual(values = c("red", "navy"))+
  scale_y_continuous(labels = scales::percent)+
  coord_flip()+
  labs(x = NULL, y = "predicted percent who believe in man-made climate change")+
  theme_bw()

# Overall the plot shows that the groups got closer to each other in the model
# that controls for party (blue). What is happening here? In the party model,
# instead of giving each religious group the actual distribution of party
# affiliation specific to their group, we have given them the party affiliation
# of the entire sample. So its a counterfactual where we take away their party
# affiliation differences. You can see that this makes the most liberal groups
# (Jewish, Non-Religious, Other) move to lower values of belief and the most
# conservative groups (Protestants) move to higher values, which over all reduces
# differences across religious groups.

# Technically, we are also giving each religious group the distribution of party
# affiliation across the whole population in the first (red) model as well but
# because the first model did not include party affiliation as a predictor, this
# does not affect the outcomes.

# Notice that if you use the "by" argument rather than the "variables" argument,
# you will get *unadjusted* average predictions:
avg_predictions(model2, by = "relig")

# so be careful to use variables and not by. by can be useful if you have a
# an interaction and want to see the effect of one variable across the other
# interacted variable
model3 <- glm(globalwarm ~ military * relig, data = politics, family = binomial)
# the effect of military service across different religious groups
avg_slopes(model3, variables = "military", by = "relig")
# the differences across religious groups across different military status
avg_slopes(model3, variables = "relig", by = "military")

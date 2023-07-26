# Reproducible example of glmer spline predict problem

library(tibble)
library(dplyr)
library(splines)
library(lme4)

## Simulate data
set.seed(1234L)

n_groups <- 100L

num_per_group <- rpois(n_groups, 50L) + 1L

group_mean <- rnorm(n_groups, sd = 0.2)

x_vals <- c(-20:19)

simulated_data <- tibble(x = sample(x_vals,
                                    sum(num_per_group),
                                    replace = TRUE),
                         group_no = rep(c(1:n_groups),
                                        num_per_group),
                         group_mean = rep(group_mean,
                                          num_per_group)) %>%
  arrange(x, group_no) %>%
  mutate(lin_pred = 3 - 0.05*abs(x) + group_mean,
         y = exp(lin_pred + rnorm(sum(num_per_group),
                                  mean = 0, sd = 0.1)),
         y = round(y))

# Fit model

glmer_spline <- glmer(y ~ bs(x,
                             knots = c(0),
                             degree = 1) +
                        (1 | group_no),
                      data = simulated_data,
                      family = poisson(link = "log"),
                      control = glmerControl(optimizer="bobyqa")
)

summary(glmer_spline)

# This throws an error:
# Error in `[.data.frame`(fr, vars) : undefined columns selected

prediction_output_splines <- predict(glmer_spline,
                                     newdata = simulated_data,
                                     type = "response",
                                     allow.new.levels = TRUE,
                                     re.form=~0)

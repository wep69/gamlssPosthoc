# Example 5 ---------------------------------------------------------------
# Polynomial contrasts among discrete ordered dose levels.
# Use this when the scientific question concerns ordered treatment means.

library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(505)
doses <- c(0, 25, 50, 100)  # unequally spaced
D <- expand.grid(
  block = factor(1:6),
  dose_num = doses,
  rep = 1:5
)
D$dose_factor <- factor(D$dose_num, levels = doses, ordered = TRUE)
mu <- exp(1.6 + 0.012 * D$dose_num - 0.00008 * D$dose_num^2)
D$response <- rGA(nrow(D), mu = mu, sigma = 0.32)

fit <- gamlss(response ~ dose_factor, family = GA, data = D, trace = FALSE)

# Because doses are not equally spaced, use opoly with the actual numeric scores.
ans <- gamlss_posthoc(
  fit,
  specs = "dose_factor",
  estimand = "parameter",
  what = "mu",
  contrast = "opoly",
  scores = doses,
  data = D
)
print(ans)

# If levels were equally spaced, contrast='poly' would be appropriate.

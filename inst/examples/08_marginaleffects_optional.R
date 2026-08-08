# Example 8 ---------------------------------------------------------------
# Optional marginaleffects engine for a single GAMLSS parameter.
# This can be useful for average predictions and delta-method standard errors.
# The package still recommends the distribution+bootstrap engine for estimands
# that combine multiple parameters.

library(gamlss)
library(gamlss.dist)
library(marginaleffects)
library(gamlssPosthoc)

set.seed(808)
D <- data.frame(
  treatment = factor(rep(c("A", "B", "C"), each = 80)),
  x = runif(240, 0, 1)
)
D$y <- rGA(240,
  mu = exp(1 + 0.25 * (D$treatment == "B") + 0.45 * (D$treatment == "C") + 0.5 * D$x),
  sigma = 0.4
)
fit <- gamlss(y ~ treatment + x, family = GA, data = D, trace = FALSE)

ans <- gamlss_posthoc(
  fit,
  specs = "treatment",
  estimand = "parameter",
  what = "mu",
  engine = "marginaleffects",
  grid = "observed",
  data = D
)
print(ans)

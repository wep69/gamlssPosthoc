# Example 3 ---------------------------------------------------------------
# GAF model: mu is the positive mean and Var(Y)=sigma^2*mu^nu.
# Demonstrates post-hoc inference for mu and nu separately.

library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(303)
D <- expand.grid(
  block = factor(1:6),
  cultivar = factor(c("A", "B", "C", "D")),
  environment = factor(c("E1", "E2")),
  rep = 1:5
)

mu <- exp(1.3 + 0.22 * as.numeric(D$cultivar) + 0.30 * (D$environment == "E2"))
sigma <- exp(-0.9 + 0.15 * (D$environment == "E2"))
nu <- 1.55 + 0.45 * (D$environment == "E2")
var_pos <- sigma^2 * mu^nu
shape <- mu^2 / var_pos
rate <- mu / var_pos
D$yield_component <- rgamma(nrow(D), shape = shape, rate = rate)

fit <- gamlss(
  yield_component ~ cultivar + environment,
  sigma.formula = ~ environment,
  nu.formula = ~ environment,
  family = GAF,
  data = D,
  trace = FALSE
)

# Mean parameter mu.
mu_ph <- gamlss_posthoc(
  fit, specs = "cultivar", by = "environment",
  estimand = "parameter", what = "mu",
  contrast = "pairwise", data = D
)
print(mu_ph)

# Variance-power parameter nu.
nu_ph <- gamlss_posthoc(
  fit, specs = "environment",
  estimand = "parameter", what = "nu",
  contrast = "pairwise", data = D
)
print(nu_ph)

# Actual distribution variance, which combines mu, sigma and nu.
var_ph <- gamlss_posthoc(
  fit, specs = "cultivar", by = "environment",
  estimand = "variance",
  contrast = "pairwise",
  engine = "distribution",
  uncertainty = "none",
  data = D
)
print(var_ph)

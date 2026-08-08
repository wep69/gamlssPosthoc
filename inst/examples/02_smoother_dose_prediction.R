# Example 2 ---------------------------------------------------------------
# Dose-response with pb(dose). emmeans is deliberately bypassed because the
# selected GAMLSS submodel contains a smoother.

library(gamlss)
library(gamlss.dist)
library(distributions3)
library(gamlssPosthoc)

set.seed(202)
D <- expand.grid(
  block = factor(1:5),
  treatment = factor(c("Control", "Biostimulant")),
  dose = seq(0, 100, by = 10),
  rep = 1:3
)

mu <- exp(1.8 +
  0.25 * (D$treatment == "Biostimulant") +
  0.010 * D$dose - 0.000065 * D$dose^2 +
  0.003 * D$dose * (D$treatment == "Biostimulant"))
sigma <- 0.35 + 0.0015 * D$dose
D$root_mass <- rGA(nrow(D), mu = mu, sigma = sigma)

fit <- gamlss(
  root_mass ~ treatment + pb(dose) + treatment:pb(dose),
  sigma.formula = ~ pb(dose),
  family = GA,
  data = D,
  trace = FALSE
)

gamlss_engine_info(fit, estimand = "parameter", what = "mu")

# Mean response curve, standardized over the observed covariate distribution.
curve_mean <- gamlss_trend(
  fit,
  x = "dose",
  by = "treatment",
  method = "curve",
  estimand = "mean",
  n = 101,
  data = D,
  uncertainty = "none"
)
print(curve_mean)
plot(curve_mean)

# Numerical derivative: where does the response increase or decrease?
deriv <- gamlss_trend(
  fit,
  x = "dose",
  by = "treatment",
  method = "derivative",
  estimand = "mean",
  n = 101,
  data = D,
  uncertainty = "none"
)
print(deriv)
plot(deriv)

# Full predicted distribution at selected doses.
nd <- expand.grid(
  treatment = levels(D$treatment),
  dose = c(0, 25, 50, 75, 100),
  block = levels(D$block)[1],
  rep = mean(D$rep)
)
summary_dist <- gamlss_distribution_summary(fit, nd, data = D)
print(summary_dist)

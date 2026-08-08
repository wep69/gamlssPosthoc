# Example 7 ---------------------------------------------------------------
# Zero-adjusted GAF: four distributional components mu, sigma, nu, xi0.
# Demonstrates custom functions of multiple parameters.

library(gamlss)
library(gamlss.dist)
library(gamlss.inf)
library(gamlssPosthoc)

set.seed(707)
D <- expand.grid(
  block = factor(1:6),
  treatment = factor(c("Control", "T1", "T2", "T3")),
  environment = factor(c("E1", "E2")),
  rep = 1:8
)
mu <- exp(1.2 + 0.18 * as.numeric(D$treatment) + 0.30 * (D$environment == "E2"))
sigma <- exp(-0.75 + 0.12 * (D$environment == "E2"))
nu <- 1.65 + 0.35 * (D$environment == "E2")
xi0 <- plogis(-1.0 + 0.7 * (D$environment == "E2") - 0.25 * as.numeric(D$treatment))
vp <- sigma^2 * mu^nu
shape <- mu^2 / vp
rate <- mu / vp
yp <- rgamma(nrow(D), shape = shape, rate = rate)
D$root_response <- ifelse(rbinom(nrow(D), 1, xi0) == 1, 0, yp)

fit <- gamlssZadj(
  y = root_response,
  mu.formula = ~ treatment + environment,
  sigma.formula = ~ environment,
  nu.formula = ~ environment,
  xi0.formula = ~ treatment + environment,
  family = GAF,
  data = D,
  trace = FALSE
)

# Marginal mean: built in for GAF because its positive-part mean is mu.
mean_ph <- gamlss_posthoc(
  fit, specs = "treatment", by = "environment",
  estimand = "mean", contrast = "pairwise",
  engine = "distribution", uncertainty = "none", data = D
)
print(mean_ph)

# Marginal variance of the zero-adjusted GAF:
# Var(Y) = (1-h)*sigma^2*mu^nu + h*(1-h)*mu^2
marginal_variance <- function(pars, newdata, object) {
  h <- pars$xi0
  (1 - h) * pars$sigma^2 * pars$mu^pars$nu + h * (1 - h) * pars$mu^2
}

var_ph <- gamlss_posthoc(
  fit, specs = "treatment", by = "environment",
  estimand = "custom", custom_fun = marginal_variance,
  contrast = "pairwise", engine = "distribution",
  uncertainty = "none", data = D
)
print(var_ph)

# Example 6 ---------------------------------------------------------------
# Zero-adjusted Gamma using gamlss.inf::gamlssZadj().
# The agronomic marginal mean is (1-xi0)*mu for a Gamma positive component.

library(gamlss)
library(gamlss.dist)
library(gamlss.inf)
library(gamlssPosthoc)

set.seed(606)
D <- expand.grid(
  block = factor(1:6),
  inoculation = factor(c("None", "Rhizobium", "Co-inoculation")),
  water = factor(c("100ETc", "60ETc")),
  rep = 1:8
)

eta_mu <- log(1.4) + 0.65 * (D$inoculation == "Rhizobium") +
  0.90 * (D$inoculation == "Co-inoculation") - 0.40 * (D$water == "60ETc")
mu <- exp(eta_mu)
eta_zero <- 0.5 - 1.2 * (D$inoculation == "Rhizobium") -
  1.7 * (D$inoculation == "Co-inoculation") + 0.9 * (D$water == "60ETc")
xi0 <- plogis(eta_zero)
ypos <- rGA(nrow(D), mu = mu, sigma = 0.55)
D$nodule_mass <- ifelse(rbinom(nrow(D), 1, xi0) == 1, 0, ypos)

fit <- gamlssZadj(
  y = nodule_mass,
  mu.formula = ~ inoculation * water,
  sigma.formula = ~ water,
  xi0.formula = ~ inoculation * water,
  family = GA,
  data = D,
  trace = FALSE
)

# Probability of zero.
pzero <- gamlss_posthoc(
  fit, specs = "inoculation", by = "water",
  estimand = "prob_zero", contrast = "pairwise",
  engine = "distribution", uncertainty = "none", data = D
)
print(pzero)

# Positive-part mean mu.
posmean <- gamlss_posthoc(
  fit, specs = "inoculation", by = "water",
  estimand = "parameter", what = "mu",
  contrast = "pairwise", engine = "distribution",
  uncertainty = "none", data = D
)
print(posmean)

# Marginal mean E(Y)=(1-xi0)*mu. Case bootstrap propagates uncertainty from
# both the zero and positive components. Increase B to >=999 for final analysis.
margmean <- gamlss_posthoc(
  fit, specs = "inoculation", by = "water",
  estimand = "mean", contrast = "pairwise",
  engine = "distribution", uncertainty = "bootstrap",
  bootstrap = "case", B = 99,
  data = D, seed = 606
)
print(margmean)

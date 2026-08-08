# Example 1 ---------------------------------------------------------------
# Factorial agronomic experiment: Gamma GAMLSS without smoothers.
# The automatic router selects emmeans only when the target population is explicitly the reference grid.

library(gamlss)
library(gamlss.dist)
library(emmeans)
library(gamlssPosthoc)

set.seed(101)
D <- expand.grid(
  block = factor(1:5),
  cultivar = factor(c("C1", "C2", "C3")),
  water = factor(c("100ETc", "70ETc")),
  rep = 1:6
)

eta_mu <- 2.1 +
  0.20 * (D$cultivar == "C2") +
  0.38 * (D$cultivar == "C3") -
  0.30 * (D$water == "70ETc") -
  0.12 * (D$cultivar == "C3") * (D$water == "70ETc")
mu <- exp(eta_mu)
sigma <- exp(-0.75 + 0.18 * (D$water == "70ETc"))
D$biomass <- rGA(nrow(D), mu = mu, sigma = sigma)

fit <- gamlss(
  biomass ~ cultivar * water,
  sigma.formula = ~ water,
  family = GA,
  data = D,
  trace = FALSE
)

# Check routing
gamlss_engine_info(fit, estimand = "parameter", what = "mu")

# Adjusted means of the positive response parameter mu.
ans <- gamlss_posthoc(
  fit,
  specs = "cultivar",
  by = "water",
  estimand = "parameter",
  what = "mu",
  contrast = "pairwise",
  adjust = "tukey",
  data = D
)

print(ans)
plot(ans)

# Dispersion can be compared separately.
ans_sigma <- gamlss_posthoc(
  fit,
  specs = "water",
  estimand = "parameter",
  what = "sigma",
  contrast = "pairwise",
  data = D
)
print(ans_sigma)

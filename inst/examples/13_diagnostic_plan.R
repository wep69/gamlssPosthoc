# Full diagnostic before post-hoc analysis
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(213)
D <- data.frame(dose = seq(.1, 1.5, length.out = 100))
D$y <- rGA(nrow(D), mu = exp(.5 + .25 * sin(5 * D$dose)), sigma = .3)
fit <- gamlss(y ~ pb(dose), family = GA, data = D, trace = FALSE)

plan <- gamlss_posthoc_plan(
  fit,
  estimand = "parameter",
  what = "mu",
  contrast = "none",
  population = "observed"
)
print(plan)

# Generic zero-adjusted model with a positive Generalized Gamma distribution
library(gamlss)
library(gamlss.dist)
library(gamlss.inf)
library(distributions3)
library(gamlssPosthoc)

set.seed(210)
D <- data.frame(trt = factor(rep(c("Control", "BioA", "BioB"), each = 80)))
mu <- c(Control = 2.0, BioA = 2.8, BioB = 3.3)[D$trt]
h <- c(Control = .45, BioA = .25, BioB = .15)[D$trt]
ypos <- rGG(nrow(D), mu = mu, sigma = .38, nu = .70)
D$root_mass <- ifelse(rbinom(nrow(D), 1, h) == 1, 0, ypos)

fit <- gamlssZadj(
  y = root_mass,
  mu.formula = ~ trt,
  sigma.formula = ~ 1,
  nu.formula = ~ 1,
  xi0.formula = ~ trt,
  family = GG,
  data = D,
  trace = FALSE
)

# The full response mean is not assumed to equal mu.
ph <- gamlss_posthoc(
  fit, specs = "trt", estimand = "mean",
  contrast = "pairwise", comparison = "percent_change",
  uncertainty = "none", data = D
)
print(ph)

# Generic full-distribution summaries: mean, variance, zero mass, quantiles.
nd <- data.frame(trt = factor(levels(D$trt), levels = levels(D$trt)))
gamlss_distribution_summary(fit, nd, probs = c(.25, .5, .9), data = D)

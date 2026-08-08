# Example 9 ---------------------------------------------------------------
# Optional compact letter display (CLD).
# Always report estimates and intervals; letters are only a compact display layer.

library(gamlss)
library(gamlss.dist)
library(emmeans)
library(multcompView)
library(gamlssPosthoc)

set.seed(909)
D <- data.frame(
  treatment = factor(rep(c("Control", "T1", "T2", "T3"), each = 50))
)
mu <- c(Control = 2.0, T1 = 2.15, T2 = 2.8, T3 = 3.0)[D$treatment]
D$biomass <- rGA(nrow(D), mu = mu, sigma = .35)
fit <- gamlss(biomass ~ treatment, family = GA, data = D, trace = FALSE)

ph <- gamlss_posthoc(
  fit,
  specs = "treatment",
  estimand = "parameter",
  what = "mu",
  contrast = "pairwise",
  adjust = "tukey",
  data = D
)

print(ph)
letters_table <- gamlss_cld(ph, alpha = 0.05)
print(letters_table)

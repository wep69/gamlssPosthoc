# Example 4 ---------------------------------------------------------------
# Quantitative polynomial regression: fit nested models and compare them.
# This is NOT the same as a polynomial contrast among factor means.

library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(404)
D <- expand.grid(
  block = factor(1:6),
  dose = c(0, 20, 40, 60, 80, 100),
  inoculant = factor(c("No", "Yes")),
  rep = 1:4
)
mu <- exp(1.5 + 0.32 * (D$inoculant == "Yes") +
          0.014 * D$dose - 0.00009 * D$dose^2)
D$shoot_mass <- rGA(nrow(D), mu = mu, sigma = 0.38)

m1 <- gamlss(
  shoot_mass ~ inoculant + dose,
  family = GA, data = D, trace = FALSE
)
m2 <- gamlss(
  shoot_mass ~ inoculant + dose + I(dose^2),
  family = GA, data = D, trace = FALSE
)
m3 <- gamlss(
  shoot_mass ~ inoculant + dose + I(dose^2) + I(dose^3),
  family = GA, data = D, trace = FALSE
)

# Sequential LR statistics plus GAIC.
poly_models <- gamlss_poly_compare(linear = m1, quadratic = m2, cubic = m3)
print(poly_models)

# Interpret the selected curve by predictions and derivatives.
curve <- gamlss_trend(
  m2, x = "dose", by = "inoculant",
  method = "curve", estimand = "mean",
  n = 101, data = D
)
plot(curve)

derivative <- gamlss_trend(
  m2, x = "dose", by = "inoculant",
  method = "derivative", estimand = "mean",
  n = 101, data = D
)
plot(derivative)

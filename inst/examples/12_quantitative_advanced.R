# Quantitative regression: curve, derivatives, turning point, optimum
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(212)
D <- data.frame(dose = seq(0, 150, length.out = 180))
eta <- log(2.0) + 0.012 * D$dose - 0.000045 * D$dose^2
D$biomass <- rGA(nrow(D), mu = exp(eta), sigma = .20)
fit <- gamlss(biomass ~ dose + I(dose^2), family = GA, data = D, trace = FALSE)
g <- seq(0, 150, length.out = 151)

curve <- gamlss_trend(fit, "dose", at_x = g, method = "curve",
                      estimand = "mean", uncertainty = "none", data = D)
d1 <- gamlss_trend(fit, "dose", at_x = g, method = "derivative",
                   derivative_order = 1, estimand = "mean", uncertainty = "none", data = D)
d2 <- gamlss_trend(fit, "dose", at_x = g, method = "derivative",
                   derivative_order = 2, estimand = "mean", uncertainty = "none", data = D)
tp <- gamlss_trend(fit, "dose", at_x = g, method = "turning_points",
                   estimand = "mean", uncertainty = "none", data = D)
opt <- gamlss_trend(fit, "dose", at_x = g, method = "optimum", optimum = "maximum",
                    estimand = "mean", uncertainty = "none", data = D)

head(curve$values)
head(d1$values)
head(d2$values)
tp$special_points
opt$special_points

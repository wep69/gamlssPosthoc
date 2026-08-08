# Scientific contrast scales: difference, ratio, log-ratio, percent change
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(211)
D <- data.frame(trt = factor(rep(c("T0", "T1", "T2"), each = 50)))
mu <- c(T0 = 3.0, T1 = 3.6, T2 = 4.4)[D$trt]
D$yield <- rGA(nrow(D), mu = mu, sigma = .25)
fit <- gamlss(yield ~ trt, family = GA, data = D, trace = FALSE)

for (scale in c("difference", "ratio", "log_ratio", "percent_change")) {
  cat("\n---", scale, "---\n")
  print(gamlss_posthoc(
    fit, specs = "trt", estimand = "mean",
    contrast = "reference", comparison = scale,
    uncertainty = "none", data = D
  ))
}

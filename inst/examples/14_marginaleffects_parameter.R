# marginaleffects owns standardized parameter predictions and ratio comparisons
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

if (requireNamespace("marginaleffects", quietly = TRUE)) {
  set.seed(214)
  D <- data.frame(
    trt = factor(rep(c("A", "B", "C"), each = 50)),
    block = factor(rep(1:5, times = 30))
  )
  D$y <- rGA(nrow(D), mu = exp(1 + .15 * as.numeric(D$trt)), sigma = .28)
  fit <- gamlss(y ~ trt + block, family = GA, data = D, trace = FALSE)

  ans <- gamlss_posthoc(
    fit, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "ratio",
    engine = "marginaleffects", uncertainty = "delta", adjust = "holm",
    population = "observed", data = D
  )
  print(ans)
}

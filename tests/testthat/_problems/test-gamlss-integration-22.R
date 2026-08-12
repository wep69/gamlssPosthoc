# Extracted from test-gamlss-integration.R:22

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gamlssPosthoc", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
make_ga_data <- function(seed = 10, n = 35L) {
  set.seed(seed)
  d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = n)))
  mu <- c(A = 2, B = 2.6, C = 3.1)[d$trt]
  d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.35)
  d
}

# test -------------------------------------------------------------------------
skip_if_not_installed("gamlss")
skip_if_not_installed("gamlss.dist")
skip_if_not_installed("emmeans")
d <- make_ga_data()
m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
observed <- gamlss_engine_info(m, "parameter", "mu", population = "observed")
expect_false(observed$emmeans_safe)
reference <- gamlss_engine_info(m, "parameter", "mu", population = "reference")
expect_true(reference$emmeans_safe)
z <- gamlss_posthoc(m, specs = "trt", estimand = "parameter", what = "mu",
                      population = "reference", data = d)

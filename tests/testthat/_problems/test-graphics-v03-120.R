# Extracted from test-graphics-v03.R:120

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "gamlssPosthoc", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
testthat::skip_if_not_installed("ggplot2")
testthat::skip_if_not_installed("distributions3")
z <- .make_v03_fit()
ph <- gamlss_posthoc(z$fit, "trt", estimand = "mean", contrast = "pairwise",
                       uncertainty = "none", data = z$d)
ds <- gamlss_distribution_summary(z$fit,
      data.frame(trt = factor(c("A", "B"), levels = levels(z$d$trt)), dose = c(50, 50)),
      data = z$d)
plan <- gamlss_posthoc_plan(z$fit, estimand = "parameter", what = "mu")
testthat::expect_s3_class(ggplot2::autoplot(ds, x = "trt"), "ggplot")
testthat::expect_s3_class(ggplot2::autoplot(plan), "ggplot")
testthat::expect_invisible(plot(ds))
testthat::expect_invisible(plot(plan))

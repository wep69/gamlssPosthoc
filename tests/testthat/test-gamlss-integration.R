make_ga_data <- function(seed = 10, n = 35L) {
  set.seed(seed)
  d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = n)))
  mu <- c(A = 2, B = 2.6, C = 3.1)[d$trt]
  d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.35)
  d
}

test_that("emmeans is used only for an explicit reference population", {
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
  expect_s3_class(z, "gamlss_posthoc")
  expect_identical(z$engine, "emmeans")
  expect_identical(z$population, "reference")
  expect_equal(nrow(z$estimates), 3L)
})

test_that("distribution parameter predictions aggregate to group estimates", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(11)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    engine = "distribution", grid = "observed", data = d
  )
  expect_s3_class(z, "gamlss_posthoc")
  expect_identical(z$engine, "distribution")
  expect_true(all(is.finite(z$estimates$estimate)))
  expect_equal(nrow(z$estimates), 3L)
})

test_that("full-distribution GA means are finite", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("distributions3")
  d <- make_ga_data(12)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  nd <- data.frame(trt = factor(c("A", "B", "C"), levels = levels(d$trt)))
  z <- gamlss_distribution_summary(m, nd, data = d)
  expect_equal(nrow(z), 3L)
  expect_true(all(z$mean > 0))
  expect_true(all(z$variance > 0))
})

test_that("pb smoother is not routed to emmeans", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(13)
  d <- data.frame(x = seq(0, 1, length.out = 80))
  d$y <- gamlss.dist::rGA(80, mu = exp(0.8 + 0.4 * sin(3 * d$x)), sigma = 0.35)
  pb <- getExportedValue("gamlss", "pb")
  m <- gamlss::gamlss(y ~ pb(x), family = gamlss.dist::GA,
                      data = d, trace = FALSE)
  info <- gamlss_engine_info(m, "parameter", "mu")
  expect_false(info$emmeans_safe)
  expect_true(info$recommended_engine %in% c("marginaleffects", "distribution"))
})

test_that("trend curve and derivative return finite values", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(14)
  d <- data.frame(x = seq(0.2, 2, length.out = 80))
  d$y <- gamlss.dist::rGA(80, mu = exp(0.3 + 0.5 * d$x), sigma = 0.25)
  m <- gamlss::gamlss(y ~ x, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_trend(
    m, x = "x", at_x = seq(0.3, 1.9, length.out = 12),
    method = "derivative", estimand = "parameter", what = "mu", data = d
  )
  expect_s3_class(z, "gamlss_trend")
  expect_true(all(is.finite(z$values$derivative1)))
})

test_that("polynomial comparison validates models and returns LR columns", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(15)
  d <- data.frame(x = seq(-1, 1, length.out = 100))
  d$y <- gamlss.dist::rGA(100, mu = exp(1 + 0.4*d$x - 0.25*d$x^2), sigma = 0.3)
  m1 <- gamlss::gamlss(y ~ x, family = gamlss.dist::GA, data = d, trace = FALSE)
  m2 <- gamlss::gamlss(y ~ x + I(x^2), family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_poly_compare(linear = m1, quadratic = m2)
  expect_equal(nrow(z), 2L)
  expect_true(all(c("GAIC", "LR_from_previous", "p_value") %in% names(z)))
})

test_that("custom estimands enforce one output per prediction row", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(16)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  expect_error(
    gamlss_posthoc(
      m, specs = "trt", estimand = "custom", engine = "distribution", data = d,
      custom_fun = function(pars, newdata, object) 1
    ),
    "one value per row"
  )
})


test_that("prob_zero is exact mass, not CDF at zero, for continuous families", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("distributions3")
  set.seed(17)
  d <- data.frame(g = factor(rep(c("A", "B"), each = 30)))
  d$y <- stats::rnorm(nrow(d), mean = ifelse(d$g == "A", 0, 1), sd = 1)
  m <- gamlss::gamlss(y ~ g, family = gamlss.dist::NO, data = d, trace = FALSE)
  z <- gamlss_posthoc(m, specs = "g", estimand = "prob_zero",
                      engine = "distribution", data = d)
  expect_equal(z$estimates$estimate, c(0, 0), tolerance = 1e-12)
})

test_that("prediction grids preserve factor classes and reject unknown levels", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(18)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_posthoc(m, specs = "trt", estimand = "parameter", what = "mu",
                      engine = "distribution", grid = "observed", data = d)
  expect_equal(nrow(z$estimates), 3L)
  expect_error(
    gamlss_posthoc(m, specs = "trt", at = list(trt = "UNKNOWN"),
                   estimand = "parameter", what = "mu",
                   engine = "distribution", data = d),
    "unknown level"
  )
})


test_that("zero-adjusted Gamma marginal mean combines mu and xi0", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("distributions3")
  skip_if_not_installed("gamlss.inf")
  set.seed(19)
  d <- data.frame(trt = factor(rep(c("A", "B"), each = 45)))
  mu <- ifelse(d$trt == "A", 2.0, 3.0)
  p0 <- ifelse(d$trt == "A", 0.45, 0.20)
  ypos <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.30)
  d$y <- ifelse(stats::rbinom(nrow(d), 1, p0) == 1, 0, ypos)
  m <- gamlss.inf::gamlssZadj(
    y = y, mu.formula = ~ trt, xi0.formula = ~ trt,
    family = gamlss.dist::GA, data = d, trace = FALSE
  )
  z <- gamlss_posthoc(m, specs = "trt", estimand = "mean",
                      engine = "distribution", data = d)
  pz <- gamlss_posthoc(m, specs = "trt", estimand = "prob_zero",
                       engine = "distribution", data = d)
  expect_equal(nrow(z$estimates), 2L)
  expect_true(all(z$estimates$estimate > 0))
  expect_true(all(pz$estimates$estimate > 0 & pz$estimates$estimate < 1))

  nd <- data.frame(trt = factor(c("A", "B"), levels = levels(d$trt)))
  muhat <- stats::predict(m, parameter = "mu", newdata = nd,
                          type = "response", data = d)
  hhat <- stats::predict(m, parameter = "xi0", newdata = nd,
                         type = "response", data = d)
  expect_equal(z$estimates$estimate, as.numeric((1 - hhat) * muhat), tolerance = 1e-7)
})

test_that("pb smoother can be evaluated by prediction-based trend", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(20)
  d <- data.frame(x = seq(0.05, 1, length.out = 70))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.7 + 0.35 * sin(4 * d$x)), sigma = 0.30)
  pb <- getExportedValue("gamlss", "pb")
  m <- gamlss::gamlss(y ~ pb(x), family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_trend(m, x = "x", at_x = seq(0.1, 0.9, length.out = 9),
                    method = "curve", estimand = "parameter", what = "mu", data = d)
  expect_s3_class(z, "gamlss_trend")
  expect_true(all(is.finite(z$values$estimate)))
})

test_that("refit case bootstrap returns finite uncertainty", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(21, n = 20L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  expect_warning(
    z <- gamlss_posthoc(
      m, specs = "trt", estimand = "parameter", what = "mu",
      engine = "distribution", uncertainty = "bootstrap",
      bootstrap = "case", B = 5, data = d, seed = 21
    ),
    "B < 20"
  )
  expect_true(all(is.finite(z$estimates$SE)))
  expect_true(all(z$estimates$boot_success >= 2))
})

test_that("opoly works for unequally spaced factor scores", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("emmeans")
  set.seed(22)
  dose <- rep(c(0, 25, 50, 100), each = 25)
  d <- data.frame(dose_f = factor(dose, levels = c(0, 25, 50, 100)))
  d$y <- gamlss.dist::rGA(nrow(d), mu = 2 + 0.015 * dose - 0.00008 * dose^2, sigma = 0.25)
  m <- gamlss::gamlss(y ~ dose_f, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- gamlss_posthoc(
    m, specs = "dose_f", estimand = "parameter", what = "mu",
    contrast = "opoly", scores = c(0, 25, 50, 100), degree = 2, data = d
  )
  expect_s3_class(z, "gamlss_posthoc")
  expect_equal(nrow(z$contrasts), 2L)
})

test_that("marginaleffects engine returns predictions when installed", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("marginaleffects")
  d <- make_ga_data(23, n = 20L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- suppressWarnings(gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    engine = "marginaleffects", data = d
  ))
  expect_s3_class(z, "gamlss_posthoc")
  expect_identical(z$engine, "marginaleffects")
  expect_equal(nrow(z$estimates), 3L)
})

test_that("generic zero-adjusted GG provides mean variance and quantiles", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("gamlss.inf")
  skip_if_not_installed("distributions3")
  set.seed(31)
  d <- data.frame(trt = factor(rep(c("A", "B"), each = 70)))
  mu <- ifelse(d$trt == "A", 2.0, 3.0)
  h <- ifelse(d$trt == "A", .35, .15)
  yp <- gamlss.dist::rGG(nrow(d), mu = mu, sigma = .35, nu = .7)
  d$y <- ifelse(stats::rbinom(nrow(d), 1, h) == 1, 0, yp)
  m <- gamlss.inf::gamlssZadj(
    y = y, mu.formula = ~ trt, xi0.formula = ~ trt,
    family = gamlss.dist::GG, data = d, trace = FALSE
  )
  nd <- data.frame(trt = factor(c("A", "B"), levels = levels(d$trt)))
  z <- gamlss_distribution_summary(m, nd, probs = c(.25, .5, .9), data = d)
  expect_true(all(is.finite(z$mean) & z$mean > 0))
  expect_true(all(is.finite(z$variance) & z$variance > 0))
  expect_true(all(z$prob_zero > 0 & z$prob_zero < 1))
  expect_true(all(z$q25 >= 0 & z$q50 >= 0 & z$q90 >= 0))

  p <- gamlss_posthoc(m, specs = "trt", estimand = "variance",
                      engine = "distribution", uncertainty = "none", data = d)
  expect_true(all(is.finite(p$estimates$estimate) & p$estimates$estimate > 0))
})

test_that("diagnostic plan discovers parameters and recommends around smoothers", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  set.seed(32)
  d <- data.frame(x = seq(.05, 1, length.out = 80))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(.6 + .3 * sin(4*d$x)), sigma = .3)
  pb <- getExportedValue("gamlss", "pb")
  m <- gamlss::gamlss(y ~ pb(x), family = gamlss.dist::GA, data = d, trace = FALSE)
  p <- gamlss_posthoc_plan(m, estimand = "parameter", what = "mu")
  expect_s3_class(p, "gamlss_posthoc_plan")
  expect_true("mu" %in% p$parameter_table$parameter)
  expect_true(p$parameter_table$smoother[p$parameter_table$parameter == "mu"])
  expect_false(p$capabilities$eligible[p$capabilities$engine == "emmeans"])
})

test_that("scientific ratios and percent changes are available on response scale", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("distributions3")
  d <- make_ga_data(33, n = 30L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  zr <- gamlss_posthoc(m, specs = "trt", estimand = "mean", contrast = "pairwise",
                       comparison = "ratio", engine = "distribution",
                       uncertainty = "none", data = d)
  zp <- gamlss_posthoc(m, specs = "trt", estimand = "mean", contrast = "pairwise",
                       comparison = "percent_change", engine = "distribution",
                       uncertainty = "none", data = d)
  expect_true(all(is.finite(zr$contrasts$estimate)))
  expect_true(all(is.finite(zp$contrasts$estimate)))
  expect_true(all(zr$contrasts$estimate > 0))
  expect_identical(zr$estimand_info$population, "observed")
})

test_that("quantitative trends include second derivatives turning points and optima", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("distributions3")
  set.seed(34)
  d <- data.frame(x = seq(0, 10, length.out = 160))
  eta <- 1 + .20*d$x - .020*d$x^2
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(eta), sigma = .20)
  m <- gamlss::gamlss(y ~ x + I(x^2), family = gamlss.dist::GA, data = d, trace = FALSE)
  g <- seq(.2, 9.8, length.out = 80)
  d2 <- gamlss_trend(m, x = "x", at_x = g, method = "derivative",
                     derivative_order = 2, estimand = "mean", uncertainty = "none", data = d)
  expect_true(all(is.finite(d2$values$derivative2)))
  tp <- gamlss_trend(m, x = "x", at_x = g, method = "turning_points",
                     estimand = "mean", uncertainty = "none", data = d)
  expect_true(nrow(tp$special_points) >= 1L)
  op <- gamlss_trend(m, x = "x", at_x = g, method = "optimum", optimum = "maximum",
                     estimand = "mean", uncertainty = "none", data = d)
  expect_true(is.finite(op$special_points$x[1]))
  expect_true(op$special_points$x[1] > 3 && op$special_points$x[1] < 7)
})

test_that("marginaleffects can own response-scale ratio comparisons when available", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("marginaleffects")
  d <- make_ga_data(35, n = 25L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  z <- suppressWarnings(gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "ratio", engine = "marginaleffects",
    uncertainty = "delta", adjust = "holm", data = d
  ))
  expect_s3_class(z, "gamlss_posthoc")
  expect_identical(z$engine, "marginaleffects")
  expect_true(all(is.finite(z$estimates$estimate)))
  if (!is.null(z$contrasts) && "p.value" %in% names(z$contrasts)) {
    expect_true("p.value.adjusted" %in% names(z$contrasts))
  }
})

test_that("marginaleffects percent change equals transformed marginal-mean ratio", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("marginaleffects")
  d <- make_ga_data(36, n = 25L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  zr <- gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "ratio", engine = "marginaleffects",
    uncertainty = "none", adjust = "holm", data = d
  )
  zp <- gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "percent_change", engine = "marginaleffects",
    uncertainty = "none", adjust = "holm", data = d
  )
  expect_identical(zr$engine, "marginaleffects")
  expect_identical(zp$engine, "marginaleffects")
  expect_equal(nrow(zr$contrasts), nrow(zp$contrasts))
  expect_equal(zp$contrasts$estimate, 100 * (zr$contrasts$estimate - 1), tolerance = 1e-8)
})

test_that("diagnostic plan mirrors distribution-engine uncertainty fallbacks", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(37, n = 20L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  p <- gamlss_posthoc_plan(m, estimand = "mean", uncertainty = "delta")
  expect_identical(p$recommendation$engine, "distribution")
  expect_identical(p$recommendation$uncertainty, "bootstrap")
})

test_that("diagnostic plan does not hide refit-bootstrap engine changes", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(38, n = 20L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  p <- gamlss_posthoc_plan(m, estimand = "parameter", what = "mu",
                           population = "observed", uncertainty = "bootstrap")
  expect_identical(p$recommendation$engine, "distribution")
  expect_identical(p$recommendation$uncertainty, "bootstrap")
})


test_that("marginaleffects and distribution engines agree on pairwise ratio orientation", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  skip_if_not_installed("marginaleffects")
  d <- make_ga_data(39, n = 30L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  zm <- gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "ratio", engine = "marginaleffects",
    uncertainty = "none", adjust = "holm", data = d
  )
  zd <- gamlss_posthoc(
    m, specs = "trt", estimand = "parameter", what = "mu",
    contrast = "pairwise", comparison = "ratio", engine = "distribution",
    uncertainty = "none", adjust = "holm", data = d
  )
  expect_equal(sort(zm$contrasts$estimate), sort(zd$contrasts$estimate), tolerance = 1e-7)
})

test_that("diagnostic flags numeric-weight nonlinear contrast covariance shortcut", {
  skip_if_not_installed("gamlss")
  skip_if_not_installed("gamlss.dist")
  d <- make_ga_data(40, n = 20L)
  m <- gamlss::gamlss(y ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  p <- gamlss_posthoc_plan(
    m, estimand = "parameter", what = "mu", contrast = "pairwise",
    comparison = "ratio", population = "observed",
    weights = rep(1, nrow(d)), uncertainty = "delta"
  )
  expect_true(any(grepl("Numeric user weights", p$warnings, fixed = TRUE)))
})

testthat::test_that("themes are ggplot themes", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::expect_s3_class(theme_gamlss(), "theme")
  testthat::expect_s3_class(theme_gamlss_journal(), "theme")
  testthat::expect_s3_class(theme_gamlss_presentation(), "theme")
})

testthat::test_that("plot-data layer is separate and auditable", {
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  pd <- gamlss_plot_data(z$fit,"parameters",x="dose",parameters=c("mu","sigma"),data=z$d,n=20)
  testthat::expect_s3_class(pd,"gamlss_plot_data")
  testthat::expect_true(all(c("estimate","parameter","dose") %in% names(pd)))
  testthat::expect_setequal(unique(pd$parameter), c("mu","sigma"))
})

testthat::test_that("all main plot constructors return ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit(); f <- z$fit; d <- z$d
  funs <- list(
    plot_gamlss_parameters(f,"dose",parameters=c("mu","sigma"),data=d,n=20),
    plot_gamlss_estimand(f,"dose",estimand="mean",data=d,n=20),
    plot_gamlss_distribution(f,"trt",type="density",data=d,n_response=40),
    plot_gamlss_quantiles(f,"dose",probs=c(.1,.5,.9),style="lines",data=d,n=15),
    plot_gamlss_fit(f,"dose",response="y",data=d,n=15),
    plot_gamlss_diagnostics(f,"qq",data=d),
    plot_gamlss_compare(M1=f,M2=f,type="criteria")
  )
  testthat::expect_true(all(vapply(funs, inherits, logical(1), what="ggplot")))
})

testthat::test_that("trend derivative and optimum plots return ggplot", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit(); f <- z$fit; d <- z$d
  p1 <- plot_gamlss_trend(f,"dose",data=d,n=25)
  p2 <- plot_gamlss_derivative(f,"dose",order=1,data=d,n=25)
  p3 <- plot_gamlss_optimum(f,"dose",data=d,n=25)
  testthat::expect_s3_class(p1,"ggplot"); testthat::expect_s3_class(p2,"ggplot"); testthat::expect_s3_class(p3,"ggplot")
})

testthat::test_that("surface data and plot are finite", {
  testthat::skip_if_not_installed("ggplot2"); testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit(); z$d$sal <- rep(seq(0,2,length.out=6), length.out=nrow(z$d))
  z$d$y <- gamlss.dist::rGA(nrow(z$d),mu=exp(.5+.008*z$d$dose-.1*z$d$sal),sigma=.25)
  f <- gamlss::gamlss(y~dose+sal,family=gamlss.dist::GA,data=z$d,trace=FALSE)
  pd <- gamlss_plot_data(f,"surface",x="dose",y="sal",data=z$d,n=15)
  testthat::expect_true(all(is.finite(pd$estimate)))
  testthat::expect_s3_class(plot_gamlss_surface(f,"dose","sal",data=z$d,n=15),"ggplot")
})

testthat::test_that("autoplot methods dispatch", {
  testthat::skip_if_not_installed("ggplot2"); testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  ph <- gamlss_posthoc(z$fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=z$d)
  tr <- gamlss_trend(z$fit,"dose",n=20,uncertainty="none",data=z$d)
  ds <- gamlss_distribution_summary(z$fit,data.frame(trt=factor(c("A","B"),levels=levels(z$d$trt)),dose=c(50,50)),data=z$d)
  testthat::expect_s3_class(ggplot2::autoplot(ph),"ggplot")
  testthat::expect_s3_class(ggplot2::autoplot(tr),"ggplot")
  testthat::expect_s3_class(ggplot2::autoplot(ds,x="trt"),"ggplot")
})

testthat::test_that("parameter plots support public link-scale prediction", {
  testthat::skip_if_not_installed("ggplot2")
  z <- .make_v03_fit()
  pd <- gamlss_plot_data(z$fit, "parameters", x = "dose", parameters = "mu",
                         parameter_scale = "link", data = z$d, n = 12)
  testthat::expect_true(all(is.finite(pd$estimate)))
  testthat::expect_s3_class(plot_gamlss_parameters(z$fit, "dose", parameters = "mu",
                                                   scale = "link", data = z$d, n = 12), "ggplot")
})

testthat::test_that("distribution plot data label distinct numeric conditioning values", {
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  pd <- gamlss_plot_data(z$fit, "distribution", x = "dose",
                         at = list(dose = c(10, 60, 110)), data = z$d,
                         n_response = 25)
  testthat::expect_true(".gph_label" %in% names(pd))
  testthat::expect_equal(length(unique(pd$.gph_label)), 3L)
})

testthat::test_that("fit plot adds predictive summaries for factors", {
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  p <- plot_gamlss_fit(z$fit, "trt", response = "y", jitter = TRUE, data = z$d)
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_gte(length(p$layers), 3L)
})

testthat::test_that("bootstrap optimum retains location draws", {
  testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  tr <- gamlss_trend(z$fit, "dose", method = "optimum", n = 17,
                     uncertainty = "bootstrap", bootstrap = "case", B = 12,
                     data = z$d, seed = 17)
  dr <- attr(tr$special_points, "optimum_draws")
  testthat::expect_true(is.list(dr))
  testthat::expect_equal(length(dr), nrow(tr$special_points))
  testthat::expect_true(all(vapply(dr, length, integer(1)) == 12L))
})

testthat::test_that("plot methods are paired with autoplot methods", {
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
  # `plot()` dispatches on its first formal argument, so the column selector
  # cannot be passed as `x =`; the default is the first column ("trt").
  testthat::expect_invisible(plot(ds))
  testthat::expect_invisible(plot(plan))
})

testthat::test_that("diagnostics use the native CRAN-safe ggplot2 engine", {
  z <- .make_v03_fit()
  testthat::skip_if_not_installed("ggplot2")
  p <- plot_gamlss_diagnostics(z$fit, "qq", engine = "native", data = z$d)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("zero-adjusted graphic exposes all three scientific components", {
  testthat::skip_if_not_installed("gamlss")
  testthat::skip_if_not_installed("gamlss.dist")
  testthat::skip_if_not_installed("gamlss.inf")
  testthat::skip_if_not_installed("distributions3")
  testthat::skip_if_not_installed("ggplot2")
  set.seed(340)
  d <- data.frame(trt = factor(rep(c("A", "B"), each = 45)))
  mu <- ifelse(d$trt == "A", 2, 3)
  h <- ifelse(d$trt == "A", .40, .18)
  yp <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = .28)
  d$y <- ifelse(stats::rbinom(nrow(d), 1, h) == 1, 0, yp)
  fit <- gamlss.inf::gamlssZadj(y = y, mu.formula = ~ trt,
      xi0.formula = ~ trt, family = gamlss.dist::GA, data = d, trace = FALSE)
  pd <- gamlss_plot_data(fit, "zero_adjusted", x = "trt", data = d)
  testthat::expect_setequal(unique(pd$component),
                            c("prob_zero", "positive_mean", "marginal_mean"))
  testthat::expect_s3_class(plot_gamlss_zero_adjusted(fit, "trt", data = d), "ggplot")
})

testthat::test_that("model comparison supports several criteria and all-facet mode", {
  testthat::skip_if_not_installed("ggplot2")
  z <- .make_v03_fit()
  p1 <- plot_gamlss_compare(M1=z$fit, M2=z$fit, type="criteria", criterion="global_deviance")
  p2 <- plot_gamlss_compare(M1=z$fit, M2=z$fit, type="criteria", criterion="all")
  testthat::expect_s3_class(p1, "ggplot")
  testthat::expect_s3_class(p2, "ggplot")
})

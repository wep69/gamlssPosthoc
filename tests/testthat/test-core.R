test_that("engine_info rejects non-GAMLSS objects", {
  expect_error(gamlss_engine_info(stats::lm(mpg ~ wt, data = mtcars)), "must inherit")
})

test_that("contrast weights are complete and zero-sum", {
  w <- gamlssPosthoc:::.gph_contrast_weights(4, "pairwise")
  expect_length(w, 6)
  expect_true(all(vapply(w, length, integer(1)) == 4L))
  expect_true(all(vapply(w, sum, numeric(1)) == 0))
})

test_that("reference and sequential contrasts have expected counts", {
  expect_length(gamlssPosthoc:::.gph_contrast_weights(5, "reference", ref = 2), 4)
  expect_length(gamlssPosthoc:::.gph_contrast_weights(5, "sequential"), 4)
})

test_that("orthogonal polynomial weights are constructed for unequal scores", {
  w <- gamlssPosthoc:::.gph_contrast_weights(
    4, "opoly", scores = c(0, 25, 50, 100), degree = 2
  )
  expect_length(w, 2)
  expect_named(w, c("linear", "quadratic"))
  expect_true(all(vapply(w, function(z) abs(sum(z)) < 1e-10, logical(1))))
})

test_that("evaluation values require at for dense numeric predictors", {
  expect_error(
    gamlssPosthoc:::.gph_levels_or_values(seq_len(50)),
    "Specify evaluation points"
  )
  expect_equal(
    gamlssPosthoc:::.gph_levels_or_values(seq_len(50), at = c(5, 10)),
    c(5, 10)
  )
})

test_that("case bootstrap preserves row count", {
  set.seed(1)
  d <- data.frame(x = seq_len(20))
  z <- gamlssPosthoc:::.gph_boot_data(d, "case")
  expect_equal(nrow(z), nrow(d))
  expect_named(z, "x")
})

test_that("cluster bootstrap requires a valid cluster", {
  d <- data.frame(g = factor(rep(letters[1:4], each = 3)), x = 1:12)
  expect_error(gamlssPosthoc:::.gph_boot_data(d, "cluster"), "cluster")
  set.seed(2)
  z <- gamlssPosthoc:::.gph_boot_data(d, "cluster", "g")
  expect_true(nrow(z) > 0)
  expect_true(is.factor(z$g))
})

test_that("CLD validates alpha before optional dependencies are needed", {
  x <- structure(list(contrast_method = "pairwise"), class = "gamlss_posthoc")
  expect_error(gamlss_cld(x, alpha = 1.2), "strictly between")
})


test_that("contrast helpers reject underidentified specifications", {
  expect_error(gamlssPosthoc:::.gph_contrast_weights(1, "pairwise"), "At least two")
  expect_error(gamlssPosthoc:::.gph_contrast_weights(3, "reference", ref = 4), "ref")
  expect_error(gamlssPosthoc:::.gph_contrast_weights(4, "opoly", scores = c(0, 1)), "scores")
  expect_error(gamlssPosthoc:::.gph_contrast_weights(4, "opoly", scores = c(0, 1, 1, 3)), "distinct")
  expect_error(gamlssPosthoc:::.gph_contrast_weights(4, "poly", degree = 4), "degree")
})

test_that("scientific comparison scales are mathematically correct", {
  expect_equal(gamlssPosthoc:::.gph_comparison_value(3, 2, "difference"), 1)
  expect_equal(gamlssPosthoc:::.gph_comparison_value(3, 2, "ratio"), 1.5)
  expect_equal(gamlssPosthoc:::.gph_comparison_value(3, 2, "log_ratio"), log(1.5))
  expect_equal(gamlssPosthoc:::.gph_comparison_value(3, 2, "percent_change"), 50)
  expect_true(is.na(gamlssPosthoc:::.gph_comparison_value(3, 0, "ratio")))
})

test_that("population alias and weights are resolved conservatively", {
  d <- data.frame(x = 1:3)
  expect_identical(gamlssPosthoc:::.gph_resolve_population(NULL, "balanced"), "balanced")
  expect_identical(gamlssPosthoc:::.gph_resolve_weights(NULL, d, "observed"), "proportional")
  expect_equal(sum(gamlssPosthoc:::.gph_resolve_weights(c(1, 2, 3), d, "observed")), 6)
})

test_that("local quadratic refinement locates an interior optimum", {
  x <- seq(0, 4, by = 0.5)
  y <- 5 - 1.7 * (x - 2.13)^2
  z <- gamlssPosthoc:::.gph_refine_extremum(x, y, "maximum")
  expect_true(z$refined)
  expect_equal(z$x, 2.13, tolerance = 1e-10)
  expect_equal(z$y, 5, tolerance = 1e-10)
})

test_that("parameter discovery fallback is not tied to conventional names", {
  fake <- list(alpha.formula = y ~ x, alpha.link = "log", shapeX.link = "identity")
  expect_setequal(gamlssPosthoc:::.gph_model_parameters(fake), c("alpha", "shapeX"))
})

test_that("marginaleffects percent change is defined from the ratio of marginal means", {
  expect_identical(gamlssPosthoc:::.gph_me_comparison("percent_change"), "ratio")
})


test_that("marginaleffects pairwise direction matches internal pairwise convention", {
  expect_identical(gamlssPosthoc:::.gph_me_variables("trt", "pairwise")$trt, "revpairwise")
  expect_identical(gamlssPosthoc:::.gph_me_variables("trt", "reference", ref = 1L)$trt, "reference")
  expect_null(gamlssPosthoc:::.gph_me_variables("trt", "reference", ref = 2L))
  expect_identical(gamlssPosthoc:::.gph_me_variables("trt", "sequential")$trt, "sequential")
})

testthat::test_that("tidy glance and augment produce standardized data frames", {
  testthat::skip_if_not_installed("generics"); testthat::skip_if_not_installed("distributions3")
  z <- .make_v03_fit()
  ph <- gamlss_posthoc(z$fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=z$d)
  td <- generics::tidy(ph); gl <- generics::glance(ph); au <- generics::augment(ph)
  testthat::expect_s3_class(td,"data.frame"); testthat::expect_equal(nrow(gl),1L); testthat::expect_true(".fitted_estimand" %in% names(au))
})

testthat::test_that("LaTeX exporter is self contained", {
  z <- .make_v03_fit(); ph <- gamlss_posthoc(z$fit,"trt",estimand="parameter",what="mu",contrast="none",uncertainty="none",data=z$d)
  tx <- export_to_latex(ph)
  testthat::expect_true(grepl("\\\\begin\\{tabular\\}",tx))
  f <- tempfile(fileext=".tex"); export_to_latex(ph,f); testthat::expect_true(file.exists(f))
})

testthat::test_that("Markdown report can be generated without optional rendering stack", {
  z <- .make_v03_fit(); ph <- gamlss_posthoc(z$fit,"trt",estimand="parameter",what="mu",contrast="none",uncertainty="none",data=z$d)
  f <- tempfile(fileext=".md"); out <- generate_report(ph,output="md",file=f)
  testthat::expect_true(file.exists(out)); txt <- readLines(out,warn=FALSE)
  testthat::expect_true(any(grepl("Estimand",txt)))
  testthat::expect_true(any(grepl("Model diagnostics",txt,fixed=TRUE)))
})

testthat::test_that("Word/flextable exports are optional and create files", {
  z <- .make_v03_fit()
  ph <- gamlss_posthoc(z$fit, "trt", estimand = "mean", contrast = "pairwise",
                       uncertainty = "none", data = z$d)
  if (requireNamespace("flextable", quietly = TRUE)) {
    testthat::expect_s3_class(as_flextable(ph), "flextable")
  }
  if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
    f <- tempfile(fileext = ".docx")
    export_to_word(ph, f)
    testthat::expect_true(file.exists(f))
  }
})

testthat::test_that("modelsummary and parameters integrations use standard methods", {
  z <- .make_v03_fit()
  ph <- gamlss_posthoc(z$fit, "trt", estimand = "mean", contrast = "pairwise",
                       uncertainty = "none", data = z$d)
  if (requireNamespace("parameters", quietly = TRUE)) {
    pp <- parameters::model_parameters(ph)
    testthat::expect_true(is.data.frame(pp))
    testthat::expect_true(all(c("Parameter", "Coefficient") %in% names(pp)))
  }
  if (requireNamespace("modelsummary", quietly = TRUE)) {
    tab <- modelsummary::modelsummary(list(posthoc = ph), output = "data.frame")
    testthat::expect_true(is.data.frame(tab))
  }
})

testthat::test_that("automatic Markdown reports may include a figure", {
  testthat::skip_if_not_installed("ggplot2")
  z <- .make_v03_fit()
  ph <- gamlss_posthoc(z$fit, "trt", estimand = "mean", contrast = "pairwise",
                       uncertainty = "none", data = z$d)
  f <- tempfile(fileext = ".md")
  generate_report(ph, output = "md", file = f, include_plots = TRUE)
  testthat::expect_true(file.exists(f))
  txt <- readLines(f, warn = FALSE)
  testthat::expect_true(any(grepl("## Figure", txt, fixed = TRUE)))
})

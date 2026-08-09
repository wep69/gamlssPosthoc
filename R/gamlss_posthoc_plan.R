#' Diagnose post-hoc capabilities for a fitted GAMLSS model
#'
#' @description
#' Inspects model class, family, distributional parameters, smooth terms,
#' optional dependencies, and the requested estimand. The result documents
#' which engines are eligible and explains the recommended engine and
#' uncertainty layer before inference is run.
#'
#' @param object A fitted `gamlss` or `gamlssZadj` model.
#' @param estimand Requested estimand accepted by [gamlss_posthoc()].
#' @param what Distributional parameter for a parameter-wise estimand.
#' @param contrast Requested level-comparison geometry.
#' @param comparison Scientific contrast scale.
#' @param population Target standardization population.
#' @param weights Weighting rule used to determine engine eligibility. Numeric
#'   observation weights rule out the `emmeans` reference-grid engine.
#' @param uncertainty Requested uncertainty layer.
#' @return An object of class `gamlss_posthoc_plan`.
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(2)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 20)))
#'   d$y <- gamlss.dist::rGA(nrow(d),
#'     mu = ifelse(d$trt == "A", 2, 2.6), sigma = 0.3)
#'   fit <- gamlss::gamlss(y ~ trt, data = d, family = gamlss.dist::GA,
#'                         trace = FALSE)
#'
#'   # Which engine is eligible, and why the others are not
#'   gamlss_posthoc_plan(fit, estimand = "mean")
#' }
#'
#' # Normal response with a `pb()` smoother: the capability matrix reports
#' # which parameter carries a smoother and how that constrains routing.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(71)
#'   d <- data.frame(x = runif(60, 0, 10))
#'   d$y <- gamlss.dist::rNO(nrow(d), mu = 5 + 0.8 * d$x, sigma = 1)
#'   # `gamlss::pb()` is qualified because examples run with only this
#'   # package attached.
#'   fit <- gamlss::gamlss(y ~ gamlss::pb(x), family = gamlss.dist::NO,
#'                         data = d, trace = FALSE)
#'   gamlss_posthoc_plan(fit, estimand = "parameter", what = "mu",
#'                       population = "reference")
#' }
#'
#' # Zero-adjusted Gamma, asking for the exact mass at zero. The plan lists
#' # every distributional parameter discovered in the fit, including `nu`.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(72)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 40)))
#'   d$y <- gamlss.dist::rZAGA(nrow(d), mu = 2, sigma = 0.4,
#'                             nu = ifelse(d$trt == "A", 0.35, 0.10))
#'   fit <- gamlss::gamlss(y ~ trt, nu.formula = ~ trt,
#'                         family = gamlss.dist::ZAGA, data = d, trace = FALSE)
#'   gamlss_posthoc_plan(fit, estimand = "prob_zero")
#' }
#' @export
gamlss_posthoc_plan <- function(object, estimand = "parameter", what = "mu",
                                 contrast = "none", comparison = "difference",
                                 population = "observed", weights = NULL,
                                 uncertainty = "auto") {
  if (!.gph_is_gamlss(object)) stop("`object` must inherit from 'gamlss' or 'gamlssZadj'.", call. = FALSE)
  estimand <- match.arg(estimand, c("parameter", "mean", "variance", "quantile", "prob_zero", "custom"))
  contrast <- match.arg(contrast, c("none", "pairwise", "reference", "sequential", "poly", "opoly"))
  comparison <- match.arg(comparison, c("difference", "ratio", "log_ratio", "percent_change"))
  population <- .gph_resolve_population(population)
  uncertainty <- match.arg(uncertainty, c("auto", "delta", "simulation", "bootstrap", "none"))
  ad <- .gph_adapter(object)
  if (estimand == "parameter" && !what %in% ad$parameters) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(ad$parameters, collapse = ", "), ".", call. = FALSE)
  }
  elig <- .gph_engine_eligibility(object, estimand, what, contrast, comparison, population, weights)
  route <- .gph_choose_engine(object, estimand, what, contrast, comparison, "auto", population, weights)
  planned_engine <- route$engine
  unc <- .gph_choose_uncertainty(planned_engine, uncertainty, estimand, elig$smoother)
  unc_recommended <- unc
  if (planned_engine == "emmeans" && !unc %in% c("delta", "none")) unc_recommended <- "delta"
  if (planned_engine == "distribution" && unc %in% c("delta", "simulation")) unc_recommended <- "bootstrap"
  if (planned_engine == "marginaleffects" && identical(unc, "bootstrap")) planned_engine <- "distribution"
  if (uncertainty == "auto" && planned_engine == "distribution" && estimand != "parameter") {
    unc_recommended <- "none; use bootstrap explicitly when interval inference is required"
  }
  dep_names <- c("gamlss", "gamlss.dist", "gamlss.inf", "distributions3", "emmeans",
                 "marginaleffects", "future", "future.apply")
  deps <- data.frame(
    package = dep_names,
    installed = vapply(dep_names, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)),
    stringsAsFactors = FALSE
  )
  capabilities <- data.frame(
    engine = c("emmeans", "marginaleffects", "distribution"),
    eligible = c(elig$emmeans, elig$marginaleffects, TRUE),
    available = c(requireNamespace("emmeans", quietly = TRUE),
                  requireNamespace("marginaleffects", quietly = TRUE),
                  requireNamespace("gamlss", quietly = TRUE) &&
                    requireNamespace("distributions3", quietly = TRUE) &&
                    (!ad$zero_adjusted || (requireNamespace("gamlss.dist", quietly = TRUE) &&
                                           requireNamespace("gamlss.inf", quietly = TRUE)))),
    reason = c(
      if (elig$emmeans) "Supported single parameter on an explicit reference population, with no selected smoother." else "Requires ordinary mu/sigma/nu/tau parameter, no selected smoother, arithmetic differences, population=reference, and non-numeric reference-grid weights.",
      if (elig$marginaleffects) "Parameter-wise standardization is eligible; runtime capability is still checked." else "Current adapter reserves marginaleffects for ordinary parameter-wise GAMLSS requests.",
      if (ad$zero_adjusted) "Uses the full positive distribution plus the zero mass." else "Uses the full predictive distribution via prodist()."
    ), stringsAsFactors = FALSE
  )
  parameter_capabilities <- ad$parameter_table
  if (nrow(parameter_capabilities)) {
    parameter_capabilities$emmeans_eligible <-
      !ad$zero_adjusted & parameter_capabilities$parameter %in% c("mu", "sigma", "nu", "tau") &
      !parameter_capabilities$smoother & identical(population, "reference") & !is.numeric(weights)
    parameter_capabilities$marginaleffects_candidate <-
      !ad$zero_adjusted & parameter_capabilities$parameter %in% ad$parameters
    parameter_capabilities$distribution_prediction <- TRUE
  }
  warnings <- character()
  if (ad$zero_adjusted && !requireNamespace("gamlss.dist", quietly = TRUE)) warnings <- c(warnings, "gamlss.dist is needed to reconstruct the positive distribution generically.")
  if (elig$smoother) warnings <- c(warnings, "A smoother is present in the selected submodel; emmeans is intentionally avoided.")
  if (contrast %in% c("poly", "opoly") && comparison != "difference") warnings <- c(warnings, "Polynomial contrasts must use arithmetic linear combinations.")
  if (is.numeric(weights) && comparison %in% c("ratio", "log_ratio", "percent_change") && uncertainty %in% c("auto", "delta", "simulation")) {
    warnings <- c(warnings, "Numeric user weights with nonlinear contrast scales use standardized group predictions; direct marginaleffects covariance shortcuts are intentionally avoided. Request bootstrap for interval inference if needed.")
  }
  out <- list(
    overview = data.frame(model_class = ad$model_class, family = ad$family,
                          zero_adjusted = ad$zero_adjusted,
                          parameters = paste(ad$parameters, collapse = ", "), stringsAsFactors = FALSE),
    parameter_table = ad$parameter_table,
    parameter_capabilities = parameter_capabilities,
    dependencies = deps,
    capabilities = capabilities,
    request = data.frame(estimand = estimand, parameter = what, contrast = contrast,
                         comparison = comparison, population = population,
                         weighting = if (is.null(weights)) "default" else if (is.numeric(weights)) "numeric user weights" else as.character(weights)[1L],
                         uncertainty = uncertainty, stringsAsFactors = FALSE),
    recommendation = data.frame(engine = planned_engine, uncertainty = unc_recommended,
                                reason = if (planned_engine == "emmeans") "Direct parameter-wise reference-grid marginal means are supported."
                                else if (planned_engine == "marginaleffects") "Prediction-based standardization is preferred for this parameter-wise request."
                                else if (route$engine == "marginaleffects" && planned_engine == "distribution") "Refit bootstrap uncertainty is handled by the distribution engine for version-stable behavior."
                                else "The requested target depends on the full distribution or multiple parameters.",
                                stringsAsFactors = FALSE),
    warnings = warnings
  )
  class(out) <- "gamlss_posthoc_plan"
  out
}

#' @method print gamlss_posthoc_plan
#' @export
#' @noRd
print.gamlss_posthoc_plan <- function(x, ...) {
  cat("gamlssPosthoc diagnostic plan\n\n")
  print(x$overview, row.names = FALSE)
  cat("\nDistributional parameters\n")
  print(x$parameter_table, row.names = FALSE)
  cat("\nParameter capabilities\n")
  print(x$parameter_capabilities, row.names = FALSE)
  cat("\nEngine capabilities\n")
  print(x$capabilities, row.names = FALSE)
  cat("\nRecommendation\n")
  print(x$recommendation, row.names = FALSE)
  if (length(x$warnings)) cat("\nWarnings\n- ", paste(x$warnings, collapse = "\n- "), "\n", sep = "")
  invisible(x)
}

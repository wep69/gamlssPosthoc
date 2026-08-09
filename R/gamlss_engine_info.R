#' Legacy one-row engine summary
#'
#' @description
#' Compatibility wrapper around [gamlss_posthoc_plan()]. New code should use
#' the richer diagnostic plan directly.
#' @param object A fitted `gamlss` or `gamlssZadj` model.
#' @param estimand Requested estimand.
#' @param what Distributional parameter name.
#' @param population Target population used for routing diagnostics.
#' @param weights Optional weighting rule used for routing diagnostics.
#' @return A one-row data frame.
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(1)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 20)))
#'   d$y <- gamlss.dist::rGA(nrow(d),
#'     mu = ifelse(d$trt == "A", 2, 2.6), sigma = 0.3)
#'   fit <- gamlss::gamlss(y ~ trt, data = d, family = gamlss.dist::GA,
#'                         trace = FALSE)
#'   gamlss_engine_info(fit, estimand = "parameter", what = "mu")
#' }
#'
#' # Normal response with a `pb()` smoother: the smoother makes the emmeans
#' # reference grid unsafe, which the summary reports.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(51)
#'   d <- data.frame(x = runif(60, 0, 10))
#'   d$y <- gamlss.dist::rNO(nrow(d), mu = 5 + 0.8 * d$x - 0.05 * d$x^2,
#'                           sigma = 1)
#'   # `gamlss::pb()` is qualified because examples run with only this
#'   # package attached.
#'   fit <- gamlss::gamlss(y ~ gamlss::pb(x), family = gamlss.dist::NO,
#'                         data = d, trace = FALSE)
#'   gamlss_engine_info(fit, estimand = "parameter", what = "mu",
#'                      population = "reference")
#' }
#'
#' # Lognormal response, asking for the response mean rather than a parameter:
#' # the target depends on more than one parameter, so routing changes.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(52)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 25)))
#'   d$y <- gamlss.dist::rLOGNO(nrow(d),
#'     mu = ifelse(d$trt == "A", 1, 1.5), sigma = 0.4)
#'   fit <- gamlss::gamlss(y ~ trt, family = gamlss.dist::LOGNO,
#'                         data = d, trace = FALSE)
#'   gamlss_engine_info(fit, estimand = "mean")
#' }
#' @export
gamlss_engine_info <- function(object, estimand = "parameter", what = "mu",
                               population = "observed", weights = NULL) {
  p <- gamlss_posthoc_plan(object, estimand = estimand, what = what,
                           population = population, weights = weights)
  sm <- if (what %in% p$parameter_table$parameter) p$parameter_table$smoother[match(what, p$parameter_table$parameter)] else FALSE
  em <- p$capabilities[p$capabilities$engine == "emmeans", ]
  data.frame(
    estimand = estimand,
    parameter = what,
    smoother_in_selected_submodel = isTRUE(sm),
    gamlssZadj = p$overview$zero_adjusted,
    emmeans_safe = isTRUE(em$eligible),
    recommended_engine = p$recommendation$engine,
    recommended_uncertainty = p$recommendation$uncertainty,
    reason = p$recommendation$reason,
    stringsAsFactors = FALSE
  )
}

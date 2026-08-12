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
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(17)
#'   d <- data.frame(x=seq(0,1,length.out=50), trt=factor(rep(c("A","B"),25)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.3+.4*d$x), sigma=.25)
#'   fit <- gamlss::gamlss(y~x, sigma.formula=~trt, family=gamlss.dist::GA,
#'                         data=d, trace=FALSE)
#'   # 1. mu parameter diagnostics
#'   gamlss_engine_info(fit, estimand="parameter", what="mu")
#'   # 2. sigma parameter diagnostics
#'   gamlss_engine_info(fit, estimand="parameter", what="sigma", population="reference")
#'   # 3. Full-distribution mean diagnostics
#'   gamlss_engine_info(fit, estimand="mean", what="mu")
#' }
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

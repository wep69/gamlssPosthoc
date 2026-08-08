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

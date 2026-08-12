#' Summarize full predicted distributions from GAMLSS models
#'
#' @description
#' Returns plug-in summaries of ordinary `gamlss` distributions and generic
#' zero-adjusted `gamlssZadj` distributions. For a zero-adjusted model, the
#' positive-part distribution is reconstructed through `gamlss.dist::GAMLSS()`
#' whenever possible and combined with the predicted zero probability.
#'
#' @param object A fitted `gamlss` or `gamlssZadj` model.
#' @param newdata Data frame containing predictor values at which to predict.
#' @param probs Numeric probabilities in `[0, 1]` for requested quantiles.
#' @param data Optional original model data.
#' @param positive_dist_fun Optional constructor for a positive-part
#'   `distributions3` object when the family cannot be reconstructed generically.
#' @return A data frame containing `newdata`, mean, variance, standard deviation,
#'   exact probability mass at zero, and requested quantiles.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(12)
#'   d <- data.frame(x = seq(0.2, 2, length.out = 60))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.1 + 0.35*d$x), sigma = 0.28)
#'   fit <- gamlss::gamlss(y ~ x, data = d, family = gamlss.dist::GA, trace = FALSE)
#'   # 1. Default 2.5%, 50%, and 97.5% quantiles
#'   gamlss_distribution_summary(fit, data.frame(x = c(.5, 1, 1.5)), data = d)
#'   # 2. Agronomic lower/median/upper predictive summaries
#'   gamlss_distribution_summary(fit, data.frame(x = c(.75, 1.25)),
#'                               probs = c(.10, .50, .90), data = d)
#'   # 3. A single prediction profile
#'   gamlss_distribution_summary(fit, data.frame(x = 1), probs = c(.25,.5,.75), data = d)
#' }
gamlss_distribution_summary <- function(object, newdata, probs = c(0.025, 0.5, 0.975),
                                         data = NULL, positive_dist_fun = NULL) {
  if (!.gph_is_gamlss(object)) stop("`object` must inherit from 'gamlss' or 'gamlssZadj'.", call. = FALSE)
  if (!is.numeric(probs) || any(!is.finite(probs)) || any(probs < 0 | probs > 1)) {
    stop("`probs` must contain finite probabilities between 0 and 1.", call. = FALSE)
  }
  newdata <- as.data.frame(newdata)
  D <- .gph_distribution(object, newdata, data, positive_dist_fun)
  out <- newdata
  if (.gph_is_zadj(object)) {
    out$mean <- .gph_zadj_mean(D)
    out$variance <- .gph_zadj_variance(D)
    out$prob_zero <- D$prob_zero
    for (p in probs) {
      nm <- paste0("q", formatC(100 * p, format = "fg", flag = "0", width = 2))
      out[[nm]] <- .gph_zadj_quantile(D, p)
    }
  } else {
    .gph_require("distributions3", "for distribution summaries.")
    out$mean <- as.numeric(base::mean(D))
    out$variance <- as.numeric(distributions3::variance(D))
    out$prob_zero <- .gph_exact_prob_zero(D)
    for (p in probs) {
      nm <- paste0("q", formatC(100 * p, format = "fg", flag = "0", width = 2))
      out[[nm]] <- as.numeric(stats::quantile(D, probs = p))
    }
  }
  out$sd <- sqrt(out$variance)
  attr(out, "model") <- object
  attr(out, "data") <- data
  class(out) <- c("gamlss_distribution_summary", "data.frame")
  out
}

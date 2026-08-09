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
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.1 + 0.35 * d$x), sigma = 0.28)
#'   fit <- gamlss::gamlss(y ~ x, data = d, family = gamlss.dist::GA, trace = FALSE)
#'   gamlss_distribution_summary(fit, data.frame(x = c(0.5, 1, 1.5)), data = d)
#' }
#'
#' # Lognormal: mean and median differ, and both are returned on the response
#' # scale rather than on the scale of the location parameter.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(31)
#'   d <- data.frame(x = runif(60, 0, 10))
#'   d$y <- gamlss.dist::rLOGNO(nrow(d), mu = 0.5 + 0.1 * d$x, sigma = 0.4)
#'   fit <- gamlss::gamlss(y ~ x, family = gamlss.dist::LOGNO,
#'                         data = d, trace = FALSE)
#'   gamlss_distribution_summary(fit, data.frame(x = c(2, 5, 8)), data = d)
#' }
#'
#' # Zero-adjusted Gamma: a mixed distribution, with an atom at zero and a
#' # continuous positive part. `prob_zero` is the exact mass at zero, not a
#' # value read off the cumulative distribution of a continuous family.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(32)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 40)))
#'   d$y <- gamlss.dist::rZAGA(nrow(d), mu = ifelse(d$trt == "A", 2, 3),
#'                             sigma = 0.4,
#'                             nu = ifelse(d$trt == "A", 0.35, 0.10))
#'   fit <- gamlss::gamlss(y ~ trt, nu.formula = ~ trt,
#'                         family = gamlss.dist::ZAGA, data = d, trace = FALSE)
#'   nd <- data.frame(trt = factor(c("A", "B"), levels = levels(d$trt)))
#'   gamlss_distribution_summary(fit, nd, data = d)
#' }
#'
#' # Poisson counts: a discrete family, where the mass at zero is exp(-mu).
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(33)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 30)))
#'   d$y <- gamlss.dist::rPO(nrow(d), mu = ifelse(d$trt == "A", 1.5, 4))
#'   fit <- gamlss::gamlss(y ~ trt, family = gamlss.dist::PO,
#'                         data = d, trace = FALSE)
#'   nd <- data.frame(trt = factor(c("A", "B"), levels = levels(d$trt)))
#'   gamlss_distribution_summary(fit, nd, probs = c(0.1, 0.5, 0.9), data = d)
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
      ## `gamlss.dist:::quantile.GAMLSS()` forwards `...` to the family's
      ## q-function, which has no `names` argument, so `names=` must not be
      ## passed here. `as.numeric()` already drops any names.
      out[[nm]] <- as.numeric(stats::quantile(D, probs = p))
    }
  }
  out$sd <- sqrt(out$variance)
  out
}

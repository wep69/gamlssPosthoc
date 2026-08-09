# Generic probability-distribution helpers ------------------------------------

.gph_positive_distribution <- function(object, pars, positive_dist_fun = NULL) {
  if (is.function(positive_dist_fun)) {
    D <- positive_dist_fun(pars = pars, object = object)
    return(D)
  }
  .gph_require("gamlss.dist", "to construct the positive-part distribution.")
  .gph_require("distributions3", "to summarize probability distributions.")
  fam <- .gph_model_family(object)
  if (!is.character(fam) || length(fam) != 1L || is.na(fam) || !nzchar(fam)) {
    stop("Could not identify the positive-part GAMLSS family. Supply `positive_dist_fun`.", call. = FALSE)
  }
  # The current CRAN GAMLSS() distribution constructor supports the standard
  # gamlss.dist parameter interface.  The adapter layer isolates this detail.
  constructor_parameters <- setdiff(names(formals(gamlss.dist::GAMLSS)), "family")
  allowed <- intersect(names(pars), constructor_parameters)
  args <- c(list(family = fam), pars[allowed])
  ans <- try(do.call(gamlss.dist::GAMLSS, args), silent = TRUE)
  if (inherits(ans, "try-error")) {
    stop("Could not construct the positive-part distributions3 object for family '", fam,
         "'. This can occur for locally generated/transformed families. Supply `positive_dist_fun`. ",
         "Original error: ", as.character(ans), call. = FALSE)
  }
  ans
}

.gph_distribution <- function(object, newdata, data = NULL, positive_dist_fun = NULL) {
  .gph_require("distributions3", "for distributional summaries.")
  if (!.gph_is_zadj(object)) {
    .gph_require("gamlss", "for `prodist()`.")
    ## `prodist.gamlss()` is registered as an S3 method for the
    ## distributions3 generic but is not exported by gamlss, so it must be
    ## reached through the generic rather than with `gamlss::prodist`.
    return(distributions3::prodist(object,
                                   newdata = .gph_clean_newdata(newdata, data),
                                   data = data))
  }
  pars <- .gph_predict_parameters(object, newdata, data)
  if (is.null(pars$xi0)) stop("The zero-adjusted model does not expose an `xi0` parameter.", call. = FALSE)
  list(
    positive = .gph_positive_distribution(object, pars, positive_dist_fun),
    prob_zero = pmin(pmax(as.numeric(pars$xi0), 0), 1),
    pars = pars,
    family = .gph_model_family(object)
  )
}

.gph_zadj_mean <- function(z) {
  mplus <- as.numeric(base::mean(z$positive))
  (1 - z$prob_zero) * mplus
}

.gph_zadj_variance <- function(z) {
  .gph_require("distributions3", "for variance calculations.")
  mplus <- as.numeric(base::mean(z$positive))
  vplus <- as.numeric(distributions3::variance(z$positive))
  h <- z$prob_zero
  (1 - h) * vplus + h * (1 - h) * mplus^2
}

.gph_zadj_quantile <- function(z, prob) {
  .gph_require("distributions3", "for quantile calculations.")
  if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) || prob < 0 || prob > 1) {
    stop("`prob` must be one finite probability between 0 and 1.", call. = FALSE)
  }
  h <- z$prob_zero
  ans <- numeric(length(h))
  positive <- prob > h
  if (any(positive)) {
    qadj <- (prob - h[positive]) / (1 - h[positive])
    qadj <- pmin(pmax(qadj, 0), 1)
    idx <- which(positive)
    ans[idx] <- vapply(seq_along(idx), function(j) {
      ## `names=` must not be forwarded: gamlss.dist's quantile method passes
      ## `...` straight to the family q-function, which has no such argument.
      as.numeric(stats::quantile(z$positive[idx[j]], probs = qadj[j]))[1L]
    }, numeric(1))
  }
  ans
}

.gph_zadj_random <- function(z, n = 1L) {
  .gph_require("distributions3", "for random generation.")
  n <- as.integer(n)[1L]
  if (!is.finite(n) || n < 1L) stop("`n` must be a positive integer.", call. = FALSE)
  h <- z$prob_zero
  one <- function() {
    ypos <- vapply(seq_along(h), function(i) {
      as.numeric(distributions3::random(z$positive[i], n = 1L))[1L]
    }, numeric(1))
    iszero <- stats::rbinom(length(h), size = 1L, prob = h) == 1L
    ifelse(iszero, 0, ypos)
  }
  if (n == 1L) return(one())
  replicate(n, one())
}

## Exact probability mass at zero.
##
## Three cases must be separated, because a single rule is wrong for at least
## one of them:
##
##   discrete (PO, NBI, ...)   P(Y=0) is the density at zero.
##   mixed    (ZAGA, ZAIG, ...) `is_discrete()` and `is_continuous()` are both
##                             FALSE. Support is non-negative, so P(Y<0)=0 and
##                             the atom at zero is exactly F(0).
##   continuous (GA, NO, ...)  There is no atom, so the mass is 0. Note that
##                             F(0) must NOT be used here: for a family with
##                             support on the whole real line, such as NO,
##                             F(0) is a tail probability, not a point mass.
.gph_exact_prob_zero <- function(D) {
  .gph_require("distributions3", "for exact probability masses.")
  n <- length(D)
  discrete <- distributions3::is_discrete(D)
  if (length(discrete) == 1L) discrete <- rep(discrete, n)
  continuous <- distributions3::is_continuous(D)
  if (length(continuous) == 1L) continuous <- rep(continuous, n)

  ans <- numeric(n)
  if (any(discrete)) {
    ans[discrete] <- as.numeric(distributions3::pdf(D[discrete], 0))
  }
  mixed <- !discrete & !continuous
  if (any(mixed)) {
    val <- try(as.numeric(distributions3::cdf(D[mixed], 0)), silent = TRUE)
    if (!inherits(val, "try-error") && length(val) == sum(mixed)) {
      ans[mixed] <- pmin(pmax(val, 0), 1)
    }
  }
  ans
}

.gph_predict_estimand <- function(object, newdata, estimand = "parameter",
                                  what = NULL, prob = 0.5, custom_fun = NULL,
                                  data = NULL, positive_dist_fun = NULL, positive_mean_fun = NULL) {
  estimand <- match.arg(estimand,
                        c("parameter", "mean", "variance", "quantile", "prob_zero", "custom"))
  if (estimand == "parameter") {
    if (is.null(what) || !nzchar(what)) stop("Supply `what` for estimand='parameter'.", call. = FALSE)
    return(.gph_predict_parameter(object, newdata, what, data))
  }

  if (estimand == "custom") {
    if (!is.function(custom_fun)) stop("`custom_fun` must be a function for estimand='custom'.", call. = FALSE)
    pars <- .gph_predict_parameters(object, newdata, data)
    val <- custom_fun(pars = pars, newdata = newdata, object = object)
    if (length(val) != nrow(newdata)) stop("`custom_fun` must return one value per row of `newdata`.", call. = FALSE)
    return(as.numeric(val))
  }

  if (.gph_is_zadj(object) && estimand == "mean" && is.function(positive_mean_fun)) {
    pars <- .gph_predict_parameters(object, newdata, data)
    if (is.null(pars$xi0)) stop("The zero-adjusted model does not expose `xi0`.", call. = FALSE)
    mp <- positive_mean_fun(pars = pars, newdata = newdata, object = object)
    if (length(mp) != nrow(newdata)) stop("`positive_mean_fun` must return one value per prediction row.", call. = FALSE)
    return((1 - pars$xi0) * as.numeric(mp))
  }

  D <- .gph_distribution(object, newdata, data, positive_dist_fun)
  if (.gph_is_zadj(object)) {
    if (estimand == "prob_zero") return(D$prob_zero)
    if (estimand == "mean") return(.gph_zadj_mean(D))
    if (estimand == "variance") return(.gph_zadj_variance(D))
    if (estimand == "quantile") return(.gph_zadj_quantile(D, prob))
  }

  if (estimand == "prob_zero") return(.gph_exact_prob_zero(D))
  if (estimand == "mean") return(as.numeric(base::mean(D)))
  if (estimand == "variance") return(as.numeric(distributions3::variance(D)))
  if (estimand == "quantile") {
    if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) || prob < 0 || prob > 1) {
      stop("`prob` must be one finite probability between 0 and 1.", call. = FALSE)
    }
    return(as.numeric(stats::quantile(D, probs = prob)))
  }
  stop("Unknown estimand.", call. = FALSE)
}

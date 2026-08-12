# Generic probability-distribution helpers ------------------------------------

# `prodist()` for GAMLSS fits is available in some 'gamlss' releases as a
# registered S3 method and in others only as an internal object.  Resolve it
# defensively and return NULL when it cannot be used, so callers can fall back
# to the public parameter-based construction.
.gph_prodist_gamlss <- function(object, newdata, data = NULL) {
  if (!requireNamespace("gamlss", quietly = TRUE)) return(NULL)
  fn <- NULL
  if (requireNamespace("distributions3", quietly = TRUE)) {
    fn <- tryCatch(utils::getS3method("prodist", "gamlss", optional = TRUE),
                   error = function(e) NULL)
  }
  if (is.null(fn)) {
    fn <- tryCatch(get("prodist", envir = asNamespace("gamlss"), mode = "function"),
                   error = function(e) NULL)
  }
  if (!is.function(fn)) return(NULL)
  ans <- try(fn(object, newdata = newdata, data = data), silent = TRUE)
  if (inherits(ans, "try-error")) NULL else ans
}

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
    ans <- .gph_prodist_gamlss(object, newdata = newdata, data = data)
    if (!is.null(ans)) return(ans)
    # Portable fallback: rebuild the predictive distribution from the
    # predicted distributional parameters using the public gamlss.dist API.
    pars <- .gph_predict_parameters(object, newdata, data)
    return(.gph_positive_distribution(object, pars, positive_dist_fun))
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

.gph_exact_prob_zero <- function(D) {
  .gph_require("distributions3", "for exact probability masses.")
  discrete <- distributions3::is_discrete(D)
  if (length(discrete) == 1L) discrete <- rep(discrete, length(D))
  ans <- numeric(length(discrete))
  if (any(discrete)) ans[discrete] <- as.numeric(distributions3::pdf(D[discrete], 0))
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

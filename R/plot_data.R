# Plot-data layer -------------------------------------------------------------

.gph_long_group_label <- function(df, cols) {
  if (!length(cols)) return(rep("All", nrow(df)))
  apply(df[, cols, drop = FALSE], 1L, function(z) paste(paste(cols, z, sep = "="), collapse = ", "))
}

.gph_numeric_grid <- function(data, x, n = 100L) {
  n <- max(3L, as.integer(n)[1L])
  seq(min(data[[x]], na.rm = TRUE), max(data[[x]], na.rm = TRUE), length.out = n)
}

.gph_plot_eval <- function(object, x = NULL, by = NULL, at = list(), newdata = NULL,
                           population = "observed", weights = NULL, data = NULL, n = 100L) {
  data <- .gph_get_data(object, data)
  if (!is.null(newdata)) {
    nd <- .gph_restore_classes(as.data.frame(newdata), data)
    nd$.gph_group_id <- seq_len(nrow(nd))
    nd$.gph_weight <- 1
    groups <- nd[, setdiff(names(nd), c(".gph_group_id", ".gph_weight")), drop = FALSE]
    groups$.gph_group_id <- seq_len(nrow(groups))
    return(list(eval_data = nd, groups = groups, population = "newdata",
                weighting = "none", base_data = nd, weights = nd$.gph_weight))
  }
  if (is.null(x) || !nzchar(x)) stop("Supply `x` or `newdata`.", call. = FALSE)
  if (!x %in% names(data)) stop("`x` is not present in model data.", call. = FALSE)
  if (is.numeric(data[[x]]) && is.null(at[[x]])) at[[x]] <- .gph_numeric_grid(data, x, n)
  .gph_build_eval_data(object, data, specs = x, by = by, at = at,
                       population = population, weights = .gph_resolve_weights(weights, data, population))
}

.gph_group_distribution_curve <- function(object, eval_info, response_grid = NULL,
                                          type = c("density", "cdf", "survival"),
                                          data = NULL, positive_dist_fun = NULL, n_response = 200L) {
  type <- match.arg(type)
  .gph_require("distributions3", "for distribution plots.")
  ev <- eval_info$eval_data
  D <- .gph_distribution(object, ev, data, positive_dist_fun)
  gid <- ev$.gph_group_id
  ids <- eval_info$groups$.gph_group_id
  out <- list()
  for (g in ids) {
    ii <- which(gid == g)
    w <- ev$.gph_weight[ii]; w <- w / sum(w)
    if (is.null(response_grid)) {
      if (.gph_is_zadj(object)) {
        qs <- vapply(c(.001, .999), function(p) {
          q <- vapply(ii, function(j) as.numeric(stats::quantile(D$positive[j], p))[1L], numeric(1))
          stats::weighted.mean(q, w, na.rm = TRUE)
        }, numeric(1))
        lo <- 0; hi <- max(qs[2], .Machine$double.eps)
      } else {
        qs <- vapply(c(.001, .999), function(p) {
          q <- as.numeric(stats::quantile(D[ii], p))
          stats::weighted.mean(q, w, na.rm = TRUE)
        }, numeric(1))
        lo <- qs[1]; hi <- qs[2]
      }
      rg <- seq(lo, hi, length.out = max(50L, as.integer(n_response)))
    } else rg <- sort(unique(as.numeric(response_grid)))
    vals <- vapply(rg, function(y) {
      if (.gph_is_zadj(object)) {
        fp <- as.numeric(distributions3::pdf(D$positive[ii], y))
        cp <- as.numeric(distributions3::cdf(D$positive[ii], y))
        if (type == "density") z <- if (y == 0) rep(0, length(ii)) else (1 - D$prob_zero[ii]) * fp
        else {
          zc <- if (y < 0) rep(0, length(ii)) else D$prob_zero[ii] + (1 - D$prob_zero[ii]) * cp
          z <- if (type == "cdf") zc else 1 - zc
        }
      } else {
        z <- if (type == "density") as.numeric(distributions3::pdf(D[ii], y)) else {
          zc <- as.numeric(distributions3::cdf(D[ii], y))
          if (type == "cdf") zc else 1 - zc
        }
      }
      stats::weighted.mean(z, w, na.rm = TRUE)
    }, numeric(1))
    rr0 <- eval_info$groups[eval_info$groups$.gph_group_id == g, , drop = FALSE]
    label_cols <- setdiff(names(rr0), ".gph_group_id")
    group_label <- .gph_long_group_label(rr0, label_cols)[1L]
    rr <- rr0[rep(1L, length(rg)), , drop = FALSE]
    rr$.gph_label <- group_label
    rr$response <- rg; rr$value <- vals; rr$curve <- type
    if (.gph_is_zadj(object)) rr$prob_zero <- stats::weighted.mean(D$prob_zero[ii], w, na.rm = TRUE)
    out[[length(out) + 1L]] <- rr
  }
  z <- do.call(rbind, out); rownames(z) <- NULL; .gph_relabel_groups(z)
}

.gph_mixture_quantile <- function(object, D, ii, w, p) {
  w <- w / sum(w)
  if (.gph_is_zadj(object)) {
    hbar <- sum(w * D$prob_zero[ii])
    if (p <= hbar) return(0)
    lo <- 0
    hi <- max(vapply(ii, function(j) as.numeric(stats::quantile(D$positive[j], .9999))[1L], numeric(1)), na.rm = TRUE)
    Fmix <- function(y) sum(w * (D$prob_zero[ii] + (1 - D$prob_zero[ii]) * as.numeric(distributions3::cdf(D$positive[ii], y))))
  } else {
    lo <- min(as.numeric(stats::quantile(D[ii], .0001)), na.rm = TRUE)
    hi <- max(as.numeric(stats::quantile(D[ii], .9999)), na.rm = TRUE)
    Fmix <- function(y) sum(w * as.numeric(distributions3::cdf(D[ii], y)))
  }
  if (!is.finite(lo) || !is.finite(hi) || lo == hi) return(lo)
  f <- function(y) Fmix(y) - p
  flo <- f(lo); fhi <- f(hi)
  if (flo >= 0) return(lo)
  if (fhi <= 0) return(hi)
  stats::uniroot(f, c(lo, hi), tol = 1e-7)$root
}

.gph_group_quantiles <- function(object, eval_info, probs, data = NULL, positive_dist_fun = NULL) {
  .gph_require("distributions3", "for quantile plots.")
  ev <- eval_info$eval_data; D <- .gph_distribution(object, ev, data, positive_dist_fun)
  out <- list()
  for (g in eval_info$groups$.gph_group_id) {
    ii <- which(ev$.gph_group_id == g); w <- ev$.gph_weight[ii]
    q <- vapply(probs, function(p) .gph_mixture_quantile(object, D, ii, w, p), numeric(1))
    rr <- eval_info$groups[eval_info$groups$.gph_group_id == g, , drop = FALSE]
    rr <- rr[rep(1L, length(probs)), , drop = FALSE]
    rr$prob <- probs; rr$quantile <- q
    out[[length(out) + 1L]] <- rr
  }
  z <- do.call(rbind, out); rownames(z) <- NULL; .gph_relabel_groups(z)
}

.gph_zero_components <- function(object, eval_info, data = NULL, positive_dist_fun = NULL) {
  if (!.gph_is_zadj(object)) stop("Zero-adjusted plots require a `gamlssZadj` model.", call. = FALSE)
  D <- .gph_distribution(object, eval_info$eval_data, data, positive_dist_fun)
  ev <- eval_info$eval_data; mp <- as.numeric(base::mean(D$positive)); mm <- (1-D$prob_zero)*mp
  out <- list()
  for (g in eval_info$groups$.gph_group_id) {
    ii <- which(ev$.gph_group_id == g); w <- ev$.gph_weight[ii]
    rr <- eval_info$groups[eval_info$groups$.gph_group_id == g, , drop = FALSE]
    vals <- c(prob_zero = stats::weighted.mean(D$prob_zero[ii], w),
              positive_mean = stats::weighted.mean(mp[ii], w),
              marginal_mean = stats::weighted.mean(mm[ii], w))
    r2 <- rr[rep(1L, 3L), , drop = FALSE]; r2$component <- names(vals); r2$estimate <- as.numeric(vals)
    out[[length(out)+1L]] <- r2
  }
  z <- do.call(rbind, out); rownames(z) <- NULL; .gph_relabel_groups(z)
}

#' Build standardized data for GAMLSS graphics
#'
#' @description Creates an auditable data layer before any ggplot is built. It
#' supports parameter effects, marginal estimands, contrasts, full predictive
#' distributions, marginal mixture quantiles, zero-adjusted decomposition,
#' quantitative trends, derivatives, optima, and two-predictor surfaces.
#'
#' @param object Fitted GAMLSS model or a `gamlss_posthoc`/`gamlss_trend` object for compatible types.
#' @param type Plot-data type: `parameters`, `estimand`, `contrasts`, `distribution`,
#' `quantiles`, `zero_adjusted`, `trend`, `derivative`, `optimum`, or `surface`.
#' @param x,y Predictor names. `y` is used only for surfaces.
#' @param by Optional conditioning variables.
#' @param parameter,parameters Distributional parameter(s).
#' @param parameter_scale Parameter prediction scale, `response` or `link`.
#' @param estimand,what Estimand and parameter passed to post-hoc/trend engines.
#' @param at Named list of evaluation values.
#' @param newdata Optional explicit prediction rows.
#' @param population,weights Standardization population and weights.
#' @param probs Quantile probabilities.
#' @param curve Distribution curve type: density, CDF, or survival.
#' @param response_grid Optional response grid for distribution curves.
#' @param n Number of predictor grid points.
#' @param n_response Number of response grid points.
#' @param uncertainty,B Bootstrap/inference controls forwarded when relevant.
#' @param positive_dist_fun Optional positive-distribution constructor for zero-adjusted models.
#' @param data Optional original data.
#' @param ... Additional arguments forwarded to the underlying post-hoc or trend function.
#' @return A data frame with class `gamlss_plot_data`.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- gamlss_plot_data(fit, "parameters", x = "dose", parameters = c("mu", "sigma"), data = d, n = 12)
#'   p2 <- gamlss_plot_data(fit, "distribution", x = "trt", curve = "density", data = d, n_response = 40)
#'   p3 <- gamlss_plot_data(fit, "quantiles", x = "dose", probs = c(.1, .5, .9), data = d, n = 12)
#' }
gamlss_plot_data <- function(object, type = c("parameters", "estimand", "contrasts", "distribution",
                                              "quantiles", "zero_adjusted", "trend", "derivative",
                                              "optimum", "surface"),
                             x = NULL, y = NULL, by = NULL, parameter = "mu", parameters = NULL,
                             parameter_scale = c("response", "link"),
                             estimand = "mean", what = "mu", at = list(), newdata = NULL,
                             population = "observed", weights = NULL, probs = c(.05,.25,.5,.75,.95),
                             curve = c("density","cdf","survival"), response_grid = NULL,
                             n = 100L, n_response = 200L, uncertainty = "none", B = 199L,
                             positive_dist_fun = NULL, data = NULL, ...) {
  type <- match.arg(type); curve <- match.arg(curve); parameter_scale <- match.arg(parameter_scale)
  if (inherits(object, "gamlss_posthoc") && type == "contrasts") {
    z <- object$contrasts
    if (is.null(z)) stop("The post-hoc object contains no contrasts.", call.=FALSE)
    class(z) <- c("gamlss_plot_data", class(z)); attr(z,"plot_type") <- type; return(z)
  }
  if (inherits(object, "gamlss_trend") && type %in% c("trend","derivative","optimum")) {
    z <- if (type == "optimum") object$special_points else object$values
    class(z) <- c("gamlss_plot_data", class(z)); attr(z,"plot_type") <- type; return(z)
  }
  if (!.gph_is_gamlss(object)) stop("`object` must be a GAMLSS model or a compatible gamlssPosthoc result.", call.=FALSE)
  data <- .gph_get_data(object, data)
  if (type == "parameters") {
    pars <- parameters %||% parameter
    rows <- lapply(pars, function(p) {
      if (!p %in% .gph_model_parameters(object)) return(NULL)
      if (parameter_scale == "link") {
        ei <- .gph_plot_eval(object, x=x, by=by, at=at, newdata=newdata,
                             population=population, weights=weights, data=data, n=n)
        z <- .gph_predict_parameter_standardized(object, ei, p, data=data, scale="link")
        z <- .gph_relabel_groups(z)
      } else if (!is.null(x) && is.numeric(data[[x]])) {
        tr <- gamlss_trend(object, x=x, by=by, n=n, estimand="parameter", what=p,
                           population=population, weights=weights, uncertainty=uncertainty,
                           data=data, B=B)
        z <- tr$values
      } else {
        z <- gamlss_posthoc(object, specs=x, by=by, estimand="parameter", what=p,
                            population=population, weights=weights, at=at, contrast="none",
                            uncertainty=uncertainty, data=data, B=B)$estimates
      }
      z$parameter <- p; z
    })
    z <- do.call(rbind, Filter(Negate(is.null), rows))
  } else if (type == "estimand") {
    if (!is.null(x) && is.numeric(data[[x]])) z <- gamlss_trend(object,x=x,by=by,n=n,estimand=estimand,what=what,
        population=population,weights=weights,uncertainty=uncertainty,data=data,B=B,...)$values
    else z <- gamlss_posthoc(object,specs=x,by=by,estimand=estimand,what=what,population=population,weights=weights,
        at=at,contrast="none",uncertainty=uncertainty,data=data,B=B,...)$estimates
  } else if (type == "contrasts") {
    z <- gamlss_posthoc(object, specs=x, by=by, estimand=estimand, what=what, population=population,
                        weights=weights, at=at, contrast="pairwise", uncertainty=uncertainty, data=data, B=B, ...)$contrasts
  } else if (type %in% c("distribution","quantiles","zero_adjusted")) {
    ei <- .gph_plot_eval(object,x,by,at,newdata,population,weights,data,n)
    if (type == "distribution") z <- .gph_group_distribution_curve(object,ei,response_grid,curve,data,positive_dist_fun,n_response)
    if (type == "quantiles") z <- .gph_group_quantiles(object,ei,probs,data,positive_dist_fun)
    if (type == "zero_adjusted") z <- .gph_zero_components(object,ei,data,positive_dist_fun)
  } else if (type %in% c("trend","derivative","optimum")) {
    m <- switch(type, trend="curve", derivative="derivative", optimum="optimum")
    tr <- gamlss_trend(object,x=x,by=by,n=n,method=m,estimand=estimand,what=what,population=population,
                       weights=weights,uncertainty=uncertainty,data=data,B=B,...)
    z <- if (type == "optimum") tr$special_points else tr$values
  } else if (type == "surface") {
    if (is.null(x) || is.null(y) || !all(c(x,y) %in% names(data))) stop("Surface plots require valid `x` and `y`.",call.=FALSE)
    xv <- at[[x]] %||% .gph_numeric_grid(data,x,max(20L,round(sqrt(n*n))))
    yv <- at[[y]] %||% .gph_numeric_grid(data,y,max(20L,round(sqrt(n*n))))
    grid <- expand.grid(stats::setNames(list(xv,yv),c(x,y)), KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
    grid <- .gph_restore_classes(grid,data)
    grid$estimate <- .gph_predict_estimand(object,grid,estimand,what,data=data,positive_dist_fun=positive_dist_fun)
    z <- grid
  }
  if (is.null(z) || !nrow(z)) stop("No plot data could be generated.", call.=FALSE)
  class(z) <- c("gamlss_plot_data", setdiff(class(z),"gamlss_plot_data")); attr(z,"plot_type") <- type
  z
}

#' Quantitative trends, derivatives, turning points, and optima from GAMLSS
#'
#' @description
#' Evaluates a declared estimand along a numeric predictor. The function can
#' return the fitted curve, first or second numerical derivatives, turning
#' points, or extrema locally refined around an interior grid optimum. Prediction-based derivatives remain available
#' for GAMLSS smoothers such as `pb()`.
#'
#' @param object Fitted GAMLSS model.
#' @param x Name of a numeric predictor.
#' @param by Optional grouping variables.
#' @param at_x Numeric evaluation grid. If `NULL`, an equally spaced grid is used.
#' @param n Number of grid points when `at_x` is `NULL`.
#' @param method `curve`, `derivative`, `turning_points`, or `optimum`.
#' @param derivative_order Derivative order, currently 1 or 2.
#' @param optimum For `method = "optimum"`, select `maximum` or `minimum`.
#' @param band `pointwise` or `simultaneous`. Simultaneous bands require
#'   bootstrap draws.
#' @param estimand Distributional estimand passed to [gamlss_posthoc()].
#' @param what Distributional parameter when `estimand = "parameter"`.
#' @param engine `auto`, `marginaleffects`, or `distribution`. First derivatives
#'   of ordinary distributional parameters can use `marginaleffects::avg_slopes()`.
#' @param population Target standardization population.
#' @param weights Weighting rule passed to [gamlss_posthoc()].
#' @param grid Deprecated alias for `population`.
#' @param data Original model data.
#' @param uncertainty `auto`, `delta`, `simulation`, `bootstrap`, or `none`.
#'   Delta/simulation are available when `marginaleffects` owns a first-derivative
#'   parameter trend; distribution-derived curves use refit bootstrap when these
#'   covariance-based layers are requested. Derivative bands are based on retained
#'   bootstrap curves.
#' @param bootstrap Bootstrap type.
#' @param B Number of bootstrap refits.
#' @param cluster Cluster column for cluster bootstrap.
#' @param custom_fun Optional custom estimand function.
#' @param positive_dist_fun Optional positive-distribution constructor for
#'   zero-adjusted models.
#' @param refit_fun Optional custom model-refit function.
#' @param simulate_fun Optional custom response simulator.
#' @param parallel Use parallel bootstrap through `future.apply`.
#' @param cores Number of workers; values above two are reduced to two.
#' @param seed Random seed.
#' @return Object of class `gamlss_trend` with curve/derivative values and,
#'   where requested, detected turning points or extrema.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(14)
#'   d <- data.frame(dose = rep(seq(0, 120, length.out = 10), each = 8))
#'   mu <- exp(.4 + .015*d$dose - .00007*d$dose^2)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = .22)
#'   fit <- gamlss::gamlss(y ~ dose + I(dose^2), family = gamlss.dist::GA,
#'                         data = d, trace = FALSE)
#'   # 1. Predicted response-mean curve
#'   gamlss_trend(fit, "dose", method = "curve", n = 25,
#'                uncertainty = "none", data = d)
#'   # 2. First derivative of the mean curve
#'   gamlss_trend(fit, "dose", method = "derivative", derivative_order = 1,
#'                n = 25, uncertainty = "none", data = d)
#'   # 3. Refined maximum on the supplied dose range
#'   gamlss_trend(fit, "dose", method = "optimum", optimum = "maximum",
#'                n = 25, uncertainty = "none", data = d)
#' }
gamlss_trend <- function(object, x, by = NULL, at_x = NULL, n = 100L,
                         method = c("curve", "derivative", "turning_points", "optimum"),
                         derivative_order = 1L,
                         optimum = c("maximum", "minimum"),
                         band = c("pointwise", "simultaneous"),
                         estimand = "mean", what = "mu",
                         engine = c("auto", "marginaleffects", "distribution"),
                         population = NULL, weights = NULL, grid = NULL, data = NULL,
                         uncertainty = c("auto", "delta", "simulation", "bootstrap", "none"),
                         bootstrap = "case", B = 499L, cluster = NULL,
                         custom_fun = NULL, positive_dist_fun = NULL,
                         refit_fun = NULL, simulate_fun = NULL,
                         parallel = FALSE, cores = 2L, seed = 1234) {
  method <- match.arg(method)
  engine <- match.arg(engine)
  optimum <- match.arg(optimum)
  band <- match.arg(band)
  uncertainty <- match.arg(uncertainty)
  population <- .gph_resolve_population(population, grid)
  derivative_order <- suppressWarnings(as.integer(derivative_order)[1L])
  if (!derivative_order %in% c(1L, 2L)) stop("`derivative_order` must be 1 or 2.", call. = FALSE)
  data <- .gph_get_data(object, data)
  if (!x %in% names(data) || !is.numeric(data[[x]])) stop("`x` must name a numeric column in data.", call. = FALSE)
  n <- suppressWarnings(as.integer(n)[1L])
  if (!is.finite(n) || n < 3L) stop("`n` must be an integer of at least 3.", call. = FALSE)
  if (is.null(at_x)) at_x <- seq(min(data[[x]], na.rm = TRUE), max(data[[x]], na.rm = TRUE), length.out = n)
  at_x <- sort(unique(as.numeric(at_x)))
  if (length(at_x) < 3L || any(!is.finite(at_x))) stop("`at_x` must contain at least three distinct finite values.", call. = FALSE)

  # Let marginaleffects own first-derivative parameter trends when it can do so
  # with a model covariance matrix. Higher derivatives and distribution-derived
  # targets remain prediction-based.
  me_eligible <- estimand == "parameter" && derivative_order == 1L && method == "derivative" &&
    !.gph_is_zadj(object) && what %in% .gph_model_parameters(object) &&
    uncertainty != "bootstrap" && requireNamespace("marginaleffects", quietly = TRUE)
  if (engine == "auto" && me_eligible) engine <- "marginaleffects"
  if (engine == "auto") engine <- "distribution"
  if (engine == "marginaleffects") {
    if (!me_eligible) {
      warning("marginaleffects trend routing is available only for first derivatives of an ordinary distributional parameter; using distribution engine.", call. = FALSE)
      engine <- "distribution"
    } else {
      me_out <- .gph_trend_marginaleffects(object, x, by, at_x, what, population, weights, data, uncertainty, B)
      if (!is.null(me_out)) return(me_out)
      warning("marginaleffects::avg_slopes() failed; using numerical derivatives of predictions.", call. = FALSE)
      engine <- "distribution"
    }
  }

  # For curve-derived uncertainty, bootstrap is the stable generic layer.
  unc_ph <- if (uncertainty == "auto") "none" else uncertainty
  ph <- gamlss_posthoc(
    object, specs = x, by = by, estimand = estimand, what = what,
    contrast = "none", at = stats::setNames(list(at_x), x),
    population = population, weights = weights,
    engine = "distribution", uncertainty = unc_ph,
    bootstrap = bootstrap, B = B, cluster = cluster,
    custom_fun = custom_fun, positive_dist_fun = positive_dist_fun,
    data = data, refit_fun = refit_fun, simulate_fun = simulate_fun,
    parallel = parallel, cores = cores, seed = seed,
    keep_draws = unc_ph == "bootstrap"
  )
  d <- ph$estimates
  D <- ph$draws
  split_key <- if (length(by)) interaction(d[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(d)))
  idx_list <- split(seq_len(nrow(d)), split_key)

  if (!is.null(D) && band == "simultaneous") {
    d$simultaneous_lower <- NA_real_
    d$simultaneous_upper <- NA_real_
    for (ii in idx_list) {
      se <- apply(D[, ii, drop = FALSE], 2, stats::sd, na.rm = TRUE)
      good <- is.finite(se) & se > 0
      if (!any(good)) next
      Z <- sweep(D[, ii[good], drop = FALSE], 2, d$estimate[ii[good]], "-")
      Z <- sweep(Z, 2, se[good], "/")
      crit <- stats::quantile(apply(abs(Z), 1, max, na.rm = TRUE), 0.95, na.rm = TRUE)
      d$simultaneous_lower[ii[good]] <- d$estimate[ii[good]] - crit * se[good]
      d$simultaneous_upper[ii[good]] <- d$estimate[ii[good]] + crit * se[good]
    }
  }

  if (method == "curve") return(.gph_trend_object(d, method, x, by, estimand, ph, special = NULL))

  first <- .gph_derivative_table(d, x, by, order = 1L)
  derivD1 <- if (!is.null(D)) .gph_derivative_draws(D, d, x, by, order = 1L) else NULL

  if (method == "derivative") {
    ord <- derivative_order
    dd <- if (ord == 1L) first else .gph_derivative_table(d, x, by, order = 2L)
    derivD <- if (is.null(D)) NULL else .gph_derivative_draws(D, d, x, by, order = ord)
    if (!is.null(derivD)) {
      nm <- paste0("derivative", ord)
      dd[[paste0(nm, "_SE")]] <- apply(derivD, 2, stats::sd, na.rm = TRUE)
      dd[[paste0(nm, "_lower")]] <- apply(derivD, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
      dd[[paste0(nm, "_upper")]] <- apply(derivD, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
      if (band == "simultaneous") {
        dd[[paste0(nm, "_simultaneous_lower")]] <- NA_real_
        dd[[paste0(nm, "_simultaneous_upper")]] <- NA_real_
        for (ii in idx_list) {
          se <- apply(derivD[, ii, drop = FALSE], 2, stats::sd, na.rm = TRUE)
          good <- is.finite(se) & se > 0
          if (!any(good)) next
          center <- dd[[nm]][ii[good]]
          Z <- sweep(derivD[, ii[good], drop = FALSE], 2, center, "-")
          Z <- sweep(Z, 2, se[good], "/")
          crit <- stats::quantile(apply(abs(Z), 1, max, na.rm = TRUE), .95, na.rm = TRUE)
          dd[[paste0(nm, "_simultaneous_lower")]][ii[good]] <- center - crit * se[good]
          dd[[paste0(nm, "_simultaneous_upper")]][ii[good]] <- center + crit * se[good]
        }
      }
    }
    return(.gph_trend_object(dd, paste0("derivative", ord), x, by, estimand, ph, special = NULL))
  }

  if (method == "turning_points") {
    tp <- .gph_turning_points(d, first, x, by)
    return(.gph_trend_object(first, method, x, by, estimand, ph, special = tp))
  }

  op <- .gph_optima(d, x, by, optimum)
  if (!is.null(D)) {
    idx_by <- idx_list
    draws_op <- lapply(idx_by, function(ii) {
      xv <- d[[x]][ii]
      apply(D[, ii, drop = FALSE], 1, function(yv) {
        .gph_refine_extremum(xv, yv, optimum)$x
      })
    })
    for (j in seq_len(nrow(op))) {
      z <- draws_op[[j]]
      op$x_SE[j] <- stats::sd(z, na.rm = TRUE)
      op$x_lower[j] <- stats::quantile(z, 0.025, na.rm = TRUE)
      op$x_upper[j] <- stats::quantile(z, 0.975, na.rm = TRUE)
    }
    names(draws_op) <- if (length(by)) vapply(seq_len(nrow(op)), function(j) {
      vals <- vapply(by, function(b) as.character(op[[b]][j]), character(1))
      paste(paste(by, vals, sep = "="), collapse = ", ")
    }, character(1)) else "All"
    attr(op, "optimum_draws") <- draws_op
  }
  .gph_trend_object(d, method, x, by, estimand, ph, special = op)
}

.gph_trend_marginaleffects <- function(object, x, by, at_x, what, population, weights, data, uncertainty, B) {
  eval_info <- .gph_build_eval_data(object, data, specs = x, by = by,
                                     at = stats::setNames(list(at_x), x),
                                     population = population, weights = weights)
  z <- try(marginaleffects::avg_slopes(
    object, variables = x, newdata = eval_info$eval_data, by = ".gph_group_id",
    wts = eval_info$eval_data$.gph_weight, type = "response", what = what,
    vcov = if (uncertainty == "none") FALSE else TRUE
  ), silent = TRUE)
  if (inherits(z, "try-error")) return(NULL)
  if (uncertainty == "auto") uncertainty <- "delta"
  if (uncertainty == "simulation") {
    zz <- try(marginaleffects::inferences(z, method = "simulation", R = as.integer(B)), silent = TRUE)
    if (!inherits(zz, "try-error")) z <- zz
  }
  mt <- as.data.frame(z)
  if (!".gph_group_id" %in% names(mt) || !"estimate" %in% names(mt)) return(NULL)
  out <- merge(eval_info$groups, mt, by = ".gph_group_id", all.x = TRUE, sort = FALSE)
  out <- out[match(eval_info$groups$.gph_group_id, out$.gph_group_id), , drop = FALSE]
  rownames(out) <- NULL
  out$derivative1 <- out$estimate
  out$estimate <- NULL
  if ("conf.low" %in% names(out)) names(out)[names(out) == "conf.low"] <- "derivative1_lower"
  if ("conf.high" %in% names(out)) names(out)[names(out) == "conf.high"] <- "derivative1_upper"
  if ("std.error" %in% names(out)) names(out)[names(out) == "std.error"] <- "derivative1_SE"
  out <- .gph_relabel_groups(out)
  source <- list(engine = "marginaleffects", estimand = "parameter", what = what,
                 population = population, weighting = eval_info$weighting,
                 uncertainty_method = uncertainty)
  .gph_trend_object(out, "derivative1", x, by, paste0("parameter:", what), source, special = NULL)
}

.gph_gradient <- function(x, y) {
  n <- length(x); out <- rep(NA_real_, n)
  out[1] <- (y[2] - y[1]) / (x[2] - x[1])
  out[n] <- (y[n] - y[n - 1L]) / (x[n] - x[n - 1L])
  if (n >= 3L) out[2:(n - 1L)] <- (y[3:n] - y[1:(n - 2L)]) / (x[3:n] - x[1:(n - 2L)])
  out
}

.gph_numeric_derivative <- function(x, y, order = 1L) {
  out <- y
  for (i in seq_len(order)) out <- .gph_gradient(x, out)
  out
}

.gph_derivative_table <- function(d, x, by, order = 1L) {
  key <- if (length(by)) interaction(d[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(d)))
  chunks <- lapply(split(seq_len(nrow(d)), key), function(ii) {
    z <- d[ii, , drop = FALSE]
    o <- order(z[[x]]); z <- z[o, , drop = FALSE]
    z[[paste0("derivative", order)]] <- .gph_numeric_derivative(z[[x]], z$estimate, order)
    z
  })
  out <- do.call(rbind, chunks); rownames(out) <- NULL; out
}

.gph_derivative_draws <- function(D, d, x, by, order = 1L) {
  key <- if (length(by)) interaction(d[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(d)))
  out <- matrix(NA_real_, nrow(D), nrow(d))
  for (ii in split(seq_len(nrow(d)), key)) {
    ord <- ii[order(d[[x]][ii])]
    xv <- d[[x]][ord]
    for (r in seq_len(nrow(D))) out[r, ord] <- .gph_numeric_derivative(xv, D[r, ord], order)
  }
  out
}

.gph_turning_points <- function(curve, deriv, x, by) {
  key <- if (length(by)) interaction(curve[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(curve)))
  rows <- list()
  for (ii in split(seq_len(nrow(curve)), key)) {
    z <- curve[ii, , drop = FALSE]; dz <- deriv[ii, , drop = FALSE]
    o <- order(z[[x]]); z <- z[o, , drop = FALSE]; dz <- dz[o, , drop = FALSE]
    dv <- dz$derivative1
    cand <- which(is.finite(dv[-length(dv)]) & is.finite(dv[-1L]) & dv[-length(dv)] * dv[-1L] <= 0)
    for (j in cand) {
      x1 <- z[[x]][j]; x2 <- z[[x]][j + 1L]; d1 <- dv[j]; d2 <- dv[j + 1L]
      x0 <- if (isTRUE(all.equal(d1, d2))) (x1 + x2) / 2 else x1 - d1 * (x2 - x1) / (d2 - d1)
      y0 <- stats::approx(z[[x]], z$estimate, xout = x0, rule = 2)$y
      rr <- if (length(by)) z[1, by, drop = FALSE] else data.frame(.dummy = 1)[, FALSE, drop = FALSE]
      rr[[x]] <- x0; rr$estimate <- y0
      rr$type <- if (d1 > 0 && d2 < 0) "maximum" else if (d1 < 0 && d2 > 0) "minimum" else "stationary"
      rows[[length(rows) + 1L]] <- rr
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

.gph_refine_extremum <- function(x, y, optimum = c("maximum", "minimum")) {
  optimum <- match.arg(optimum)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (!length(y)) return(list(x = NA_real_, y = NA_real_, refined = FALSE))
  o <- order(x); x <- x[o]; y <- y[o]
  j <- if (optimum == "maximum") which.max(y) else which.min(y)
  ans <- list(x = unname(x[j]), y = unname(y[j]), refined = FALSE)
  if (length(y) < 3L || j == 1L || j == length(y)) return(ans)
  ii <- (j - 1L):(j + 1L)
  X <- cbind(1, x[ii], x[ii]^2)
  cf <- try(stats::lm.fit(X, y[ii])$coefficients, silent = TRUE)
  if (inherits(cf, "try-error") || length(cf) < 3L || any(!is.finite(cf)) || abs(cf[3]) < sqrt(.Machine$double.eps)) return(ans)
  curvature_ok <- if (optimum == "maximum") cf[3] < 0 else cf[3] > 0
  if (!curvature_ok) return(ans)
  xv <- -cf[2] / (2 * cf[3])
  if (!is.finite(xv) || xv < min(x[ii]) || xv > max(x[ii])) return(ans)
  yv <- cf[1] + cf[2] * xv + cf[3] * xv^2
  if (!is.finite(yv)) return(ans)
  list(x = unname(xv), y = unname(yv), refined = TRUE)
}

.gph_optima <- function(d, x, by, optimum) {
  key <- if (length(by)) interaction(d[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(d)))
  rows <- lapply(split(seq_len(nrow(d)), key), function(ii) {
    z <- d[ii, , drop = FALSE]
    ex <- .gph_refine_extremum(z[[x]], z$estimate, optimum)
    rr <- if (length(by)) z[1, by, drop = FALSE] else data.frame(.dummy = 1)[, FALSE, drop = FALSE]
    rr[[x]] <- ex$x; rr$estimate <- ex$y; rr$type <- optimum; rr$refined <- ex$refined
    rr
  })
  out <- do.call(rbind, rows); rownames(out) <- NULL; out
}

.gph_trend_object <- function(values, method, x, by, estimand, source, special = NULL) {
  out <- list(values = values, method = method, x = x, by = by, estimand = estimand,
              special_points = special, source = source)
  class(out) <- "gamlss_trend"
  out
}

#' @method print gamlss_trend
#' @export
#' @noRd
print.gamlss_trend <- function(x, ...) {
  cat("gamlssPosthoc quantitative trend\n")
  cat("  method:   ", x$method, "\n", sep = "")
  cat("  predictor:", x$x, "\n\n", sep = "")
  print(x$values, row.names = FALSE)
  if (!is.null(x$special_points)) {
    cat("\nSpecial points\n")
    print(x$special_points, row.names = FALSE)
  }
  invisible(x)
}

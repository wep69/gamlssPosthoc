#' Reliable marginal and distributional post-hoc inference for GAMLSS models
#'
#' @description
#' `gamlss_posthoc()` separates four decisions that are often conflated in
#' post-hoc work: the statistical estimand, the target population used for
#' standardization, the contrast geometry, and the inferential engine. Simple
#' parameter-wise requests can use `emmeans` or `marginaleffects`; quantities
#' involving the full distribution, zero adjustment, or multiple parameters are
#' evaluated by the distribution engine and can use refit bootstrap uncertainty.
#'
#' @param object A fitted `gamlss` or `gamlssZadj` model.
#' @param specs Character vector of focal variables.
#' @param by Optional character vector of conditioning variables.
#' @param estimand One of `parameter`, `mean`, `variance`, `quantile`,
#'   `prob_zero`, or `custom`.
#' @param what Distributional parameter for `estimand = "parameter"`. Parameter
#'   availability is discovered from the fitted model rather than hard-coded.
#' @param contrast Which levels are compared: `none`, `pairwise`, `reference`,
#'   `sequential`, `poly`, or `opoly`.
#' @param comparison Scientific scale of ordinary level contrasts: `difference`,
#'   `ratio`, `log_ratio`, or `percent_change`. Polynomial contrasts are linear
#'   combinations and therefore use `difference`.
#' @param adjust Multiplicity adjustment.
#' @param at Named list of evaluation values.
#' @param population Target population for standardization: `observed`,
#'   `balanced`, or `reference`.
#' @param weights Weighting rule: `proportional`, `equal`, or a numeric vector
#'   with one non-negative weight per original data row.
#' @param grid Deprecated compatibility alias for `population`.
#' @param engine `auto`, `emmeans`, `marginaleffects`, or `distribution`.
#' @param uncertainty `auto`, `delta`, `simulation`, `bootstrap`, or `none`.
#'   `auto` uses delta-method inference for supported parameter-wise engines.
#'   For generic distribution-derived estimands it returns point estimates only
#'   and the diagnostic plan recommends explicit refit bootstrap when interval
#'   inference is required.
#' @param bootstrap Bootstrap type: `parametric`, `case`, or `cluster`.
#' @param B Number of simulation/bootstrap draws or refits.
#' @param cluster Cluster column for cluster bootstrap.
#' @param ref Reference level index for reference contrasts.
#' @param scores Numeric scores for `opoly` contrasts.
#' @param degree Maximum polynomial contrast degree.
#' @param prob Probability for `estimand = "quantile"`.
#' @param custom_fun Function of `pars`, `newdata`, and `object` returning one
#'   derived estimand per row.
#' @param positive_dist_fun Optional constructor for the positive-part
#'   `distributions3` object in a `gamlssZadj` model whose family cannot be
#'   reconstructed by `gamlss.dist::GAMLSS()`.
#' @param positive_mean_fun Deprecated compatibility hook. Prefer
#'   `positive_dist_fun`, which enables means, variances, quantiles, and random
#'   generation consistently.
#' @param data Original model data. Strongly recommended.
#' @param refit_fun Optional custom refit function `function(object, boot_data)`.
#' @param simulate_fun Optional custom response simulator for parametric bootstrap.
#' @param parallel Use `future.apply` for bootstrap refits if available.
#' @param cores Number of workers. Values above two are reduced to two.
#' @param seed Random seed.
#' @param keep_draws Keep bootstrap draws in the returned object.
#' @param ... Additional arguments passed to compatible external engines.
#'
#' @return An object of class `gamlss_posthoc` containing estimates, contrasts,
#'   a formal estimand description, engine diagnostics, and optional draws.
#' @export
#'
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(11)
#'   d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = 20)))
#'   d$y <- gamlss.dist::rGA(nrow(d),
#'     mu = c(A = 2, B = 2.5, C = 3.0)[d$trt], sigma = 0.25)
#'   fit <- gamlss::gamlss(y ~ trt, sigma.formula = ~ trt, data = d,
#'                         family = gamlss.dist::GA, trace = FALSE)
#'   # 1. Difference between marginal response means
#'   gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise",
#'                  comparison = "difference", uncertainty = "none", data = d)
#'   # 2. Ratios relative to the first treatment
#'   gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "reference",
#'                  comparison = "ratio", uncertainty = "none", data = d)
#'   # 3. Compare the distributional sigma parameter on a reference grid
#'   gamlss_posthoc(fit, "trt", estimand = "parameter", what = "sigma",
#'                  population = "reference", contrast = "none",
#'                  uncertainty = "none", data = d)
#' }
gamlss_posthoc <- function(object, specs, by = NULL,
                            estimand = c("parameter", "mean", "variance", "quantile", "prob_zero", "custom"),
                            what = "mu",
                            contrast = c("none", "pairwise", "reference", "sequential", "poly", "opoly"),
                            comparison = c("difference", "ratio", "log_ratio", "percent_change"),
                            adjust = "tukey",
                            at = list(),
                            population = NULL,
                            weights = NULL,
                            grid = NULL,
                            engine = c("auto", "emmeans", "marginaleffects", "distribution"),
                            uncertainty = c("auto", "delta", "simulation", "bootstrap", "none"),
                            bootstrap = c("parametric", "case", "cluster"),
                            B = 499L,
                            cluster = NULL,
                            ref = 1L,
                            scores = NULL,
                            degree = NULL,
                            prob = 0.5,
                            custom_fun = NULL,
                            positive_dist_fun = NULL,
                            positive_mean_fun = NULL,
                            data = NULL,
                            refit_fun = NULL,
                            simulate_fun = NULL,
                            parallel = FALSE,
                            cores = 2L,
                            seed = 1234,
                            keep_draws = FALSE,
                            ...) {
  if (!.gph_is_gamlss(object)) stop("`object` must inherit from 'gamlss' or 'gamlssZadj'.", call. = FALSE)
  estimand <- match.arg(estimand)
  contrast <- match.arg(contrast)
  comparison <- match.arg(comparison)
  engine <- match.arg(engine)
  engine_requested <- engine
  uncertainty <- match.arg(uncertainty)
  uncertainty_requested <- uncertainty
  bootstrap <- match.arg(bootstrap)
  population <- .gph_resolve_population(population, grid)
  specs <- as.character(specs)
  if (!length(specs) || any(!nzchar(specs))) stop("`specs` must contain at least one variable name.", call. = FALSE)
  by <- if (is.null(by)) NULL else as.character(by)
  data <- .gph_get_data(object, data)
  weights <- .gph_resolve_weights(weights, data, population)

  if (!is.null(positive_mean_fun)) {
    warning("`positive_mean_fun` is deprecated. Use `positive_dist_fun` so mean, variance, quantiles, and simulation share one coherent positive distribution.", call. = FALSE)
  }

  if (estimand == "parameter" && !what %in% .gph_model_parameters(object)) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(.gph_model_parameters(object), collapse = ", "), ".", call. = FALSE)
  }
  if (contrast %in% c("poly", "opoly") && comparison != "difference") {
    stop("Polynomial contrasts require `comparison='difference'`.", call. = FALSE)
  }

  routed <- .gph_choose_engine(object, estimand, what, contrast, comparison, engine, population, weights)
  engine <- routed$engine
  if (engine == "emmeans" && !routed$eligibility$emmeans) {
    stop("The emmeans engine is not eligible for this request. Use `gamlss_posthoc_plan()` for details.", call. = FALSE)
  }
  if (engine == "marginaleffects" && !routed$eligibility$marginaleffects) {
    stop("The marginaleffects engine is currently reserved for parameter-wise ordinary GAMLSS requests. ",
         "Use engine='distribution' for zero-adjusted or distribution-derived estimands.", call. = FALSE)
  }
  uncertainty <- .gph_choose_uncertainty(engine, uncertainty, estimand, routed$eligibility$smoother)
  if (engine == "emmeans" && !uncertainty %in% c("delta", "none")) {
    warning("emmeans does not provide the requested uncertainty layer here; using delta-method inference.", call. = FALSE)
    uncertainty <- "delta"
  }
  if (engine == "distribution" && uncertainty %in% c("delta", "simulation")) {
    warning("Generic distribution-derived estimands do not have a reliable joint coefficient covariance interface in current gamlss. Using refit bootstrap.", call. = FALSE)
    uncertainty <- "bootstrap"
  }
  if (engine == "marginaleffects" && uncertainty == "bootstrap") {
    # Use our refit bootstrap so the behavior is stable across marginaleffects versions.
    engine <- "distribution"
  }

  if (engine == "marginaleffects") {
    ans <- (.gph_posthoc_marginaleffects(
      object = object, specs = specs, by = by, what = what,
      contrast = contrast, comparison = comparison, at = at,
      population = population, weights = weights, uncertainty = uncertainty,
      B = B, ref = ref, scores = scores, degree = degree, adjust = adjust, data = data, ...
    ))
    if (!is.null(ans)) {
      ans$estimand <- estimand
      ans$estimand_info <- .gph_estimand_info(object, estimand, what, prob, population, ans$weighting)
      ans$call <- match.call()
      ans$model <- object
      ans$data <- data
      class(ans) <- "gamlss_posthoc"
      return(ans)
    }
    if (identical(engine_requested, "marginaleffects")) {
      stop("marginaleffects could not complete this explicitly requested operation for the fitted object. ",
           "Use `gamlss_posthoc_plan()` and try `engine='distribution'` if appropriate.", call. = FALSE)
    }
    warning("marginaleffects could not complete the automatically routed request; falling back to the distribution engine.", call. = FALSE)
    engine <- "distribution"
    if (identical(uncertainty_requested, "auto")) {
      uncertainty <- "none"
    } else if (uncertainty %in% c("delta", "simulation")) {
      warning("The requested covariance-based uncertainty is unavailable after fallback; using refit bootstrap.", call. = FALSE)
      uncertainty <- "bootstrap"
    }
  }

  if (engine == "emmeans") {
    .gph_require("emmeans", "for the emmeans engine.")
    spec_formula <- if (length(by)) {
      stats::as.formula(paste("~", paste(specs, collapse = " * "), "|", paste(by, collapse = " * ")))
    } else stats::as.formula(paste("~", paste(specs, collapse = " * ")))
    emm_weights <- if (is.character(weights)) weights else NULL
    emm_obj <- .gph_self_contained(object, data)
    emm_vcov <- .gph_param_vcov(object, what)
    emm_args <- c(list(emm_obj, specs = spec_formula, what = what, data = data,
                       at = at, weights = emm_weights),
                  if (!is.null(emm_vcov)) list(vcov. = emm_vcov), list(...))
    emm <- do.call(emmeans::emmeans, emm_args)
    emm_resp <- try(emmeans::regrid(emm, transform = "response"), silent = TRUE)
    if (inherits(emm_resp, "try-error")) emm_resp <- emm
    est <- as.data.frame(summary(emm_resp, infer = c(uncertainty != "none", uncertainty != "none")))
    names(est)[names(est) %in% c("emmean", "response", what)] <- "estimate"
    if (!"estimate" %in% names(est)) {
      candidate <- setdiff(names(est), c(specs, by, "SE", "df", "lower.CL", "upper.CL", "asymp.LCL", "asymp.UCL"))
      if (length(candidate)) names(est)[match(candidate[1], names(est))] <- "estimate"
    }
    con <- NULL
    if (contrast != "none") {
      if (contrast %in% c("pairwise", "reference", "sequential")) {
        method <- switch(contrast, pairwise = "pairwise", reference = "trt.vs.ctrl", sequential = "consec")
        args <- list(object = emm_resp, method = method, adjust = adjust)
        if (contrast == "reference") args$ref <- ref
        con <- as.data.frame(summary(do.call(emmeans::contrast, args), infer = c(TRUE, TRUE)))
        con$comparison <- "difference"
      } else {
        args <- list(object = emm_resp, method = if (contrast == "poly") "poly" else "opoly", adjust = adjust)
        if (contrast == "opoly") {
          if (is.null(scores)) stop("Supply `scores` for contrast='opoly'.", call. = FALSE)
          args$scores <- scores
        }
        if (!is.null(degree)) args$max.degree <- degree
        z <- try(do.call(emmeans::contrast, args), silent = TRUE)
        if (!inherits(z, "try-error")) con <- as.data.frame(summary(z, infer = c(TRUE, TRUE)))
        if (!is.null(con)) con$comparison <- "difference"
      }
    }
    emm_weighting <- if (is.null(emm_weights)) "emmeans default reference-grid weights" else paste0("emmeans ", emm_weights, " reference-grid weights")
    info <- .gph_estimand_info(object, estimand, what, prob, "reference", emm_weighting)
    out <- list(estimates = est, contrasts = con, engine = "emmeans", estimand = estimand,
                estimand_info = info, what = what, specs = specs, by = by,
                contrast_method = contrast, comparison = comparison,
                population = "reference", weighting = emm_weighting,
                uncertainty_method = uncertainty, call = match.call(), model = object, data = data, emmGrid = emm_resp,
                notes = "Parameter-wise inference from emmeans; arithmetic response-scale contrasts use regrid(transform='response').")
    class(out) <- "gamlss_posthoc"
    return(out)
  }

  # Distribution/prediction engine -------------------------------------------
  eval_info <- .gph_build_eval_data(object, data, specs, by, at, population, weights)
  values <- .gph_predict_estimand(object, eval_info$eval_data, estimand, what, prob,
                                  custom_fun, data, positive_dist_fun, positive_mean_fun)
  est <- .gph_aggregate(values, eval_info)
  draw_mat <- NULL

  if (uncertainty == "bootstrap") {
    B <- suppressWarnings(as.integer(B)[1L])
    if (!is.finite(B) || B < 2L) stop("`B` must be an integer of at least 2 for bootstrap uncertainty.", call. = FALSE)
    if (B < 20L) warning("B < 20 is only suitable for debugging, not inference.", call. = FALSE)
    set.seed(seed)
    response <- try(.gph_response_name(object, data), silent = TRUE)
    one_boot <- function(b) {
      tryCatch({
        if (bootstrap == "parametric") {
          bd <- data
          if (inherits(response, "try-error")) stop("Response name unavailable; supply refit_fun.")
          bd[[response]] <- .gph_simulate_response(object, data, simulate_fun, positive_dist_fun)
        } else bd <- .gph_boot_data(data, bootstrap, cluster)
        fitb <- .gph_refit(object, bd, data, refit_fun)
        vb <- .gph_predict_estimand(fitb, eval_info$eval_data, estimand, what, prob,
                                    custom_fun, bd, positive_dist_fun, positive_mean_fun)
        .gph_aggregate(vb, eval_info)$estimate
      }, error = function(e) rep(NA_real_, nrow(eval_info$groups)))
    }
    boot_list <- .gph_parallel_lapply(seq_len(B), one_boot, parallel, cores, seed)
    draw_mat <- do.call(rbind, boot_list)
    ok <- rowSums(is.finite(draw_mat)) == ncol(draw_mat)
    if (!any(ok)) stop("All bootstrap refits failed; inspect model stability and `refit_fun`.", call. = FALSE)
    fail_rate <- 1 - mean(ok)
    if (fail_rate > 0.10) warning(sprintf("Bootstrap failure rate is %.1f%%. Inspect model stability.", 100 * fail_rate), call. = FALSE)
    draw_mat <- draw_mat[ok, , drop = FALSE]
    est$SE <- apply(draw_mat, 2, stats::sd, na.rm = TRUE)
    est$lower.CL <- apply(draw_mat, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
    est$upper.CL <- apply(draw_mat, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
    est$boot_success <- nrow(draw_mat)
    est$boot_B <- B
  }

  con <- NULL
  if (contrast != "none") {
    con <- .gph_make_contrasts(est, specs, by, contrast, ref, scores, degree, draw_mat, comparison)
    if (!is.null(con) && "p.value" %in% names(con)) {
      adj_method <- if (adjust %in% stats::p.adjust.methods) adjust else "holm"
      if (!adjust %in% stats::p.adjust.methods) warning("Bootstrap contrasts cannot use '", adjust, "' through p.adjust(); using Holm.", call. = FALSE)
      if (length(by)) {
        key <- interaction(con[by], drop = TRUE, lex.order = TRUE)
        con$p.value.adjusted <- ave(con$p.value, key, FUN = function(z) stats::p.adjust(z, method = adj_method))
      } else con$p.value.adjusted <- stats::p.adjust(con$p.value, method = adj_method)
    }
  }

  est <- .gph_relabel_groups(est)
  info <- .gph_estimand_info(object, estimand, what, prob, population, eval_info$weighting)
  out <- list(estimates = est, contrasts = con, engine = "distribution", estimand = estimand,
              estimand_info = info, what = what, specs = specs, by = by,
              contrast_method = contrast, comparison = comparison,
              population = population, weighting = eval_info$weighting,
              uncertainty_method = uncertainty, call = match.call(),
              model = object, data = data,
              bootstrap = if (uncertainty == "bootstrap") bootstrap else NULL,
              draws = if (keep_draws) draw_mat else NULL,
              notes = c("Predictions were standardized over the declared target population.",
                        if (population == "observed") "Observed-population estimates are g-computation averages over the empirical covariate distribution." else NULL,
                        if (uncertainty == "none") "No coefficient-estimation uncertainty is attached to these point estimates." else NULL))
  class(out) <- "gamlss_posthoc"
  out
}

.gph_posthoc_marginaleffects <- function(object, specs, by, what, contrast, comparison,
                                         at, population, weights, uncertainty, B,
                                         ref, scores, degree, adjust, data, ...) {
  .gph_require("marginaleffects", "for the marginaleffects engine.")
  eval_info <- .gph_build_eval_data(object, data, specs, by, at, population, weights)
  me_vcov <- if (uncertainty == "none") FALSE else TRUE
  object <- .gph_self_contained(object, data)
  # Some gamlss fits cannot expose a joint covariance matrix to marginaleffects
  # (`vcov.gamlss()` resolves the fitted data by name).  Retry without standard
  # errors rather than losing the point estimates.
  me <- try(marginaleffects::avg_predictions(
    object, newdata = eval_info$eval_data, by = ".gph_group_id",
    type = "response", what = what, wts = eval_info$eval_data$.gph_weight,
    vcov = me_vcov, ...
  ), silent = TRUE)
  if (inherits(me, "try-error") && !identical(me_vcov, FALSE)) {
    me_vcov <- FALSE
    uncertainty <- "none"
    me <- try(marginaleffects::avg_predictions(
      object, newdata = eval_info$eval_data, by = ".gph_group_id",
      type = "response", what = what, wts = eval_info$eval_data$.gph_weight,
      vcov = FALSE, ...
    ), silent = TRUE)
  }
  if (inherits(me, "try-error")) return(NULL)
  if (uncertainty == "simulation") {
    sim <- try(marginaleffects::inferences(me, method = "simulation", R = as.integer(B)), silent = TRUE)
    if (!inherits(sim, "try-error")) me <- sim
  }
  mt <- as.data.frame(me)
  gid_col <- if (".gph_group_id" %in% names(mt)) ".gph_group_id" else NULL
  est_col <- if ("estimate" %in% names(mt)) "estimate" else if ("Estimate" %in% names(mt)) "Estimate" else NULL
  if (is.null(gid_col) || is.null(est_col)) return(NULL)
  est <- merge(eval_info$groups, mt, by = ".gph_group_id", all.x = TRUE, sort = FALSE)
  est <- est[match(eval_info$groups$.gph_group_id, est$.gph_group_id), , drop = FALSE]
  rownames(est) <- NULL
  names(est)[names(est) == est_col] <- "estimate"
  if ("conf.low" %in% names(est)) names(est)[names(est) == "conf.low"] <- "lower.CL"
  if ("conf.high" %in% names(est)) names(est)[names(est) == "conf.high"] <- "upper.CL"
  if ("std.error" %in% names(est)) names(est)[names(est) == "std.error"] <- "SE"

  con <- NULL
  vars <- .gph_me_variables(specs, contrast, ref)
  weighted_nonlinear <- is.numeric(weights) && comparison %in% c("ratio", "log_ratio", "percent_change")
  if (!is.null(vars) && population == "observed" && !length(at) && !weighted_nonlinear) {
    cmp_args <- c(list(
      model = object, variables = vars, newdata = data,
      type = "response", what = what, comparison = .gph_me_comparison(comparison),
      wts = if (is.numeric(weights)) weights else FALSE,
      vcov = me_vcov
    ), if (length(by)) list(by = by) else list(), list(...))
    cmp <- try(do.call(marginaleffects::avg_comparisons, cmp_args), silent = TRUE)
    if (inherits(cmp, "try-error") && !identical(me_vcov, FALSE)) {
      cmp_args$vcov <- FALSE
      cmp <- try(do.call(marginaleffects::avg_comparisons, cmp_args), silent = TRUE)
    }
    if (!inherits(cmp, "try-error")) {
      if (uncertainty == "simulation") {
        simc <- try(marginaleffects::inferences(cmp, method = "simulation", R = as.integer(B)), silent = TRUE)
        if (!inherits(simc, "try-error")) cmp <- simc
      }
      con <- as.data.frame(cmp)
      if ("conf.low" %in% names(con)) names(con)[names(con) == "conf.low"] <- "lower.CL"
      if ("conf.high" %in% names(con)) names(con)[names(con) == "conf.high"] <- "upper.CL"
      if ("std.error" %in% names(con)) names(con)[names(con) == "std.error"] <- "SE"
      if (identical(comparison, "percent_change")) {
        if ("estimate" %in% names(con)) con$estimate <- 100 * (con$estimate - 1)
        if ("SE" %in% names(con)) con$SE <- 100 * con$SE
        if ("lower.CL" %in% names(con)) con$lower.CL <- 100 * (con$lower.CL - 1)
        if ("upper.CL" %in% names(con)) con$upper.CL <- 100 * (con$upper.CL - 1)
      }
      con$comparison <- comparison
      if ("p.value" %in% names(con)) {
        adj_method <- if (adjust %in% stats::p.adjust.methods) adjust else "holm"
        if (!adjust %in% stats::p.adjust.methods) {
          warning("marginaleffects contrasts cannot apply '", adjust,
                  "' through p.adjust(); using Holm for multiplicity control.", call. = FALSE)
        }
        if (length(by) && all(by %in% names(con))) {
          key <- interaction(con[by], drop = TRUE, lex.order = TRUE)
          con$p.value.adjusted <- ave(con$p.value, key,
                                      FUN = function(z) stats::p.adjust(z, method = adj_method))
        } else {
          con$p.value.adjusted <- stats::p.adjust(con$p.value, method = adj_method)
        }
      }
    }
  }
  if (is.null(con) && contrast != "none") {
    if (uncertainty != "none") return(NULL)
    con <- .gph_make_contrasts(est, specs, by, contrast, ref, scores, degree, draws = NULL, comparison = comparison)
  }
  est <- .gph_relabel_groups(est)
  list(estimates = est, contrasts = con, engine = "marginaleffects",
       what = what, specs = specs, by = by, contrast_method = contrast,
       comparison = comparison, population = population, weighting = eval_info$weighting,
       uncertainty_method = uncertainty,
       notes = "Parameter-wise standardization used marginaleffects; direct avg_comparisons() is used when the requested contrast geometry is supported.")
}

#' @method print gamlss_posthoc
#' @export
#' @noRd
print.gamlss_posthoc <- function(x, ...) {
  cat("gamlssPosthoc result\n")
  cat("  engine:      ", x$engine, "\n", sep = "")
  cat("  estimand:    ", x$estimand_info$target[1], "\n", sep = "")
  cat("  definition:  ", x$estimand_info$definition[1], "\n", sep = "")
  cat("  population:  ", x$population, "\n", sep = "")
  cat("  weighting:   ", x$weighting, "\n", sep = "")
  cat("  uncertainty: ", x$uncertainty_method, "\n", sep = "")
  if (!identical(x$contrast_method, "none")) cat("  comparison:  ", x$comparison, "\n", sep = "")
  cat("\n")
  print(x$estimates, row.names = FALSE)
  if (!is.null(x$contrasts)) {
    cat("\nContrasts\n")
    print(x$contrasts, row.names = FALSE)
  }
  invisible(x)
}

# gamlssPosthoc 0.3.0 standalone source
# Generated from package R/ files. Package installation is preferred.


# ===== utils.R =====

`%||%` <- function(x, y) if (is.null(x)) y else x

.gph_require <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- sprintf("Package '%s' is required", pkg)
    if (!is.null(why)) msg <- paste0(msg, " ", why)
    stop(msg, ". Install it with install.packages('", pkg, "').", call. = FALSE)
  }
  invisible(TRUE)
}

.gph_get_data <- function(object, data = NULL) {
  if (!is.null(data)) return(as.data.frame(data))
  cl <- object$call
  if (!is.null(cl$data)) {
    ff <- try(.gph_get_formula(object, "mu"), silent = TRUE)
    fenv <- if (!inherits(ff, "try-error") && !is.null(ff)) environment(ff) else NULL
    envs <- Filter(Negate(is.null), list(parent.frame(), fenv))
    for (ee in envs) {
      ans <- try(eval(cl$data, envir = ee), silent = TRUE)
      if (!inherits(ans, "try-error") && is.data.frame(ans)) return(ans)
    }
  }
  stop("Could not recover the original data. Supply `data =` explicitly. ",
       "For reproducible GAMLSS post-hoc analyses this is strongly recommended.",
       call. = FALSE)
}

.gph_response_name <- function(object, data) {
  if (.gph_is_zadj(object)) {
    ycall <- object$call$y
    if (is.name(ycall) && as.character(ycall) %in% names(data)) return(as.character(ycall))
    if (!is.null(object$y)) {
      hits <- names(data)[vapply(data, function(z) {
        is.numeric(z) && length(z) == length(object$y) &&
          isTRUE(all.equal(as.numeric(z), as.numeric(object$y), check.attributes = FALSE))
      }, logical(1))]
      if (length(hits)) return(hits[1])
    }
  } else {
    f <- .gph_get_formula(object, "mu")
    if (!is.null(f)) {
      lhs <- all.vars(f[[2]])
      if (length(lhs) == 1L && lhs %in% names(data)) return(lhs)
    }
  }
  stop("Could not identify the response column. Supply a custom `refit_fun` for bootstrap.", call. = FALSE)
}

.gph_levels_or_values <- function(x, at = NULL, max_numeric = 30L) {
  if (!is.null(at)) return(at)
  if (is.factor(x)) return(levels(x))
  if (is.character(x)) return(unique(x))
  if (is.logical(x)) return(c(FALSE, TRUE))
  ux <- sort(unique(x[is.finite(x)]))
  if (length(ux) <= max_numeric) return(ux)
  stop("A numeric focal variable has more than ", max_numeric,
       " unique values. Specify evaluation points with `at = list(variable = ...)`.", call. = FALSE)
}

.gph_expand <- function(lst) {
  if (!length(lst)) return(data.frame(.gph_dummy = 1)[, FALSE, drop = FALSE])
  do.call(expand.grid, c(lst, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE))
}

.gph_restore_classes <- function(d, template) {
  common <- intersect(names(d), names(template))
  for (v in common) {
    if (is.factor(template[[v]])) {
      z <- factor(d[[v]], levels = levels(template[[v]]), ordered = is.ordered(template[[v]]))
      if (anyNA(z) && any(!is.na(d[[v]]))) {
        bad <- unique(as.character(d[[v]])[is.na(z) & !is.na(d[[v]])])
        stop("Prediction grid contains unknown level(s) for factor '", v, "': ",
             paste(bad, collapse = ", "), call. = FALSE)
      }
      d[[v]] <- z
    } else if (inherits(template[[v]], "Date")) {
      d[[v]] <- as.Date(d[[v]], origin = "1970-01-01")
    }
  }
  d
}

.gph_resolve_population <- function(population = NULL, grid = NULL) {
  allowed <- c("observed", "balanced", "reference")
  if (is.null(population)) {
    if (!is.null(grid)) population <- grid else population <- "observed"
  } else if (!is.null(grid) && !identical(as.character(population)[1L], as.character(grid)[1L])) {
    warning("Both `population` and deprecated alias `grid` were supplied; using `population`.", call. = FALSE)
  }
  population <- match.arg(as.character(population)[1L], allowed)
  population
}

.gph_resolve_weights <- function(weights, data, population) {
  if (is.null(weights)) weights <- if (population == "observed") "proportional" else "equal"
  if (is.character(weights)) {
    weights <- match.arg(weights[1L], c("proportional", "equal"))
    return(weights)
  }
  if (!is.numeric(weights) || length(weights) != nrow(data) || any(!is.finite(weights)) || any(weights < 0)) {
    stop("`weights` must be 'proportional', 'equal', or one non-negative finite numeric weight per data row.", call. = FALSE)
  }
  if (sum(weights) <= 0) stop("Numeric `weights` must have a positive sum.", call. = FALSE)
  as.numeric(weights)
}

.gph_build_eval_data <- function(object, data, specs, by = NULL, at = list(),
                                 population = "observed", weights = NULL) {
  population <- .gph_resolve_population(population)
  weights <- .gph_resolve_weights(weights, data, population)
  if (!is.list(at)) stop("`at` must be a named list.", call. = FALSE)
  if (length(at) && (is.null(names(at)) || any(!nzchar(names(at))))) {
    stop("All elements of `at` must be named.", call. = FALSE)
  }
  unknown_at <- setdiff(names(at), names(data))
  if (length(unknown_at)) stop("Variables in `at` not found in data: ", paste(unknown_at, collapse = ", "), call. = FALSE)
  focal <- unique(c(specs, by))
  miss <- setdiff(focal, names(data))
  if (length(miss)) stop("Variables not found in data: ", paste(miss, collapse = ", "), call. = FALSE)

  focal_values <- lapply(focal, function(v) .gph_levels_or_values(data[[v]], at[[v]]))
  names(focal_values) <- focal
  combos <- .gph_expand(focal_values)
  combos$.gph_group_id <- seq_len(nrow(combos))

  if (population == "observed") {
    base_w <- if (is.numeric(weights)) weights / sum(weights) else rep(1 / nrow(data), nrow(data))
    chunks <- lapply(seq_len(nrow(combos)), function(i) {
      d <- data
      for (v in focal) d[[v]] <- combos[[v]][i]
      for (v in setdiff(names(at), focal)) {
        if (length(at[[v]]) != 1L) {
          stop("For non-focal variables with population='observed', `at` must contain one value per variable.", call. = FALSE)
        }
        d[[v]] <- at[[v]][1]
      }
      d <- .gph_restore_classes(d, data)
      d$.gph_group_id <- combos$.gph_group_id[i]
      d$.gph_weight <- base_w
      d
    })
    ev <- do.call(rbind, chunks)
    rownames(ev) <- NULL
    return(list(eval_data = ev, groups = combos, population = population,
                weighting = if (is.numeric(weights)) "user-supplied" else weights,
                base_data = data, weights = ev$.gph_weight))
  }

  preds <- intersect(.gph_all_predictors(object), names(data))
  nuisance <- setdiff(preds, focal)
  vals <- list()
  for (v in nuisance) {
    x <- data[[v]]
    if (!is.null(at[[v]])) vals[[v]] <- at[[v]]
    else if (population == "balanced" && (is.factor(x) || is.character(x) || is.logical(x))) vals[[v]] <- .gph_levels_or_values(x)
    else if (is.numeric(x)) vals[[v]] <- mean(x, na.rm = TRUE)
    else if (is.factor(x)) vals[[v]] <- levels(x)[1]
    else if (is.character(x)) vals[[v]] <- unique(x)[1]
    else if (is.logical(x)) vals[[v]] <- FALSE
  }
  nuisance_grid <- .gph_expand(vals)
  if (!nrow(nuisance_grid)) nuisance_grid <- data.frame(.gph_dummy = 1)[, FALSE, drop = FALSE]
  nuisance_w <- rep(1 / nrow(nuisance_grid), nrow(nuisance_grid))
  chunks <- lapply(seq_len(nrow(combos)), function(i) {
    d <- nuisance_grid
    for (v in focal) d[[v]] <- combos[[v]][i]
    d <- .gph_restore_classes(d, data)
    d$.gph_group_id <- combos$.gph_group_id[i]
    d$.gph_weight <- nuisance_w
    d
  })
  ev <- do.call(rbind, chunks)
  rownames(ev) <- NULL
  list(eval_data = ev, groups = combos, population = population,
       weighting = "equal", base_data = nuisance_grid, weights = ev$.gph_weight)
}

.gph_aggregate <- function(values, eval_info) {
  gid <- eval_info$eval_data$.gph_group_id
  w <- eval_info$eval_data$.gph_weight
  ids <- eval_info$groups$.gph_group_id
  est <- vapply(ids, function(id) {
    ii <- which(gid == id & is.finite(values) & is.finite(w))
    if (!length(ii)) return(NA_real_)
    stats::weighted.mean(values[ii], w[ii], na.rm = TRUE)
  }, numeric(1))
  out <- eval_info$groups
  out$estimate <- est
  out
}

.gph_relabel_groups <- function(df) {
  if (".gph_group_id" %in% names(df)) df$.gph_group_id <- NULL
  df
}

.gph_contrast_weights <- function(k, method = "pairwise", ref = 1L, scores = NULL, degree = NULL) {
  method <- match.arg(method, c("pairwise", "reference", "sequential", "poly", "opoly"))
  k <- suppressWarnings(as.integer(k)[1L])
  if (!is.finite(k) || k < 2L) stop("At least two levels are required for a contrast.", call. = FALSE)
  ref <- suppressWarnings(as.integer(ref)[1L])
  if (!is.finite(ref) || ref < 1L || ref > k) stop("`ref` must index one of the available levels.", call. = FALSE)
  if (!is.null(scores)) {
    if (!is.numeric(scores) || length(scores) != k || any(!is.finite(scores))) {
      stop("`scores` must contain one finite numeric value for each level.", call. = FALSE)
    }
    if (length(unique(scores)) != k) stop("`scores` must be distinct.", call. = FALSE)
  }
  if (!is.null(degree)) {
    degree <- suppressWarnings(as.integer(degree)[1L])
    if (!is.finite(degree) || degree < 1L || degree > (k - 1L)) {
      stop("`degree` must be between 1 and the number of levels minus 1.", call. = FALSE)
    }
  }
  W <- list(); labs <- character()
  if (method == "pairwise") {
    for (i in seq_len(k - 1L)) for (j in (i + 1L):k) {
      w <- numeric(k); w[i] <- 1; w[j] <- -1
      W[[length(W) + 1L]] <- w; labs <- c(labs, paste(i, "-", j))
    }
  } else if (method == "reference") {
    others <- setdiff(seq_len(k), ref)
    for (i in others) {
      w <- numeric(k); w[i] <- 1; w[ref] <- -1
      W[[length(W) + 1L]] <- w; labs <- c(labs, paste(i, "-", ref))
    }
  } else if (method == "sequential") {
    for (i in 2:k) {
      w <- numeric(k); w[i] <- 1; w[i - 1L] <- -1
      W[[length(W) + 1L]] <- w; labs <- c(labs, paste(i, "-", i - 1L))
    }
  } else {
    if (is.null(scores)) scores <- seq_len(k)
    if (is.null(degree)) degree <- min(k - 1L, 3L)
    P <- stats::poly(scores, degree = degree, simple = TRUE)
    nms_all <- c("linear", "quadratic", "cubic", if (degree >= 4L) paste0("degree", 4:degree) else character())
    for (j in seq_len(degree)) {
      W[[j]] <- P[, j]
      labs <- c(labs, nms_all[j])
    }
  }
  names(W) <- labs
  W
}

.gph_comparison_value <- function(hi, lo, comparison = "difference") {
  comparison <- match.arg(comparison, c("difference", "ratio", "log_ratio", "percent_change"))
  if (comparison == "difference") return(hi - lo)
  if (any(!is.finite(c(hi, lo)))) return(NA_real_)
  if (comparison == "ratio") return(ifelse(lo == 0, NA_real_, hi / lo))
  if (comparison == "log_ratio") return(ifelse(hi > 0 && lo > 0, log(hi / lo), NA_real_))
  ifelse(lo == 0, NA_real_, 100 * (hi / lo - 1))
}

.gph_comparison_null <- function(comparison) {
  if (comparison %in% c("ratio")) 1 else 0
}

.gph_comparison_label <- function(comparison) {
  switch(comparison,
         difference = "difference",
         ratio = "ratio",
         log_ratio = "log ratio",
         percent_change = "percent change")
}

.gph_make_contrasts <- function(estimates, specs, by = NULL, method = "pairwise",
                                ref = 1L, scores = NULL, degree = NULL, draws = NULL,
                                comparison = "difference") {
  comparison <- match.arg(comparison, c("difference", "ratio", "log_ratio", "percent_change"))
  if (length(specs) != 1L) {
    stop("Automatic contrasts currently require exactly one variable in `specs`. ",
         "For multi-factor contrasts, create a custom contrast from the returned estimates.", call. = FALSE)
  }
  if (method %in% c("poly", "opoly") && comparison != "difference") {
    stop("Polynomial contrasts are linear combinations and therefore require `comparison='difference'`.", call. = FALSE)
  }
  s <- specs[1]
  split_key <- if (length(by)) interaction(estimates[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(estimates)))
  idx_list <- split(seq_len(nrow(estimates)), split_key)
  rows <- list(); draw_rows <- list()
  for (nm in names(idx_list)) {
    ii <- idx_list[[nm]]
    sub <- estimates[ii, , drop = FALSE]
    W <- .gph_contrast_weights(nrow(sub), method, ref, scores, degree)
    for (cn in names(W)) {
      w <- W[[cn]]
      rr <- if (length(by)) sub[1, by, drop = FALSE] else data.frame(.dummy = 1)[, FALSE, drop = FALSE]
      if (method %in% c("pairwise", "reference", "sequential")) {
        ip <- which(w > 0)[1]; im <- which(w < 0)[1]
        rr$contrast <- paste(as.character(sub[[s]][ip]), "vs", as.character(sub[[s]][im]))
        rr$estimate <- .gph_comparison_value(sub$estimate[ip], sub$estimate[im], comparison)
        rr$comparison <- comparison
        if (!is.null(draws)) {
          d <- mapply(.gph_comparison_value,
                      hi = as.numeric(draws[, ii[ip]]), lo = as.numeric(draws[, ii[im]]),
                      MoreArgs = list(comparison = comparison))
          draw_rows[[length(draw_rows) + 1L]] <- as.numeric(d)
        }
      } else {
        rr$contrast <- cn
        rr$estimate <- sum(w * sub$estimate)
        rr$comparison <- "difference"
        if (!is.null(draws)) draw_rows[[length(draw_rows) + 1L]] <- as.numeric(as.matrix(draws[, ii, drop = FALSE]) %*% w)
      }
      rows[[length(rows) + 1L]] <- rr
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  if (!is.null(draws) && length(draw_rows)) {
    D <- do.call(cbind, draw_rows)
    out$SE <- apply(D, 2, stats::sd, na.rm = TRUE)
    out$lower.CL <- apply(D, 2, stats::quantile, probs = 0.025, na.rm = TRUE)
    out$upper.CL <- apply(D, 2, stats::quantile, probs = 0.975, na.rm = TRUE)
    null <- .gph_comparison_null(comparison)
    out$p.value <- apply(D, 2, function(z) {
      z <- z[is.finite(z)]
      if (!length(z)) return(NA_real_)
      n <- length(z)
      min(1, 2 * min((sum(z <= null) + 1) / (n + 1), (sum(z >= null) + 1) / (n + 1)))
    })
    attr(out, "draws") <- D
  }
  out
}

.gph_refit <- function(object, boot_data, data_original, refit_fun = NULL) {
  if (is.function(refit_fun)) return(refit_fun(object, boot_data))
  cl <- object$call
  if (is.null(cl)) stop("The fitted object has no stored call. Supply `refit_fun`.", call. = FALSE)
  cl$data <- quote(.gph_boot_data)
  f <- try(.gph_get_formula(object, "mu"), silent = TRUE)
  fenv <- if (!inherits(f, "try-error") && !is.null(f)) environment(f) else NULL
  env <- new.env(parent = fenv %||% parent.frame())
  env$.gph_boot_data <- boot_data
  eval(cl, envir = env)
}

.gph_simulate_response <- function(object, data, simulate_fun = NULL, positive_dist_fun = NULL) {
  if (is.function(simulate_fun)) return(simulate_fun(object, data))
  D <- .gph_distribution(object, newdata = data, data = data, positive_dist_fun = positive_dist_fun)
  if (.gph_is_zadj(object)) return(.gph_zadj_random(D, n = 1L))
  .gph_require("distributions3", "for parametric bootstrap.")
  as.numeric(distributions3::random(D, n = 1L))
}

.gph_boot_data <- function(data, method, cluster = NULL) {
  n <- nrow(data)
  if (method == "case") return(data[sample.int(n, n, replace = TRUE), , drop = FALSE])
  if (method == "cluster") {
    if (is.null(cluster) || !cluster %in% names(data)) stop("Supply a valid `cluster` column.", call. = FALSE)
    lev <- unique(data[[cluster]])
    sel <- sample(lev, length(lev), replace = TRUE)
    chunks <- lapply(seq_along(sel), function(i) {
      d <- data[data[[cluster]] == sel[i], , drop = FALSE]
      d[[cluster]] <- factor(paste0(as.character(sel[i]), "__boot", i))
      d
    })
    return(do.call(rbind, chunks))
  }
  stop("Unknown bootstrap method.", call. = FALSE)
}

.gph_parallel_lapply <- function(X, FUN, parallel = FALSE, cores = 2L, seed = NULL) {
  if (!parallel) return(lapply(X, FUN))
  cores <- suppressWarnings(as.integer(cores)[1L])
  if (!is.finite(cores) || cores < 1L) cores <- 1L
  if (cores > 2L) {
    warning("CRAN policy limits package examples/check-time parallelism to two workers; using 2 workers.", call. = FALSE)
    cores <- 2L
  }
  if (!requireNamespace("future.apply", quietly = TRUE) || !requireNamespace("future", quietly = TRUE)) {
    warning("Packages 'future' and 'future.apply' not installed; using sequential bootstrap.", call. = FALSE)
    return(lapply(X, FUN))
  }
  old <- future::plan()
  on.exit(future::plan(old), add = TRUE)
  future::plan(future::multisession, workers = cores)
  future.apply::future_lapply(X, FUN, future.seed = seed %||% TRUE)
}


# ===== adapters.R =====

# Internal model-adapter layer -------------------------------------------------
#
# The public API should depend on generic model capabilities rather than on the
# conventional GAMLSS parameter names.  The current CRAN gamlss implementation
# still uses mu/sigma/nu/tau, while gamlssZadj adds xi0.  Keeping those details
# here makes the post-processing core easier to extend to future model classes.

.gph_is_zadj <- function(object) inherits(object, "gamlssZadj")
.gph_is_gamlss <- function(object) inherits(object, "gamlss") || .gph_is_zadj(object)

.gph_model_class <- function(object) {
  if (.gph_is_zadj(object)) return("gamlssZadj")
  if (inherits(object, "gamlss")) return("gamlss")
  class(object)[1L] %||% "unknown"
}

.gph_model_parameters <- function(object) {
  p <- object$parameters
  if (is.null(p) || !length(p)) {
    # Dynamic fallback for older/atypical fitted objects.  Parameter names are
    # inferred from formula/link components instead of a fixed mu/sigma/nu/tau
    # vocabulary; external engines may still impose their own narrower support.
    fpars <- sub("\\.formula$", "", grep("\\.formula$", names(object), value = TRUE))
    lpars <- sub("\\.link$", "", grep("\\.link$", names(object), value = TRUE))
    p <- union(fpars, lpars)
  }
  unique(as.character(p))
}

.gph_model_family <- function(object) {
  if (.gph_is_zadj(object)) {
    fam <- object$original.family %||% object$base.family
    if (!is.null(fam) && length(fam)) return(as.character(fam)[1L])
  }
  fam <- object$family
  if (!is.null(fam) && length(fam)) {
    fam <- as.character(fam)[1L]
    # gamlssZadj labels can be ZadjGA, ZadjGG, ...; use only as a fallback.
    fam <- sub("^Zadj", "", fam)
    return(fam)
  }
  NA_character_
}

.gph_get_formula <- function(object, what = NULL) {
  if (is.null(what)) return(NULL)
  if (!what %in% .gph_model_parameters(object)) return(NULL)

  # Use each fitted class's documented formula method instead of relying on
  # private storage slots. gamlssZadj uses `parameter=`, while ordinary gamlss
  # uses `what=` for the conventional distributional submodels.
  if (.gph_is_zadj(object)) {
    f <- try(stats::formula(object, parameter = what), silent = TRUE)
    if (!inherits(f, "try-error") && !is.null(f)) return(f)
  } else {
    f <- try(stats::formula(object, what = what), silent = TRUE)
    if (!inherits(f, "try-error") && !is.null(f)) return(f)
  }

  nm <- paste0(what, ".formula")
  f <- object[[nm]] %||% object$call[[nm]]
  if (!is.null(f)) return(stats::as.formula(f))
  NULL
}

.gph_formula_has_smoother <- function(f) {
  if (is.null(f)) return(FALSE)
  txt <- paste(deparse(f), collapse = " ")
  grepl("\\b(pb|ps|cs|lo|random|re|ga|nl|pvc|pbm|pbc|pbz|s|te|ti|t2)\\s*\\(", txt)
}

.gph_has_smoother <- function(object, what = NULL) {
  .gph_formula_has_smoother(.gph_get_formula(object, what))
}

.gph_parameter_table <- function(object) {
  p <- .gph_model_parameters(object)
  if (!length(p)) return(data.frame())
  data.frame(
    parameter = p,
    smoother = vapply(p, function(z) .gph_has_smoother(object, z), logical(1)),
    formula = vapply(p, function(z) {
      f <- .gph_get_formula(object, z)
      if (is.null(f)) NA_character_ else paste(deparse(f), collapse = " ")
    }, character(1)),
    link = vapply(p, function(z) as.character(object[[paste0(z, ".link")]] %||% NA_character_)[1L], character(1)),
    stringsAsFactors = FALSE
  )
}

.gph_all_predictors <- function(object) {
  vars <- character()
  for (p in .gph_model_parameters(object)) {
    f <- .gph_get_formula(object, p)
    if (!is.null(f)) vars <- union(vars, all.vars(stats::delete.response(stats::terms(f))))
  }
  vars
}

.gph_predict_parameter <- function(object, newdata, what, data = NULL,
                                   type = c("response", "link")) {
  type <- match.arg(type)
  pars <- .gph_model_parameters(object)
  if (!what %in% pars) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(pars, collapse = ", "), ".", call. = FALSE)
  }
  if (.gph_is_zadj(object)) {
    return(as.numeric(stats::predict(object, parameter = what, newdata = newdata,
                                     type = type, data = data)))
  }
  # Current CRAN gamlss exposes `what` for its distributional parameters.
  as.numeric(stats::predict(object, what = what, newdata = newdata,
                            type = type, data = data))
}

.gph_predict_parameter_standardized <- function(object, eval_info, what, data = NULL,
                                                scale = c("response", "link")) {
  scale <- match.arg(scale)
  z <- .gph_predict_parameter(object, eval_info$eval_data, what, data = data,
                              type = scale)
  .gph_aggregate(z, eval_info)
}

.gph_predict_parameters <- function(object, newdata, data = NULL) {
  out <- list()
  for (p in .gph_model_parameters(object)) {
    val <- try(.gph_predict_parameter(object, newdata, p, data), silent = TRUE)
    if (!inherits(val, "try-error")) out[[p]] <- as.numeric(val)
  }
  if (!length(out)) stop("No distributional parameters could be predicted.", call. = FALSE)
  out
}

.gph_adapter <- function(object) {
  if (!.gph_is_gamlss(object)) {
    stop("`object` must inherit from 'gamlss' or 'gamlssZadj'.", call. = FALSE)
  }
  list(
    model_class = .gph_model_class(object),
    family = .gph_model_family(object),
    parameters = .gph_model_parameters(object),
    zero_adjusted = .gph_is_zadj(object),
    parameter_table = .gph_parameter_table(object)
  )
}


# ===== zero_adjusted.R =====

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
    return(gamlss::prodist(object, newdata = newdata, data = data))
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
      as.numeric(stats::quantile(z$positive[idx[j]], probs = qadj[j], names = FALSE))[1L]
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
    return(as.numeric(stats::quantile(D, probs = prob, names = FALSE)))
  }
  stop("Unknown estimand.", call. = FALSE)
}


# ===== estimands.R =====

.gph_estimand_info <- function(object, estimand, what, prob, population, weighting) {
  zadj <- .gph_is_zadj(object)
  target <- switch(estimand,
    parameter = paste0("distributional parameter ", what),
    mean = if (zadj) "marginal response mean including the zero mass" else "conditional response mean",
    variance = if (zadj) "marginal response variance including the zero mass" else "conditional response variance",
    quantile = paste0("conditional response quantile Q(", format(prob), ")"),
    prob_zero = "exact probability mass at zero",
    custom = "user-defined function of distributional parameters"
  )
  definition <- switch(estimand,
    parameter = paste0(what, "(x) on its response-parameter scale"),
    mean = if (zadj) "E(Y|x) = [1 - P(Y=0|x)] E(Y+|Y>0,x)" else "E(Y|x)",
    variance = if (zadj) "Var(Y|x) = (1-h) Var(Y+|x) + h(1-h) E(Y+|x)^2" else "Var(Y|x)",
    quantile = if (zadj) "Q(p)=0 for p<=h; otherwise Q+((p-h)/(1-h))" else paste0("Q_Y(", format(prob), "|x)"),
    prob_zero = "P(Y=0|x)",
    custom = "custom_fun(pars, newdata, object)"
  )
  data.frame(
    estimand = estimand,
    target = target,
    definition = definition,
    scale = if (estimand == "parameter") "parameter-response" else "response/distribution",
    population = population,
    weighting = weighting,
    conditioning = "conditional predictions standardized over the declared target population",
    stringsAsFactors = FALSE
  )
}

.gph_engine_eligibility <- function(object, estimand, what, contrast, comparison,
                                    population = "observed", weights = NULL) {
  pars <- .gph_model_parameters(object)
  smoother <- if (what %in% pars) .gph_has_smoother(object, what) else FALSE
  emmeans_ok <- estimand == "parameter" && what %in% c("mu", "sigma", "nu", "tau") &&
    !.gph_is_zadj(object) && !smoother && comparison == "difference" &&
    identical(population, "reference") && !is.numeric(weights)
  me_ok <- estimand == "parameter" && what %in% pars && !.gph_is_zadj(object)
  list(emmeans = emmeans_ok, marginaleffects = me_ok, distribution = TRUE,
       smoother = smoother)
}

.gph_choose_engine <- function(object, estimand, what, contrast, comparison, engine,
                               population = "observed", weights = NULL) {
  elig <- .gph_engine_eligibility(object, estimand, what, contrast, comparison, population, weights)
  if (engine != "auto") return(list(engine = engine, eligibility = elig))
  if (elig$emmeans && requireNamespace("emmeans", quietly = TRUE)) {
    return(list(engine = "emmeans", eligibility = elig))
  }
  if (elig$marginaleffects && requireNamespace("marginaleffects", quietly = TRUE)) {
    return(list(engine = "marginaleffects", eligibility = elig))
  }
  list(engine = "distribution", eligibility = elig)
}

.gph_choose_uncertainty <- function(engine, uncertainty, estimand, smoother = FALSE) {
  if (uncertainty != "auto") return(uncertainty)
  if (engine %in% c("emmeans", "marginaleffects") && estimand == "parameter") return("delta")
  # A generic refit bootstrap can be expensive. Auto therefore avoids
  # silently launching hundreds of refits; the diagnostic plan recommends
  # bootstrap when interval uncertainty is required for derived targets.
  if (engine == "distribution") return("none")
  if (smoother) return("none")
  "none"
}

.gph_me_comparison <- function(comparison) {
  # avg_comparisons() automatically uses its average-scale versions of these
  # shortcuts.  Percent change is obtained from the average-scale ratio and is
  # transformed after inference so it remains exactly comparable with the
  # distribution engine: 100 * (mean_hi / mean_lo - 1).
  switch(comparison,
         difference = "difference",
         ratio = "ratio",
         log_ratio = "lnratio",
         percent_change = "ratio")
}

.gph_me_variables <- function(specs, contrast, ref = 1L) {
  if (length(specs) != 1L || !contrast %in% c("pairwise", "reference", "sequential")) return(NULL)
  if (contrast == "reference" && !identical(as.integer(ref)[1L], 1L)) return(NULL)
  # marginaleffects defines pairwise as later - earlier; revpairwise aligns
  # direction with the package's internal/emmeans convention (earlier - later).
  # Reference and sequential already use current level relative to reference or
  # previous level, matching our local convention.
  method <- switch(contrast, pairwise = "revpairwise", reference = "reference", sequential = "sequential")
  stats::setNames(list(method), specs)
}


# ===== gamlss_posthoc_plan.R =====

#' Diagnose post-hoc capabilities for a fitted GAMLSS model
#'
#' @description
#' Inspects model class, family, distributional parameters, smooth terms,
#' optional dependencies, and the requested estimand. The result documents
#' which engines are eligible and explains the recommended engine and
#' uncertainty layer before inference is run.
#'
#' @param object A fitted `gamlss` or `gamlssZadj` model.
#' @param estimand Requested estimand accepted by [gamlss_posthoc()].
#' @param what Distributional parameter for a parameter-wise estimand.
#' @param contrast Requested level-comparison geometry.
#' @param comparison Scientific contrast scale.
#' @param population Target standardization population.
#' @param weights Weighting rule used to determine engine eligibility. Numeric
#'   observation weights rule out the `emmeans` reference-grid engine.
#' @param uncertainty Requested uncertainty layer.
#' @return An object of class `gamlss_posthoc_plan`.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(13)
#'   d <- data.frame(x = seq(0, 1, length.out = 60),
#'                   trt = factor(rep(c("A", "B"), 30)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(.5 + .4*d$x + .15*(d$trt=="B")), sigma=.25)
#'   fit <- gamlss::gamlss(y ~ trt + x, sigma.formula = ~ trt,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   # 1. Marginal response mean on the observed population
#'   gamlss_posthoc_plan(fit, estimand = "mean", population = "observed")
#'   # 2. Parameter-wise reference-grid inference
#'   gamlss_posthoc_plan(fit, estimand = "parameter", what = "sigma",
#'                       population = "reference", uncertainty = "delta")
#'   # 3. Distribution-derived variance
#'   gamlss_posthoc_plan(fit, estimand = "variance", contrast = "pairwise")
#' }
gamlss_posthoc_plan <- function(object, estimand = "parameter", what = "mu",
                                 contrast = "none", comparison = "difference",
                                 population = "observed", weights = NULL,
                                 uncertainty = "auto") {
  if (!.gph_is_gamlss(object)) stop("`object` must inherit from 'gamlss' or 'gamlssZadj'.", call. = FALSE)
  estimand <- match.arg(estimand, c("parameter", "mean", "variance", "quantile", "prob_zero", "custom"))
  contrast <- match.arg(contrast, c("none", "pairwise", "reference", "sequential", "poly", "opoly"))
  comparison <- match.arg(comparison, c("difference", "ratio", "log_ratio", "percent_change"))
  population <- .gph_resolve_population(population)
  uncertainty <- match.arg(uncertainty, c("auto", "delta", "simulation", "bootstrap", "none"))
  ad <- .gph_adapter(object)
  if (estimand == "parameter" && !what %in% ad$parameters) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(ad$parameters, collapse = ", "), ".", call. = FALSE)
  }
  elig <- .gph_engine_eligibility(object, estimand, what, contrast, comparison, population, weights)
  route <- .gph_choose_engine(object, estimand, what, contrast, comparison, "auto", population, weights)
  planned_engine <- route$engine
  unc <- .gph_choose_uncertainty(planned_engine, uncertainty, estimand, elig$smoother)
  unc_recommended <- unc
  if (planned_engine == "emmeans" && !unc %in% c("delta", "none")) unc_recommended <- "delta"
  if (planned_engine == "distribution" && unc %in% c("delta", "simulation")) unc_recommended <- "bootstrap"
  if (planned_engine == "marginaleffects" && identical(unc, "bootstrap")) planned_engine <- "distribution"
  if (uncertainty == "auto" && planned_engine == "distribution" && estimand != "parameter") {
    unc_recommended <- "none; use bootstrap explicitly when interval inference is required"
  }
  dep_names <- c("gamlss", "gamlss.dist", "gamlss.inf", "distributions3", "emmeans",
                 "marginaleffects", "future", "future.apply")
  deps <- data.frame(
    package = dep_names,
    installed = vapply(dep_names, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)),
    stringsAsFactors = FALSE
  )
  capabilities <- data.frame(
    engine = c("emmeans", "marginaleffects", "distribution"),
    eligible = c(elig$emmeans, elig$marginaleffects, TRUE),
    available = c(requireNamespace("emmeans", quietly = TRUE),
                  requireNamespace("marginaleffects", quietly = TRUE),
                  requireNamespace("gamlss", quietly = TRUE) &&
                    requireNamespace("distributions3", quietly = TRUE) &&
                    (!ad$zero_adjusted || (requireNamespace("gamlss.dist", quietly = TRUE) &&
                                           requireNamespace("gamlss.inf", quietly = TRUE)))),
    reason = c(
      if (elig$emmeans) "Supported single parameter on an explicit reference population, with no selected smoother." else "Requires ordinary mu/sigma/nu/tau parameter, no selected smoother, arithmetic differences, population=reference, and non-numeric reference-grid weights.",
      if (elig$marginaleffects) "Parameter-wise standardization is eligible; runtime capability is still checked." else "Current adapter reserves marginaleffects for ordinary parameter-wise GAMLSS requests.",
      if (ad$zero_adjusted) "Uses the full positive distribution plus the zero mass." else "Uses the full predictive distribution via prodist()."
    ), stringsAsFactors = FALSE
  )
  parameter_capabilities <- ad$parameter_table
  if (nrow(parameter_capabilities)) {
    parameter_capabilities$emmeans_eligible <-
      !ad$zero_adjusted & parameter_capabilities$parameter %in% c("mu", "sigma", "nu", "tau") &
      !parameter_capabilities$smoother & identical(population, "reference") & !is.numeric(weights)
    parameter_capabilities$marginaleffects_candidate <-
      !ad$zero_adjusted & parameter_capabilities$parameter %in% ad$parameters
    parameter_capabilities$distribution_prediction <- TRUE
  }
  warnings <- character()
  if (ad$zero_adjusted && !requireNamespace("gamlss.dist", quietly = TRUE)) warnings <- c(warnings, "gamlss.dist is needed to reconstruct the positive distribution generically.")
  if (elig$smoother) warnings <- c(warnings, "A smoother is present in the selected submodel; emmeans is intentionally avoided.")
  if (contrast %in% c("poly", "opoly") && comparison != "difference") warnings <- c(warnings, "Polynomial contrasts must use arithmetic linear combinations.")
  if (is.numeric(weights) && comparison %in% c("ratio", "log_ratio", "percent_change") && uncertainty %in% c("auto", "delta", "simulation")) {
    warnings <- c(warnings, "Numeric user weights with nonlinear contrast scales use standardized group predictions; direct marginaleffects covariance shortcuts are intentionally avoided. Request bootstrap for interval inference if needed.")
  }
  out <- list(
    overview = data.frame(model_class = ad$model_class, family = ad$family,
                          zero_adjusted = ad$zero_adjusted,
                          parameters = paste(ad$parameters, collapse = ", "), stringsAsFactors = FALSE),
    parameter_table = ad$parameter_table,
    parameter_capabilities = parameter_capabilities,
    dependencies = deps,
    capabilities = capabilities,
    request = data.frame(estimand = estimand, parameter = what, contrast = contrast,
                         comparison = comparison, population = population,
                         weighting = if (is.null(weights)) "default" else if (is.numeric(weights)) "numeric user weights" else as.character(weights)[1L],
                         uncertainty = uncertainty, stringsAsFactors = FALSE),
    recommendation = data.frame(engine = planned_engine, uncertainty = unc_recommended,
                                reason = if (planned_engine == "emmeans") "Direct parameter-wise reference-grid marginal means are supported."
                                else if (planned_engine == "marginaleffects") "Prediction-based standardization is preferred for this parameter-wise request."
                                else if (route$engine == "marginaleffects" && planned_engine == "distribution") "Refit bootstrap uncertainty is handled by the distribution engine for version-stable behavior."
                                else "The requested target depends on the full distribution or multiple parameters.",
                                stringsAsFactors = FALSE),
    warnings = warnings
  )
  class(out) <- "gamlss_posthoc_plan"
  out
}

#' @method print gamlss_posthoc_plan
#' @export
#' @noRd
print.gamlss_posthoc_plan <- function(x, ...) {
  cat("gamlssPosthoc diagnostic plan\n\n")
  print(x$overview, row.names = FALSE)
  cat("\nDistributional parameters\n")
  print(x$parameter_table, row.names = FALSE)
  cat("\nParameter capabilities\n")
  print(x$parameter_capabilities, row.names = FALSE)
  cat("\nEngine capabilities\n")
  print(x$capabilities, row.names = FALSE)
  cat("\nRecommendation\n")
  print(x$recommendation, row.names = FALSE)
  if (length(x$warnings)) cat("\nWarnings\n- ", paste(x$warnings, collapse = "\n- "), "\n", sep = "")
  invisible(x)
}


# ===== gamlss_posthoc.R =====

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
#'   fit <- gamlss::gamlss(y ~ trt, data = d, family = gamlss.dist::GA,
#'                         trace = FALSE)
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
    ans <- .gph_posthoc_marginaleffects(
      object = object, specs = specs, by = by, what = what,
      contrast = contrast, comparison = comparison, at = at,
      population = population, weights = weights, uncertainty = uncertainty,
      B = B, ref = ref, scores = scores, degree = degree, adjust = adjust, data = data, ...
    )
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
    emm <- emmeans::emmeans(object, specs = spec_formula, what = what, data = data, at = at,
                            weights = emm_weights, ...)
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
  me <- try(marginaleffects::avg_predictions(
    object, newdata = eval_info$eval_data, by = ".gph_group_id",
    type = "response", what = what, wts = eval_info$eval_data$.gph_weight,
    vcov = if (uncertainty == "none") FALSE else TRUE, ...
  ), silent = TRUE)
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
      vcov = if (uncertainty == "none") FALSE else TRUE
    ), if (length(by)) list(by = by) else list(), list(...))
    cmp <- try(do.call(marginaleffects::avg_comparisons, cmp_args), silent = TRUE)
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


# ===== gamlss_distribution_summary.R =====

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
      out[[nm]] <- as.numeric(stats::quantile(D, probs = p, names = FALSE))
    }
  }
  out$sd <- sqrt(out$variance)
  attr(out, "model") <- object
  attr(out, "data") <- data
  class(out) <- c("gamlss_distribution_summary", "data.frame")
  out
}


# ===== gamlss_trend.R =====

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
  ans <- list(x = x[j], y = y[j], refined = FALSE)
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
  list(x = xv, y = yv, refined = TRUE)
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


# ===== gamlss_poly_compare.R =====

#' Compare already-fitted nested polynomial GAMLSS models
#'
#' Summarizes the generalized AIC (GAIC) and sequential likelihood-ratio (LR)
#' comparisons for nested GAMLSS models. This helper is intended for
#' quantitative polynomial regression and is deliberately separate from
#' polynomial contrasts among ordered factor levels.
#'
#' Models must be fitted to the same response and observations and supplied in
#' increasing order of complexity. The function does not itself verify formal
#' nestedness; the user remains responsible for supplying scientifically and
#' statistically comparable models.
#'
#' @param ... Two or more fitted GAMLSS models, or one named list containing
#'   such models, ordered from simpler to more complex.
#' @param k Positive penalty multiplying the effective degrees of freedom in
#'   GAIC. `k = 2` gives AIC; `k = log(n)` corresponds to a BIC-like penalty
#'   when `n` is the relevant sample size.
#' @return A data frame with model name, effective degrees of freedom, global
#'   deviance, GAIC, and sequential LR statistic, degrees-of-freedom difference,
#'   and p-value.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(15)
#'   d <- data.frame(dose = rep(seq(0, 100, length.out = 8), each = 7))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(.5 + .01*d$dose - .00004*d$dose^2), sigma=.22)
#'   m1 <- gamlss::gamlss(y ~ dose, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   m2 <- gamlss::gamlss(y ~ dose + I(dose^2), family=gamlss.dist::GA, data=d, trace=FALSE)
#'   m3 <- gamlss::gamlss(y ~ dose + I(dose^2) + I(dose^3), family=gamlss.dist::GA, data=d, trace=FALSE)
#'   # 1. AIC penalty
#'   gamlss_poly_compare(linear=m1, quadratic=m2, cubic=m3, k=2)
#'   # 2. BIC-like penalty
#'   gamlss_poly_compare(linear=m1, quadratic=m2, cubic=m3, k=log(nrow(d)))
#'   # 3. Models can also be supplied as one named list
#'   gamlss_poly_compare(list(linear=m1, quadratic=m2, cubic=m3), k=2)
#' }
gamlss_poly_compare <- function(..., k = 2) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
    stop("`k` must be one positive finite number.", call. = FALSE)
  }
  mods <- list(...)
  if (length(mods) == 1L && is.list(mods[[1]]) && !.gph_is_gamlss(mods[[1]])) mods <- mods[[1]]
  if (length(mods) < 2L) stop("Supply at least two fitted nested GAMLSS models.", call. = FALSE)
  if (!all(vapply(mods, .gph_is_gamlss, logical(1)))) stop("All supplied models must be GAMLSS fits.", call. = FALSE)
  if (is.null(names(mods)) || any(names(mods) == "")) names(mods) <- paste0("model", seq_along(mods))
  tab <- data.frame(
    model = names(mods),
    df = vapply(mods, function(m) m$df.fit %||% NA_real_, numeric(1)),
    global_deviance = vapply(mods, function(m) m$G.deviance %||% NA_real_, numeric(1)),
    GAIC = vapply(mods, function(m) (m$G.deviance %||% NA_real_) + k * (m$df.fit %||% NA_real_), numeric(1)),
    stringsAsFactors = FALSE
  )
  tab$LR_from_previous <- NA_real_
  tab$df_diff <- NA_real_
  tab$p_value <- NA_real_
  for (i in 2:length(mods)) {
    ddev <- tab$global_deviance[i - 1] - tab$global_deviance[i]
    ddf <- tab$df[i] - tab$df[i - 1]
    tab$LR_from_previous[i] <- ddev
    tab$df_diff[i] <- ddf
    if (is.finite(ddev) && is.finite(ddf) && ddf > 0) tab$p_value[i] <- stats::pchisq(ddev, df = ddf, lower.tail = FALSE)
  }
  tab
}


# ===== gamlss_cld.R =====

#' Compact letter display for pairwise GAMLSS post-hoc results
#'
#' Creates compact letters from pairwise comparisons stored in a
#' `gamlss_posthoc` object. Compact-letter displays are intended only as a
#' presentation layer; effect estimates and uncertainty intervals should remain
#' the primary inferential output.
#'
#' @param x A `gamlss_posthoc` result created with `contrast = "pairwise"`.
#' @param alpha Numeric significance threshold strictly between 0 and 1.
#' @param p_adjust Optional method accepted by [stats::p.adjust()]. If `NULL`,
#'   an existing `p.value.adjusted` column is preferred; otherwise the stored
#'   `p.value` column is used as supplied.
#' @param Letters Character vector passed to `multcompView::multcompLetters()`.
#' @return The estimates data frame with an additional `.group` column.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("emmeans", quietly = TRUE) &&
#'     requireNamespace("multcompView", quietly = TRUE)) {
#'   set.seed(16)
#'   d <- data.frame(trt=factor(rep(c("A","B","C"), each=25)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=c(A=2,B=2.5,C=3.2)[d$trt], sigma=.20)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   ph <- gamlss_posthoc(fit, "trt", estimand="parameter", what="mu",
#'                        population="reference", contrast="pairwise",
#'                        engine="emmeans", uncertainty="delta", data=d)
#'   # 1. Default 5% CLD
#'   gamlss_cld(ph)
#'   # 2. More liberal display threshold
#'   gamlss_cld(ph, alpha=.10)
#'   # 3. Holm-adjust raw p-values before constructing letters
#'   gamlss_cld(ph, p_adjust="holm")
#' }
gamlss_cld <- function(x, alpha = 0.05, p_adjust = NULL,
                       Letters = c(letters, LETTERS, ".")) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(p_adjust) && !p_adjust %in% stats::p.adjust.methods) {
    stop("`p_adjust` must be NULL or a method in stats::p.adjust.methods.", call. = FALSE)
  }
  if (!inherits(x, "gamlss_posthoc")) stop("`x` must be a gamlss_posthoc result.", call. = FALSE)
  if (!identical(x$contrast_method, "pairwise") || is.null(x$contrasts)) {
    stop("Run gamlss_posthoc(..., contrast='pairwise') first.", call. = FALSE)
  }
  .gph_require("multcompView", "to construct compact letter displays.")
  est <- x$estimates; con <- x$contrasts
  if (length(x$specs) != 1L) stop("CLD currently requires exactly one focal variable.", call. = FALSE)
  s <- x$specs[1]; by <- x$by
  pcol <- if (is.null(p_adjust) && "p.value.adjusted" %in% names(con)) "p.value.adjusted" else "p.value"
  if (!pcol %in% names(con)) stop("No p-values are available. For the distribution engine, request bootstrap uncertainty.", call. = FALSE)

  est_key <- if (length(by)) interaction(est[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(est)))
  con_key <- if (length(by)) interaction(con[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(con)))
  est$.group <- NA_character_
  for (lev in unique(as.character(est_key))) {
    ei <- which(as.character(est_key) == lev)
    ci <- which(as.character(con_key) == lev)
    levels_here <- as.character(est[[s]][ei])
    # Map arbitrary level labels to safe tokens understood by multcompView.
    safe <- paste0("L", seq_along(levels_here))
    names(safe) <- levels_here
    p <- con[[pcol]][ci]
    if (!is.null(p_adjust) && pcol == "p.value") p <- stats::p.adjust(p, method = p_adjust)
    cn <- con$contrast[ci]
    mapped <- vapply(cn, function(z) {
      parts <- if (grepl(" vs ", z, fixed = TRUE)) strsplit(z, " vs ", fixed = TRUE)[[1]] else strsplit(z, " - ", fixed = TRUE)[[1]]
      if (length(parts) != 2L || !all(parts %in% names(safe))) return(NA_character_)
      paste0(safe[parts[1]], "-", safe[parts[2]])
    }, character(1))
    ok <- is.finite(p) & !is.na(mapped)
    if (!any(ok)) next
    pv <- p[ok]; names(pv) <- mapped[ok]
    L <- multcompView::multcompLetters(pv, threshold = alpha, Letters = Letters)$Letters
    reverse <- setNames(names(safe), safe)
    letters_by_level <- setNames(rep(NA_character_, length(levels_here)), levels_here)
    for (tok in names(L)) letters_by_level[reverse[tok]] <- L[tok]
    est$.group[ei] <- letters_by_level[as.character(est[[s]][ei])]
  }
  est
}


# ===== plot_data.R =====

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
          q <- vapply(ii, function(j) as.numeric(stats::quantile(D$positive[j], p, names = FALSE))[1L], numeric(1))
          stats::weighted.mean(q, w, na.rm = TRUE)
        }, numeric(1))
        lo <- 0; hi <- max(qs[2], .Machine$double.eps)
      } else {
        qs <- vapply(c(.001, .999), function(p) {
          q <- as.numeric(stats::quantile(D[ii], p, names = FALSE))
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
    hi <- max(vapply(ii, function(j) as.numeric(stats::quantile(D$positive[j], .9999, names = FALSE))[1L], numeric(1)), na.rm = TRUE)
    Fmix <- function(y) sum(w * (D$prob_zero[ii] + (1 - D$prob_zero[ii]) * as.numeric(distributions3::cdf(D$positive[ii], y))))
  } else {
    lo <- min(as.numeric(stats::quantile(D[ii], .0001, names = FALSE)), na.rm = TRUE)
    hi <- max(as.numeric(stats::quantile(D[ii], .9999, names = FALSE)), na.rm = TRUE)
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


# ===== plots.R =====

# ggplot2 visualization layer -------------------------------------------------

.gph_ggrequire <- function() .gph_require("ggplot2", "for visualization.")
.gph_aes <- function(x=NULL,y=NULL,colour=NULL,fill=NULL,group=NULL) {
  .gph_ggrequire()
  args <- list()
  if (!is.null(x)) args$x <- rlang::sym(x)
  if (!is.null(y)) args$y <- rlang::sym(y)
  if (!is.null(colour)) args$colour <- rlang::sym(colour)
  if (!is.null(fill)) args$fill <- rlang::sym(fill)
  if (!is.null(group)) args$group <- rlang::sym(group)
  do.call(ggplot2::aes, lapply(args, rlang::new_quosure, env=parent.frame()))
}

.gph_first_group <- function(d, exclude=character()) {
  cand <- setdiff(names(d), c(exclude,"estimate","SE","lower.CL","upper.CL","response","value","curve","prob","quantile","parameter","component","comparison","p.value","p.value.adjusted"))
  cand[vapply(d[cand], function(z) is.factor(z)||is.character(z), logical(1))][1L] %||% NULL
}

#' Scientific theme for GAMLSS figures
#' @param base_size Base font size.
#' @param base_family Font family.
#' @param grid Show major horizontal grid lines.
#' @return A complete ggplot2 theme.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   # theme_gamlss(): three typical uses
#'   p + theme_gamlss()
#'   p + theme_gamlss(base_size = 12)
#'   p + theme_gamlss(grid = FALSE)
#'   # journal variant
#'   p + theme_gamlss_journal()
#'   p + theme_gamlss_journal(base_size = 10)
#'   p + theme_gamlss_journal(grid = TRUE)
#'   # presentation variant
#'   p + theme_gamlss_presentation()
#'   p + theme_gamlss_presentation(base_size = 18)
#'   p + theme_gamlss_presentation(grid = FALSE)
#' }
theme_gamlss <- function(base_size=11, base_family="", grid=TRUE) {
  .gph_ggrequire()
  ggplot2::theme_minimal(base_size=base_size, base_family=base_family) +
    ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
                   panel.grid.major.x=ggplot2::element_blank(),
                   panel.grid.major.y=if (grid) ggplot2::element_line(linewidth=.25) else ggplot2::element_blank(),
                   legend.position="right", plot.title.position="plot",
                   strip.text=ggplot2::element_text(face="bold"),
                   axis.title=ggplot2::element_text(face="bold"))
}
#' @rdname theme_gamlss
#' @export
theme_gamlss_journal <- function(base_size=9, base_family="", grid=FALSE) theme_gamlss(base_size,base_family,grid)+ggplot2::theme(legend.position="top")
#' @rdname theme_gamlss
#' @export
theme_gamlss_presentation <- function(base_size=16, base_family="", grid=TRUE) theme_gamlss(base_size,base_family,grid)+ggplot2::theme(legend.position="bottom")

#' Plot one or more GAMLSS distributional parameters
#' @param object Fitted GAMLSS model.
#' @param x Predictor shown on the horizontal axis.
#' @param parameters Parameters to draw; `"all"` uses all detected parameters.
#' @param by Optional grouping variable(s).
#' @param n Grid size for numeric predictors.
#' @param scale `response` or `link`. Link scale is obtained directly from the model's parameter predictor.
#' @param uncertainty Inference layer passed to the data engine.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return A ggplot object.
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
#'   p1 <- plot_gamlss_parameters(fit, "dose", parameters = c("mu", "sigma"), data = d, n = 12)
#'   p2 <- plot_gamlss_parameters(fit, "dose", parameters = "mu", scale = "link", data = d, n = 12)
#'   p3 <- plot_gamlss_parameters(fit, "trt", parameters = "all", data = d)
#' }
plot_gamlss_parameters <- function(object,x,parameters="all",by=NULL,n=100L,scale=c("response","link"),uncertainty="none",data=NULL,...) {
  scale <- match.arg(scale)
  pars <- if (identical(parameters,"all")) .gph_model_parameters(object) else parameters
  d <- gamlss_plot_data(object,"parameters",x=x,by=by,parameters=pars,parameter_scale=scale,
                        n=n,uncertainty=uncertainty,data=data,...)
  num <- is.numeric(.gph_get_data(object,data)[[x]]); grp <- if(length(by)) by[1] else NULL
  p <- ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  if (num) p <- p + ggplot2::geom_line(linewidth=.8) else p <- p + ggplot2::geom_point(size=2.3)
  if (all(c("lower.CL","upper.CL")%in%names(d))) {
    if (num) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin=rlang::.data$lower.CL,ymax=rlang::.data$upper.CL,fill=if(!is.null(grp)) rlang::.data[[grp]] else NULL),alpha=.15,colour=NA)
    else p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=rlang::.data$lower.CL,ymax=rlang::.data$upper.CL),width=.08)
  }
  p + ggplot2::facet_wrap(~parameter,scales="free_y") + ggplot2::labs(x=x,y=NULL,title="Distributional parameters") + theme_gamlss()
}

#' Plot a declared GAMLSS estimand
#' @param object Fitted model.
#' @param x Predictor.
#' @param estimand Target estimand.
#' @param what Parameter when `estimand="parameter"`.
#' @param by Optional grouping variable(s).
#' @param n Numeric grid size.
#' @param uncertainty Inference layer.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_estimand(fit, "trt", estimand = "mean", data = d)
#'   p2 <- plot_gamlss_estimand(fit, "dose", estimand = "variance", data = d, n = 12)
#'   p3 <- plot_gamlss_estimand(fit, "dose", estimand = "quantile", prob = .9, data = d, n = 12)
#' }
plot_gamlss_estimand <- function(object,x,estimand="mean",what="mu",by=NULL,n=100L,uncertainty="none",data=NULL,...) {
  d <- gamlss_plot_data(object,"estimand",x=x,by=by,estimand=estimand,what=what,n=n,uncertainty=uncertainty,data=data,...)
  num <- is.numeric(.gph_get_data(object,data)[[x]]); grp <- if(length(by)) by[1] else NULL
  p <- ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  if (num) p <- p+ggplot2::geom_line(linewidth=.85) else p <- p+ggplot2::geom_point(size=2.4)
  if (all(c("lower.CL","upper.CL")%in%names(d))) {
    if(num) p <- p+ggplot2::geom_ribbon(ggplot2::aes(ymin=rlang::.data$lower.CL,ymax=rlang::.data$upper.CL,fill=if(!is.null(grp)) rlang::.data[[grp]] else NULL),alpha=.15,colour=NA)
    else p <- p+ggplot2::geom_errorbar(ggplot2::aes(ymin=rlang::.data$lower.CL,ymax=rlang::.data$upper.CL),width=.08)
  }
  p+ggplot2::labs(x=x,y=estimand,title=paste("GAMLSS",estimand))+theme_gamlss()
}

#' Forest/estimation plot for post-hoc contrasts
#' @param object `gamlss_posthoc` object or fitted model.
#' @param x Focal factor when `object` is a model.
#' @param null Optional null value; inferred from comparison when omitted.
#' @param show_p Show adjusted p-values as labels when available.
#' @param style `interval` for estimates and confidence intervals or `distribution` for retained bootstrap draws with optional `ggdist`.
#' @param ... Arguments forwarded when a model is supplied.
#' @return ggplot.
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
#'   r1 <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   p1 <- plot_gamlss_contrasts(r1)
#'   r2 <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "reference", comparison = "percent_change", uncertainty = "none", data = d)
#'   p2 <- plot_gamlss_contrasts(r2)
#'   p3 <- plot_gamlss_contrasts(r1, show_p = FALSE, style = "interval")
#' }
plot_gamlss_contrasts <- function(object,x=NULL,null=NULL,show_p=FALSE,style=c("interval","distribution"),...) {
  style <- match.arg(style)
  z <- if(inherits(object,"gamlss_posthoc")) object else gamlss_posthoc(object,specs=x,contrast="pairwise",...)
  d <- z$contrasts; if(is.null(d)||!nrow(d)) stop("No contrasts available.",call.=FALSE)
  comp <- z$comparison %||% (d$comparison[1] %||% "difference"); if(is.null(null)) null <- .gph_comparison_null(comp)
  d$.label <- d$contrast %||% seq_len(nrow(d))
  draws <- attr(d, "draws")
  if (style == "distribution" && !is.null(draws) && requireNamespace("ggdist", quietly = TRUE)) {
    dl <- do.call(rbind, lapply(seq_len(ncol(draws)), function(j) data.frame(.label=d$.label[j], draw=draws[,j])))
    p <- ggplot2::ggplot(dl, ggplot2::aes(x=rlang::.data$draw, y=rlang::.data$.label)) +
      ggdist::stat_halfeye(.width=c(.5,.8,.95)) + ggplot2::geom_vline(xintercept=null,linetype=2)
  } else {
    if (style == "distribution" && (is.null(draws) || !requireNamespace("ggdist", quietly = TRUE))) warning("Distribution style requires retained draws and package 'ggdist'; using interval style.", call.=FALSE)
    p <- ggplot2::ggplot(d,ggplot2::aes(x=rlang::.data$estimate,y=stats::reorder(rlang::.data$.label,rlang::.data$estimate)))+
      ggplot2::geom_vline(xintercept=null,linetype=2)+ggplot2::geom_point(size=2.4)
    if(all(c("lower.CL","upper.CL")%in%names(d))) p <- p+ggplot2::geom_errorbar(ggplot2::aes(xmin=rlang::.data$lower.CL,xmax=rlang::.data$upper.CL),orientation="y",width=.15)
  }
  if(show_p) { pc <- if("p.value.adjusted"%in%names(d)) "p.value.adjusted" else if("p.value"%in%names(d)) "p.value" else NULL; if(!is.null(pc)) { d$.ptxt <- paste0("p=",formatC(d[[pc]],digits=3,format="fg")); p <- p+ggplot2::geom_text(data=d,ggplot2::aes(label=rlang::.data$.ptxt),hjust=-.1,size=3) } }
  p+ggplot2::labs(x=.gph_comparison_label(comp),y=NULL,title="Post-hoc contrasts")+theme_gamlss()
}

#' Plot standardized predictive distributions
#' @param object Fitted model.
#' @param x Grouping/predictor variable.
#' @param by Optional conditioning variables.
#' @param type Density, CDF, or survival curve.
#' @param at,newdata,population,weights Prediction standardization controls.
#' @param n,n_response Grid sizes.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_distribution(fit, "trt", type = "density", data = d, n_response = 40)
#'   p2 <- plot_gamlss_distribution(fit, "trt", type = "cdf", data = d, n_response = 40)
#'   p3 <- plot_gamlss_distribution(fit, "dose", at = list(dose = c(0, 60, 120)), type = "survival", data = d, n_response = 40)
#' }
plot_gamlss_distribution <- function(object,x,by=NULL,type=c("density","cdf","survival"),at=list(),newdata=NULL,population="observed",weights=NULL,n=100L,n_response=200L,data=NULL,...) {
  type<-match.arg(type); d<-gamlss_plot_data(object,"distribution",x=x,by=by,curve=type,at=at,newdata=newdata,population=population,weights=weights,n=n,n_response=n_response,data=data,...)
  gc <- if (".gph_label" %in% names(d)) ".gph_label" else .gph_first_group(d,exclude=c("response","value")); if(is.null(gc)&&x%in%names(d)) gc<-x
  p<-ggplot2::ggplot(d,.gph_aes("response","value",colour=gc,group=gc))+ggplot2::geom_line(linewidth=.85)
  if(.gph_is_zadj(object)&&"prob_zero"%in%names(d)&&type=="density") { spike<-unique(d[c(intersect(gc,names(d)),"prob_zero")]); if(nrow(spike)) p<-p+ggplot2::geom_segment(data=spike,ggplot2::aes(x=0,xend=0,y=0,yend=rlang::.data$prob_zero),inherit.aes=FALSE,linewidth=.8) }
  p+ggplot2::labs(x="Response",y=type,title=paste("Predictive",type))+theme_gamlss()
}

#' Plot marginal predictive quantiles as lines or fan bands
#' @param object Fitted model.
#' @param x Predictor.
#' @param probs Quantile probabilities.
#' @param by Optional group.
#' @param style `lines` or `fan`.
#' @param n Grid size.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_quantiles(fit, "dose", style = "fan", data = d, n = 12)
#'   p2 <- plot_gamlss_quantiles(fit, "dose", probs = c(.1, .5, .9), style = "lines", data = d, n = 12)
#'   p3 <- plot_gamlss_quantiles(fit, "dose", by = "trt", style = "fan", data = d, n = 12)
#' }
plot_gamlss_quantiles <- function(object,x,probs=c(.05,.1,.25,.5,.75,.9,.95),by=NULL,style=c("fan","lines"),n=100L,data=NULL,...) {
  style<-match.arg(style); d<-gamlss_plot_data(object,"quantiles",x=x,by=by,probs=sort(unique(probs)),n=n,data=data,...)
  grp<-if(length(by)) by[1] else NULL
  if(style=="lines") return(ggplot2::ggplot(d,.gph_aes(x,"quantile",colour=grp,group=grp))+ggplot2::geom_line(ggplot2::aes(linetype=factor(rlang::.data$prob)),linewidth=.75)+ggplot2::labs(x=x,y="Quantile",linetype="p",title="Predictive quantiles")+theme_gamlss())
  # Build symmetric intervals around median when matching probabilities exist.
  lower<-sort(probs[probs<.5]); upper<-sort(probs[probs>.5],decreasing=TRUE); k<-min(length(lower),length(upper)); if(k<1L) stop("Fan style requires probabilities below and above 0.5.",call.=FALSE)
  med<-d[which.min(abs(d$prob-.5)),,drop=FALSE]; bands<-list()
  keys<-setdiff(names(d),c("prob","quantile")); basekeys<-keys
  for(i in seq_len(k)) { lo<-d[abs(d$prob-lower[i])<1e-12,,drop=FALSE]; hi<-d[abs(d$prob-upper[i])<1e-12,,drop=FALSE]; m<-merge(lo,hi,by=basekeys,suffixes=c(".lo",".hi")); m$.width<-upper[i]-lower[i]; bands[[i]]<-m }
  b<-do.call(rbind,bands); p<-ggplot2::ggplot()
  if(nrow(b)) p<-p+ggplot2::geom_ribbon(data=b,ggplot2::aes(x=rlang::.data[[x]],ymin=rlang::.data$quantile.lo,ymax=rlang::.data$quantile.hi,group=if(!is.null(grp)) interaction(rlang::.data[[grp]],rlang::.data$.width) else rlang::.data$.width,fill=factor(rlang::.data$.width)),alpha=.18,colour=NA)
  medall<-d[abs(d$prob-.5)==min(abs(d$prob-.5)),,drop=FALSE]; p+ggplot2::geom_line(data=medall,.gph_aes(x,"quantile",colour=grp,group=grp),linewidth=.9)+ggplot2::labs(x=x,y="Response quantiles",fill="Central width",title="Predictive quantile fan")+theme_gamlss()
}

#' Decompose zero-adjusted predictions
#' @param object Fitted `gamlssZadj` model.
#' @param x Predictor/group.
#' @param by Optional conditioning variables.
#' @param style `facets` or `normalized`.
#' @param n Grid size.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("gamlss.inf", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(312)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 45)), dose = rep(seq(0, 80, length.out = 9), 10))
#'   mu <- exp(0.2 + 0.012*d$dose + 0.25*(d$trt == "B"))
#'   h <- plogis(0.2 - 0.7*(d$trt == "B") - 0.01*d$dose)
#'   yp <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.35)
#'   d$y <- ifelse(stats::rbinom(nrow(d), 1, h) == 1, 0, yp)
#'   fit <- gamlss.inf::gamlssZadj(y = y, mu.formula = ~ trt + dose,
#'                                 sigma.formula = ~ 1,
#'                                 xi0.formula = ~ trt + dose,
#'                                 family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_zero_adjusted(fit, "trt", data = d)
#'   p2 <- plot_gamlss_zero_adjusted(fit, "dose", style = "facets", data = d, n = 12)
#'   p3 <- plot_gamlss_zero_adjusted(fit, "dose", by = "trt", style = "normalized", data = d, n = 12)
#' }
plot_gamlss_zero_adjusted <- function(object,x,by=NULL,style=c("facets","normalized"),n=100L,data=NULL,...) {
  style<-match.arg(style); d<-gamlss_plot_data(object,"zero_adjusted",x=x,by=by,n=n,data=data,...); grp<-if(length(by)) by[1] else NULL
  if(style=="normalized") d$plot_value<-ave(d$estimate,d$component,FUN=function(z) z/max(abs(z),na.rm=TRUE)) else d$plot_value<-d$estimate
  num<-is.numeric(.gph_get_data(object,data)[[x]]); p<-ggplot2::ggplot(d,.gph_aes(x,"plot_value",colour=grp,group=grp))
  if(num) p<-p+ggplot2::geom_line(linewidth=.85) else p<-p+ggplot2::geom_point(size=2.3)
  p+ggplot2::facet_wrap(~component,scales=if(style=="facets")"free_y" else "fixed")+ggplot2::labs(x=x,y=if(style=="normalized")"Normalized value" else "Estimate",title="Zero-adjusted decomposition")+theme_gamlss()
}

#' Plot a quantitative GAMLSS trend with confidence bands
#' @param object Fitted model or `gamlss_trend` object.
#' @param x Predictor when a model is supplied.
#' @param by Optional group.
#' @param band Pointwise or simultaneous band when available.
#' @param intervals Optional interval widths for ggdist draws when retained.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_trend(fit, "dose", data = d, n = 15)
#'   p2 <- plot_gamlss_trend(fit, "dose", by = "trt", data = d, n = 15)
#'   tr <- gamlss_trend(fit, "dose", n = 15, uncertainty = "none", data = d)
#'   p3 <- plot_gamlss_trend(tr)
#' }
plot_gamlss_trend <- function(object,x=NULL,by=NULL,band=c("pointwise","simultaneous"),intervals=c(.5,.8,.95),data=NULL,...) {
  band<-match.arg(band); tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="curve",band=band,data=data,...); d<-tr$values; x<-tr$x; grp<-if(length(tr$by)) tr$by[1] else NULL
  draws <- tr$source$draws %||% NULL
  if (!is.null(draws) && requireNamespace("ggdist", quietly=TRUE) && nrow(draws) > 1L) {
    dl <- do.call(rbind, lapply(seq_len(nrow(draws)), function(i) { z <- d; z$.draw <- i; z$.value <- as.numeric(draws[i,]); z }))
    p <- ggplot2::ggplot(dl, .gph_aes(x, ".value", colour=grp, group=grp)) +
      ggdist::stat_lineribbon(ggplot2::aes(group=if(!is.null(grp)) rlang::.data[[grp]] else 1), .width=intervals, alpha=.15)
  } else p<-ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  lo<-if(band=="simultaneous"&&"simultaneous_lower"%in%names(d))"simultaneous_lower" else if("lower.CL"%in%names(d))"lower.CL" else NULL
  hi<-if(band=="simultaneous"&&"simultaneous_upper"%in%names(d))"simultaneous_upper" else if("upper.CL"%in%names(d))"upper.CL" else NULL
  if (is.null(draws) || !requireNamespace("ggdist", quietly=TRUE)) {
    if(!is.null(lo)&&!is.null(hi)) p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=rlang::.data[[lo]],ymax=rlang::.data[[hi]],fill=if(!is.null(grp)) rlang::.data[[grp]] else NULL),alpha=.16,colour=NA)
    p<-p+ggplot2::geom_line(linewidth=.9)
  }
  p+ggplot2::labs(x=x,y=tr$estimand,title="GAMLSS trend")+theme_gamlss()
}

#' Plot first or second derivative of a GAMLSS trend
#' @param object Fitted model or derivative `gamlss_trend` object.
#' @param x Predictor when model supplied.
#' @param order Derivative order, 1 or 2.
#' @param by Optional group.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_derivative(fit, "dose", order = 1, data = d, n = 15)
#'   p2 <- plot_gamlss_derivative(fit, "dose", order = 2, data = d, n = 15)
#'   p3 <- plot_gamlss_derivative(fit, "dose", order = 1, by = "trt", data = d, n = 15)
#' }
plot_gamlss_derivative <- function(object,x=NULL,order=1L,by=NULL,data=NULL,...) {
  tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="derivative",derivative_order=order,data=data,...); d<-tr$values; x<-tr$x; ycol<-paste0("derivative",order); grp<-if(length(tr$by)) tr$by[1] else NULL
  p<-ggplot2::ggplot(d,.gph_aes(x,ycol,colour=grp,group=grp))+ggplot2::geom_hline(yintercept=0,linetype=2)
  lo<-paste0(ycol,"_lower"); hi<-paste0(ycol,"_upper"); if(all(c(lo,hi)%in%names(d))) p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=rlang::.data[[lo]],ymax=rlang::.data[[hi]],fill=if(!is.null(grp)) rlang::.data[[grp]] else NULL),alpha=.16,colour=NA)
  p+ggplot2::geom_line(linewidth=.85)+ggplot2::labs(x=x,y=paste0("d",order,"/d",x,order),title=paste("Derivative",order))+theme_gamlss()
}

#' Plot a maximum or minimum and its uncertainty
#' @param object Fitted model or optimum `gamlss_trend` object.
#' @param x Predictor when model supplied.
#' @param optimum Maximum or minimum.
#' @param by Optional group.
#' @param show_distribution If bootstrap optimum draws are available, use them in a distributional display.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_optimum(fit, "dose", optimum = "maximum", show_distribution = FALSE, data = d, n = 21)
#'   p2 <- plot_gamlss_optimum(fit, "dose", optimum = "minimum", show_distribution = FALSE, data = d, n = 21)
#'   p3 <- plot_gamlss_optimum(fit, "dose", by = "trt", optimum = "maximum", show_distribution = FALSE, data = d, n = 21)
#' }
plot_gamlss_optimum <- function(object,x=NULL,optimum=c("maximum","minimum"),by=NULL,show_distribution=TRUE,data=NULL,...) {
  optimum<-match.arg(optimum); tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="optimum",optimum=optimum,data=data,...); d<-tr$special_points; if(is.null(d)||!nrow(d)) stop("No optimum detected.",call.=FALSE); x<-tr$x; grp<-if(length(tr$by)) tr$by[1] else NULL
  d$.grp<-if(!is.null(grp)) as.character(d[[grp]]) else "All"
  odraws <- attr(d, "optimum_draws")
  if (isTRUE(show_distribution) && !is.null(odraws) && requireNamespace("ggdist", quietly=TRUE)) {
    dl <- do.call(rbind, lapply(seq_along(odraws), function(j) data.frame(.grp=d$.grp[j], location=as.numeric(odraws[[j]]))))
    p <- ggplot2::ggplot(dl, ggplot2::aes(x=rlang::.data$location,y=rlang::.data$.grp)) +
      ggdist::stat_halfeye(.width=c(.5,.8,.95))
  } else {
    if (isTRUE(show_distribution) && !is.null(odraws) && !requireNamespace("ggdist", quietly=TRUE))
      warning("Package 'ggdist' is not installed; showing interval representation.", call.=FALSE)
    p<-ggplot2::ggplot(d,ggplot2::aes(x=rlang::.data[[x]],y=rlang::.data$.grp))+ggplot2::geom_point(size=2.5)
    if(all(c("x_lower","x_upper")%in%names(d))) p<-p+ggplot2::geom_errorbar(ggplot2::aes(xmin=rlang::.data$x_lower,xmax=rlang::.data$x_upper),orientation="y",width=.15)
  }
  p+ggplot2::labs(x=paste("Location of",optimum),y=NULL,title=paste("Estimated",optimum))+theme_gamlss()
}

#' Plot a two-predictor estimand surface
#' @param object Fitted model.
#' @param x,y Numeric predictors.
#' @param estimand,what Target estimand.
#' @param type Heatmap, contour, or both.
#' @param n Approximate grid density.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(311)
#'   d <- expand.grid(dose = seq(0, 100, length.out = 8), salinity = seq(0, 3, length.out = 6))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.5 + 0.01*d$dose - 0.18*d$salinity), sigma = 0.25)
#'   fit <- gamlss::gamlss(y ~ dose * salinity, family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_surface(fit, "dose", "salinity", type = "heatmap", data = d, n = 16)
#'   p2 <- plot_gamlss_surface(fit, "dose", "salinity", type = "contour", data = d, n = 16)
#'   p3 <- plot_gamlss_surface(fit, "dose", "salinity", estimand = "variance", type = "both", data = d, n = 16)
#' }
plot_gamlss_surface <- function(object,x,y,estimand="mean",what="mu",type=c("heatmap","contour","both"),n=45L,data=NULL,...) {
  type<-match.arg(type); d<-gamlss_plot_data(object,"surface",x=x,y=y,estimand=estimand,what=what,n=n,data=data,...); p<-ggplot2::ggplot(d,ggplot2::aes(x=rlang::.data[[x]],y=rlang::.data[[y]]))
  if(type%in%c("heatmap","both")) p<-p+ggplot2::geom_raster(ggplot2::aes(fill=rlang::.data$estimate),interpolate=TRUE)
  if(type%in%c("contour","both")) p<-p+ggplot2::geom_contour(ggplot2::aes(z=rlang::.data$estimate),colour="black",linewidth=.35)
  p+ggplot2::labs(x=x,y=y,fill=estimand,title="GAMLSS estimand surface")+theme_gamlss()
}

#' Plot observations with a GAMLSS central curve and predictive quantiles
#' @param object Fitted model.
#' @param x Predictor.
#' @param response Optional response name; inferred from model when omitted.
#' @param by Optional group.
#' @param probs Lower, median, upper predictive probabilities.
#' @param n Grid size.
#' @param jitter Jitter observations when x is discrete.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
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
#'   p1 <- plot_gamlss_fit(fit, "dose", response = "y", data = d, n = 15)
#'   p2 <- plot_gamlss_fit(fit, "dose", response = "y", by = "trt", data = d, n = 15)
#'   p3 <- plot_gamlss_fit(fit, "trt", response = "y", jitter = TRUE, data = d)
#' }
plot_gamlss_fit <- function(object,x,response=NULL,by=NULL,probs=c(.05,.5,.95),n=100L,jitter=FALSE,data=NULL,...) {
  data<-.gph_get_data(object,data); if(is.null(response)) { f<-.gph_get_formula(object,.gph_model_parameters(object)[1]); response<-all.vars(f)[1] }
  q<-gamlss_plot_data(object,"quantiles",x=x,by=by,probs=probs,n=n,data=data,...); medp<-probs[which.min(abs(probs-.5))]; med<-q[abs(q$prob-medp)<1e-10,,drop=FALSE]; lo<-q[abs(q$prob-min(probs))<1e-10,,drop=FALSE]; hi<-q[abs(q$prob-max(probs))<1e-10,,drop=FALSE]; keys<-setdiff(names(q),c("prob","quantile")); band<-merge(lo,hi,by=keys,suffixes=c(".lo",".hi")); grp<-if(length(by)) by[1] else NULL
  p<-ggplot2::ggplot(data,.gph_aes(x,response,colour=grp)); p<-if(jitter) p+ggplot2::geom_jitter(alpha=.35,width=.08,height=0) else p+ggplot2::geom_point(alpha=.35)
  if(is.numeric(data[[x]])) {
    p<-p+ggplot2::geom_ribbon(data=band,ggplot2::aes(x=rlang::.data[[x]],ymin=rlang::.data$quantile.lo,ymax=rlang::.data$quantile.hi,fill=if(!is.null(grp)) rlang::.data[[grp]] else NULL,group=if(!is.null(grp)) rlang::.data[[grp]] else 1),inherit.aes=FALSE,alpha=.12)+ggplot2::geom_line(data=med,.gph_aes(x,"quantile",colour=grp,group=grp),linewidth=.9)
  } else {
    # For discrete predictors, show predictive median and interval at each level.
    p<-p+ggplot2::geom_errorbar(data=band,ggplot2::aes(x=rlang::.data[[x]],ymin=rlang::.data$quantile.lo,ymax=rlang::.data$quantile.hi,colour=if(!is.null(grp)) rlang::.data[[grp]] else NULL),inherit.aes=FALSE,width=.12)+
      ggplot2::geom_point(data=med,ggplot2::aes(x=rlang::.data[[x]],y=rlang::.data$quantile,colour=if(!is.null(grp)) rlang::.data[[grp]] else NULL),inherit.aes=FALSE,size=2.5)
  }
  p+ggplot2::labs(x=x,y=response,title="Observed data and predictive distribution")+theme_gamlss()
}

#' GAMLSS diagnostics in ggplot2
#' @param object Fitted model.
#' @param type One of residual_fitted, qq, worm, density, pit, rootogram, or all.
#' @param bins Number of bins.
#' @param nsim Simulations for rootogram.
#' @param engine Diagnostic engine; currently `native`, implemented with public `ggplot2` APIs.
#' @param data Original data.
#' @return A ggplot, a patchwork object when available, or a named list of plots for `type="all"`.
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
#'   p1 <- plot_gamlss_diagnostics(fit, "qq", engine = "native", data = d)
#'   p2 <- plot_gamlss_diagnostics(fit, "worm", engine = "native", data = d)
#'   p3 <- plot_gamlss_diagnostics(fit, "density", engine = "native", data = d)
#' }
plot_gamlss_diagnostics <- function(object,type=c("residual_fitted","qq","worm","density","pit","rootogram","all"),bins=20L,nsim=100L,engine=c("native"),data=NULL) {
  type<-match.arg(type); engine<-match.arg(engine); .gph_ggrequire(); data<-.gph_get_data(object,data)
  r<-try(stats::residuals(object,type="z-scores"),silent=TRUE); if(inherits(r,"try-error")) r<-stats::residuals(object); r<-as.numeric(r); fitted<-try(stats::fitted(object),silent=TRUE); if(inherits(fitted,"try-error")) fitted<-seq_along(r); fitted<-as.numeric(fitted)
  one<-function(tp){
    if(tp=="residual_fitted") return(ggplot2::ggplot(data.frame(fitted=fitted,resid=r),ggplot2::aes(rlang::.data$fitted,rlang::.data$resid))+ggplot2::geom_hline(yintercept=0,linetype=2)+ggplot2::geom_point(alpha=.5)+ggplot2::geom_smooth(se=FALSE,method="loess",formula=y~x)+ggplot2::labs(x="Fitted",y="Quantile residual",title="Residuals vs fitted")+theme_gamlss())
    if(tp=="qq") { n<-sum(is.finite(r)); d<-data.frame(theoretical=stats::qnorm(stats::ppoints(n)),sample=sort(r[is.finite(r)])); return(ggplot2::ggplot(d,ggplot2::aes(rlang::.data$theoretical,rlang::.data$sample))+ggplot2::geom_abline(slope=1,intercept=0,linetype=2)+ggplot2::geom_point()+ggplot2::labs(x="Normal quantile",y="Residual quantile",title="QQ plot")+theme_gamlss()) }
    if(tp=="worm") { n<-sum(is.finite(r)); theo<-stats::qnorm(stats::ppoints(n)); samp<-sort(r[is.finite(r)]); d<-data.frame(theoretical=theo,deviation=samp-theo); return(ggplot2::ggplot(d,ggplot2::aes(rlang::.data$theoretical,rlang::.data$deviation))+ggplot2::geom_hline(yintercept=0,linetype=2)+ggplot2::geom_point(alpha=.5)+ggplot2::geom_smooth(se=FALSE,formula=y~stats::poly(x,2),method="lm")+ggplot2::labs(x="Normal quantile",y="Detrended residual",title="Worm-style detrended QQ")+theme_gamlss()) }
    if(tp=="density") return(ggplot2::ggplot(data.frame(resid=r),ggplot2::aes(rlang::.data$resid))+ggplot2::geom_density()+ggplot2::stat_function(fun=stats::dnorm,linetype=2)+ggplot2::labs(x="Quantile residual",y="Density",title="Residual density")+theme_gamlss())
    if(tp=="pit") { u<-stats::pnorm(r); return(ggplot2::ggplot(data.frame(pit=u),ggplot2::aes(rlang::.data$pit))+ggplot2::geom_histogram(bins=bins,boundary=0)+ggplot2::geom_hline(yintercept=length(u)/bins,linetype=2)+ggplot2::labs(x="PIT from quantile residual",y="Count",title="PIT histogram")+theme_gamlss()) }
    # rootogram for integer responses via simulation
    resp<-all.vars(.gph_get_formula(object,.gph_model_parameters(object)[1]))[1]; yobs<-data[[resp]]; if(!is.numeric(yobs)||any(abs(yobs-round(yobs))>1e-8,na.rm=TRUE)) stop("Rootogram is intended for discrete/integer responses.",call.=FALSE)
    sims<-replicate(as.integer(nsim),.gph_simulate_response(object,data)); br<-seq(min(yobs,na.rm=TRUE),max(yobs,na.rm=TRUE)); obs<-tabulate(match(yobs,br),nbins=length(br)); expc<-rowMeans(vapply(seq_len(ncol(sims)),function(j)tabulate(match(round(sims[,j]),br),nbins=length(br)),numeric(length(br)))); dd<-data.frame(value=br,observed=sqrt(obs),expected=sqrt(expc),diff=sqrt(obs)-sqrt(expc)); ggplot2::ggplot(dd,ggplot2::aes(rlang::.data$value,rlang::.data$diff))+ggplot2::geom_hline(yintercept=0)+ggplot2::geom_col()+ggplot2::labs(x=resp,y="sqrt(observed)-sqrt(expected)",title="Hanging rootogram")+theme_gamlss()
  }
  if(type!="all") return(one(type)); types<-c("residual_fitted","qq","worm","density","pit"); ps<-stats::setNames(lapply(types,one),types); if(requireNamespace("patchwork",quietly=TRUE)) return(Reduce(`+`,ps)+patchwork::plot_layout(ncol=2)); ps
}

#' Compare fitted GAMLSS models graphically
#' @param ... Named fitted GAMLSS models.
#' @param type `criteria` or `estimand`.
#' @param criterion Criterion drawn when `type='criteria'`: `AIC`, `global_deviance`, `df`, or `all`.
#' @param x Predictor for estimand comparison.
#' @param estimand,what Target estimand.
#' @param data Optional original data.
#' @return ggplot.
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
#'   fit2 <- gamlss::gamlss(y ~ trt + dose, family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "criteria")
#'   p2 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose", data = d)
#'   p3 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose", estimand = "variance", data = d)
#' }
plot_gamlss_compare <- function(...,type=c("criteria","estimand"),criterion=c("AIC","global_deviance","df","all"),x=NULL,estimand="mean",what="mu",data=NULL) {
  type<-match.arg(type); criterion<-match.arg(criterion); mods<-list(...); if(!length(mods)) stop("Supply fitted models.",call.=FALSE); nm<-names(mods); if(is.null(nm)||any(!nzchar(nm))) nm<-paste0("Model",seq_along(mods))
  if(type=="criteria") {
    d<-do.call(rbind,lapply(seq_along(mods),function(i){m<-mods[[i]]; data.frame(model=nm[i],AIC=tryCatch(stats::AIC(m),error=function(e)NA_real_),global_deviance=m$G.deviance%||%NA_real_,df=m$df.fit%||%NA_real_)}))
    if (criterion == "all") {
      long<-do.call(rbind,lapply(c("AIC","global_deviance","df"),function(z)data.frame(model=d$model,criterion=z,value=d[[z]])))
      return(ggplot2::ggplot(long,ggplot2::aes(rlang::.data$model,rlang::.data$value))+ggplot2::geom_col()+ggplot2::facet_wrap(~criterion,scales="free_y")+ggplot2::labs(x=NULL,y=NULL,title="Model comparison")+theme_gamlss())
    }
    return(ggplot2::ggplot(d,ggplot2::aes(x=rlang::.data$model,y=rlang::.data[[criterion]]))+ggplot2::geom_col()+ggplot2::labs(x=NULL,y=criterion,title="Model comparison")+theme_gamlss())
  }
  if(is.null(x)) stop("`x` is required for estimand comparison.",call.=FALSE); rows<-lapply(seq_along(mods),function(i){z<-gamlss_plot_data(mods[[i]],"estimand",x=x,estimand=estimand,what=what,data=data);z$model<-nm[i];z}); d<-do.call(rbind,rows); ggplot2::ggplot(d,ggplot2::aes(x=rlang::.data[[x]],y=rlang::.data$estimate,colour=rlang::.data$model,group=rlang::.data$model))+ggplot2::geom_line(linewidth=.85)+ggplot2::labs(x=x,y=estimand,title="Model estimand comparison")+theme_gamlss()
}

# S3 plot/autoplot ------------------------------------------------------------
#' Plot and autoplot methods for gamlssPosthoc result classes
#'
#' @description `plot()` prints and invisibly returns the corresponding
#' `ggplot2::autoplot()` result. `autoplot()` returns a complete, modifiable
#' `ggplot` object for post-hoc, trend, distribution-summary, and diagnostic-plan
#' objects owned by this package.
#' @name autoplot.gamlssPosthoc
#' @aliases plot.gamlss_posthoc plot.gamlss_trend
#'   plot.gamlss_distribution_summary plot.gamlss_posthoc_plan
#'   autoplot.gamlss_posthoc autoplot.gamlss_trend
#'   autoplot.gamlss_distribution_summary autoplot.gamlss_posthoc_plan
#' @param x Result object for `plot()` methods.
#' @param object Result object for `autoplot()` methods.
#' @param ... Additional graphical arguments forwarded to the specialized
#'   constructor when supported.
#' @param x_axis Optional predictor used as horizontal axis for a distribution
#'   summary; use the method argument `x` in direct calls.
#' @return `autoplot()` returns a `ggplot`; `plot()` prints it and returns it invisibly.
#' @examples
#' if (requireNamespace("gamlss", quietly=TRUE) &&
#'     requireNamespace("gamlss.dist", quietly=TRUE) &&
#'     requireNamespace("distributions3", quietly=TRUE)) {
#'   set.seed(331)
#'   d <- data.frame(dose=rep(seq(0,100,length.out=8),each=6),
#'                   trt=factor(rep(c("A","B"),length.out=48)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.4+.01*d$dose+.15*(d$trt=="B")), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt+dose, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   ph <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                        uncertainty="none",data=d)
#'   tr <- gamlss_trend(fit,"dose",n=15,uncertainty="none",data=d)
#'   ds <- gamlss_distribution_summary(fit,
#'     data.frame(dose=c(25,75),trt=factor(c("A","B"),levels=levels(d$trt))),data=d)
#'   ggplot2::autoplot(ph)
#'   ggplot2::autoplot(tr)
#'   ggplot2::autoplot(ds, x="dose")
#' }
NULL

#' @export
plot.gamlss_posthoc <- function(x,...) { p<-ggplot2::autoplot(x,...); print(p); invisible(p) }
#' @export
plot.gamlss_trend <- function(x,...) { p<-ggplot2::autoplot(x,...); print(p); invisible(p) }

#' @export
plot.gamlss_distribution_summary <- function(x, ...) { p <- ggplot2::autoplot(x, ...); print(p); invisible(p) }
#' @export
plot.gamlss_posthoc_plan <- function(x, ...) { p <- ggplot2::autoplot(x, ...); print(p); invisible(p) }
#' @export
autoplot.gamlss_posthoc <- function(object,...) { if(!is.null(object$contrasts)) plot_gamlss_contrasts(object,...) else { d<-object$estimates; x<-object$specs[1]; p<-ggplot2::ggplot(d,.gph_aes(x,"estimate"))+ggplot2::geom_point(size=2.3); if(all(c("lower.CL","upper.CL")%in%names(d)))p<-p+ggplot2::geom_errorbar(ggplot2::aes(ymin=rlang::.data$lower.CL,ymax=rlang::.data$upper.CL),width=.08); p+ggplot2::labs(x=x,y=object$estimand_info$target[1],title="GAMLSS post-hoc estimates")+theme_gamlss() } }
#' @export
autoplot.gamlss_trend <- function(object,...) { if(grepl("derivative",object$method)) plot_gamlss_derivative(object,...) else if(object$method=="optimum") plot_gamlss_optimum(object,...) else plot_gamlss_trend(object,...) }
#' @export
autoplot.gamlss_distribution_summary <- function(object,x=NULL,...) { d<-as.data.frame(object); if(is.null(x)) x<-names(d)[1]; qcols<-grep("^q",names(d),value=TRUE); long<-do.call(rbind,lapply(qcols,function(q)data.frame(d[x],stat=q,value=d[[q]]))); ggplot2::ggplot(long,ggplot2::aes(x=rlang::.data[[x]],y=rlang::.data$value,colour=rlang::.data$stat,group=rlang::.data$stat))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(x=x,y="Value",title="Distribution summary")+theme_gamlss() }
#' @export
autoplot.gamlss_posthoc_plan <- function(object,...) { d<-object$parameter_capabilities; if(is.null(d)||!nrow(d))stop("No parameter capability table.",call.=FALSE); cols<-intersect(c("emmeans","marginaleffects","distribution"),names(d)); long<-do.call(rbind,lapply(cols,function(z)data.frame(parameter=d$parameter,engine=z,available=as.logical(d[[z]])))); ggplot2::ggplot(long,ggplot2::aes(x=rlang::.data$engine,y=rlang::.data$parameter,fill=rlang::.data$available))+ggplot2::geom_tile()+ggplot2::labs(x=NULL,y=NULL,title="Post-hoc engine capability")+theme_gamlss() }


# ===== tidy_methods.R =====

# Interoperability -----------------------------------------------------------

#' Tidy a gamlssPosthoc result
#' @param x A `gamlss_posthoc` object.
#' @param component `estimates` or `contrasts`.
#' @param conf.int Keep confidence intervals when present.
#' @param ... Unused.
#' @return A tidy data frame.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   generics::tidy(result)
#'   generics::tidy(result, component="contrasts")
#'   generics::tidy(result, conf.int=FALSE)
#' }
tidy.gamlss_posthoc <- function(x,component=c("estimates","contrasts"),conf.int=TRUE,...) {
  component<-match.arg(component); d<-if(component=="estimates")x$estimates else x$contrasts
  if(is.null(d))return(data.frame()); d<-as.data.frame(d)
  if(!conf.int)d<-d[,setdiff(names(d),c("lower.CL","upper.CL")),drop=FALSE]
  attr(d,"estimand")<-x$estimand; attr(d,"component")<-component; d
}

#' Glance at a gamlssPosthoc result
#' @param x A `gamlss_posthoc` object.
#' @param ... Unused.
#' @return One-row data frame.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   generics::glance(result)
#'   as.data.frame(generics::glance(result))
#'   generics::glance(result)[, c("engine","estimand")]
#' }
glance.gamlss_posthoc <- function(x,...) data.frame(engine=x$engine,estimand=x$estimand,population=x$population,weighting=x$weighting,uncertainty=x$uncertainty_method,n_estimates=nrow(x$estimates),n_contrasts=if(is.null(x$contrasts))0L else nrow(x$contrasts),stringsAsFactors=FALSE)

#' Augment data with row-level fitted estimands
#' @param x A `gamlss_posthoc` object.
#' @param data Optional data; defaults to data retained by the result.
#' @param ... Unused.
#' @return Original data plus `.fitted_estimand` and, when possible, `.resid`.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   head(generics::augment(result))
#'   head(generics::augment(result, data=d), 3)
#'   summary(generics::augment(result)$.fitted_estimand)
#' }
augment.gamlss_posthoc <- function(x,data=NULL,...) {
  object<-x$model; data<-data%||%x$data
  if(is.null(object)||is.null(data))stop("The result does not retain model/data; supply `data` and use a 0.3.0 result.",call.=FALSE)
  out<-as.data.frame(data); out$.fitted_estimand<-.gph_predict_estimand(object,out,x$estimand,x$what,data=out)
  resp<-try(all.vars(.gph_get_formula(object,.gph_model_parameters(object)[1]))[1],silent=TRUE); if(!inherits(resp,"try-error")&&resp%in%names(out))out$.resid<-out[[resp]]-out$.fitted_estimand
  out
}

#' Extract parameters in the easystats style
#' @param model A `gamlss_posthoc` object.
#' @param ... Unused.
#' @return Data frame compatible with `parameters` conventions.
#' @exportS3Method parameters::model_parameters
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   if (requireNamespace("parameters", quietly=TRUE)) {
#'     parameters::model_parameters(result)
#'     as.data.frame(parameters::model_parameters(result))
#'     parameters::model_parameters(result)[, c("Parameter","Coefficient")]
#'   }
#' }
model_parameters.gamlss_posthoc <- function(model,...) {
  d<-model$contrasts%||%model$estimates; d<-as.data.frame(d)
  out<-data.frame(Parameter=if("contrast"%in%names(d))d$contrast else seq_len(nrow(d)),Coefficient=d$estimate,stringsAsFactors=FALSE)
  if("SE"%in%names(d))out$SE<-d$SE; if("lower.CL"%in%names(d))out$CI_low<-d$lower.CL; if("upper.CL"%in%names(d))out$CI_high<-d$upper.CL; if("p.value"%in%names(d))out$p<-d$p.value
  class(out)<-c("parameters_model",class(out)); out
}


# ===== export_report.R =====

# Export and reporting --------------------------------------------------------

.gph_report_model_info <- function(res) {
  m <- res$model %||% NULL
  if (is.null(m)) return(data.frame())
  data.frame(
    family = tryCatch(.gph_family_name(m), error = function(e) NA_character_),
    converged = if (!is.null(m$converged)) as.logical(m$converged)[1L] else NA,
    iterations = if (!is.null(m$iter)) as.numeric(m$iter)[1L] else if (!is.null(m$no.cyc)) as.numeric(m$no.cyc)[1L] else NA_real_,
    global_deviance = m$G.deviance %||% NA_real_,
    effective_df = m$df.fit %||% NA_real_,
    AIC = tryCatch(stats::AIC(m), error = function(e) NA_real_),
    stringsAsFactors = FALSE
  )
}

.gph_result_table <- function(x,component=c("estimates","contrasts")) {
  component<-match.arg(component); if(inherits(x,"gamlss_posthoc")) return(as.data.frame(if(component=="estimates")x$estimates else x$contrasts))
  if(inherits(x,"gamlss_trend"))return(as.data.frame(x$values)); as.data.frame(x)
}

#' Convert a gamlssPosthoc result to a flextable
#' @param x Result object or data frame.
#' @param component Estimates or contrasts.
#' @param digits Number of numeric digits.
#' @param ... Arguments forwarded to `flextable::flextable()`.
#' @return A flextable object.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   if (requireNamespace("flextable", quietly = TRUE)) {
#'     f1 <- as_flextable(result)
#'     f2 <- as_flextable(result, component = "contrasts")
#'     f3 <- as_flextable(result, digits = 2)
#'   }
#' }
as_flextable <- function(x,component=c("estimates","contrasts"),digits=3,...) {
  .gph_require("flextable","for Word-ready tables."); d<-.gph_result_table(x,match.arg(component)); num<-vapply(d,is.numeric,logical(1)); d[num]<-lapply(d[num],round,digits=digits); ft<-flextable::flextable(d,...); flextable::autofit(ft)
}

#' Export results to a Word document
#' @param x Result object.
#' @param file Output `.docx` path.
#' @param component Estimates or contrasts.
#' @param title Document title.
#' @param digits Number of digits.
#' @return Invisibly, the file path.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
#'     f1 <- export_to_word(result, tempfile(fileext = ".docx"))
#'     f2 <- export_to_word(result, tempfile(fileext = ".docx"), component = "contrasts")
#'     f3 <- export_to_word(result, tempfile(fileext = ".docx"), title = "GAMLSS results")
#'   }
#' }
export_to_word <- function(x,file,component=c("estimates","contrasts"),title="gamlssPosthoc results",digits=3) {
  .gph_require("officer","for Word export."); .gph_require("flextable","for Word export."); ft<-as_flextable(x,match.arg(component),digits); doc<-officer::read_docx(); doc<-officer::body_add_par(doc,title,style="heading 1"); doc<-flextable::body_add_flextable(doc,ft); print(doc,target=file); invisible(normalizePath(file,mustWork=FALSE))
}

#' Export results as a LaTeX table
#' @param x Result object.
#' @param file Optional output `.tex` path. If `NULL`, the LaTeX string is returned.
#' @param component Estimates or contrasts.
#' @param digits Number of digits.
#' @param caption Optional caption.
#' @return Character LaTeX table or output path invisibly when a file is written.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   t1 <- export_to_latex(result)
#'   t2 <- export_to_latex(result, component = "contrasts")
#'   f3 <- export_to_latex(result, tempfile(fileext = ".tex"), caption = "GAMLSS results")
#' }
export_to_latex <- function(x, file = NULL, component = c("estimates", "contrasts"),
                            digits = 3, caption = NULL) {
  d <- .gph_result_table(x, match.arg(component))
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], round, digits = digits)

  esc <- function(z) {
    z <- as.character(z)
    z <- gsub("\\\\", "\\\\textbackslash{}", z, fixed = TRUE)
    for (ch in c("_", "&", "#", "%", "$"))
      z <- gsub(ch, paste0("\\\\", ch), z, fixed = TRUE)
    z
  }

  cols <- names(d)
  align <- paste0("l", paste(rep("r", max(0L, ncol(d) - 1L)), collapse = ""))
  body <- if (nrow(d)) vapply(seq_len(nrow(d)), function(i) {
    vals <- vapply(unname(as.list(d[i, , drop = FALSE])), esc, character(1))
    paste0(paste(vals, collapse = " & "), " \\\\")
  }, character(1)) else character()

  lines <- c(
    "\\begin{table}[ht]",
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", esc(caption), "}") else NULL,
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(esc(cols), collapse = " & "), " \\\\"),
    "\\hline",
    body,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  tex <- paste(lines, collapse = "\n")
  if (is.null(file)) return(tex)
  writeLines(tex, file, useBytes = TRUE)
  invisible(normalizePath(file, mustWork = FALSE))
}

#' Generate an automatic HTML, Word, or Markdown report
#' @param object Fitted GAMLSS model or `gamlss_posthoc` result.
#' @param output `html`, `word`, or `md`.
#' @param file Optional destination file.
#' @param title Report title.
#' @param include_plots Include an automatic post-hoc figure when possible.
#' @param ... Arguments used to construct a post-hoc result when `object` is a model.
#' @return Path to the generated report.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   f1 <- generate_report(result, "md", tempfile(fileext = ".md"), include_plots = FALSE)
#'   f2 <- generate_report(result, "md", tempfile(fileext = ".md"), title = "Agronomic GAMLSS", include_plots = FALSE)
#'   f3 <- generate_report(fit, "md", tempfile(fileext = ".md"), include_plots = FALSE, specs = "trt", estimand = "mean", uncertainty = "none", data = d)
#' }
generate_report <- function(object, output = c("html", "word", "md"), file = NULL,
                            title = "gamlssPosthoc report", include_plots = TRUE, ...) {
  output <- match.arg(output)
  res <- if (inherits(object, "gamlss_posthoc")) object else gamlss_posthoc(object, ...)
  if (is.null(file)) file <- tempfile(fileext = switch(output, html = ".html", word = ".docx", md = ".md"))

  make_plot <- function() {
    if (!isTRUE(include_plots) || !requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    tryCatch(ggplot2::autoplot(res), error = function(e) NULL)
  }

  if (output == "md") {
    fig_path <- NULL
    p <- make_plot()
    if (!is.null(p)) {
      fig_path <- paste0(tools::file_path_sans_ext(file), "_figure.png")
      tryCatch(ggplot2::ggsave(fig_path, p, width = 7, height = 4.5, dpi = 180), error = function(e) fig_path <<- NULL)
    }
    est_txt <- paste(capture.output(print(res$estimates, row.names = FALSE)), collapse = "\n")
    con_txt <- if (is.null(res$contrasts)) "None" else paste(capture.output(print(res$contrasts, row.names = FALSE)), collapse = "\n")
    diag <- .gph_report_model_info(res)
    diag_txt <- if (!nrow(diag)) "Unavailable" else paste(capture.output(print(diag, row.names = FALSE)), collapse = "\n")
    lines <- c(
      paste0("# ", title), "",
      paste0("- Engine: `", res$engine, "`"),
      paste0("- Estimand: ", res$estimand_info$target[1]),
      paste0("- Population: `", res$population, "`"),
      paste0("- Weighting: `", res$weighting, "`"),
      paste0("- Uncertainty: `", res$uncertainty_method, "`"), "",
      "## Model diagnostics", "", "```text", diag_txt, "```", "",
      "## Estimates", "", "```text", est_txt, "```", "",
      "## Contrasts", "", "```text", con_txt, "```",
      if (!is.null(fig_path)) c("", "## Figure", "", paste0("![](", basename(fig_path), ")")) else NULL
    )
    writeLines(lines, file, useBytes = TRUE)
    return(normalizePath(file, mustWork = FALSE))
  }

  .gph_require("rmarkdown", "for HTML/Word report generation.")
  rmd <- tempfile(fileext = ".Rmd")
  fmt <- if (output == "html") "html_document" else "word_document"
  tmp <- tempfile(fileext = ".rds")
  saveRDS(res, tmp)
  tmp_norm <- gsub("\\\\", "/", tmp)
  rmd_lines <- c(
    "---", paste0('title: "', title, '"'), paste0("output: ", fmt), "---", "",
    "```{r setup, echo=FALSE, message=FALSE, warning=FALSE}",
    paste0("res <- readRDS('", tmp_norm, "')"),
    "```", "",
    "## Estimand", "", "```{r}", "res$estimand_info", "```", "",
    "## Model diagnostics", "", "```{r}", ".gph_report_model_info <- getFromNamespace('.gph_report_model_info', 'gamlssPosthoc')", ".gph_report_model_info(res)", "```", "",
    "## Estimates", "", "```{r}", "res$estimates", "```", "",
    "## Contrasts", "", "```{r}", "res$contrasts", "```"
  )
  if (isTRUE(include_plots)) {
    rmd_lines <- c(rmd_lines, "", "## Figure", "",
                   "```{r, fig.width=7, fig.height=4.5}",
                   "if (requireNamespace('ggplot2', quietly=TRUE)) print(ggplot2::autoplot(res))",
                   "```")
  }
  writeLines(rmd_lines, rmd, useBytes = TRUE)
  rmarkdown::render(rmd, output_file = normalizePath(file, mustWork = FALSE),
                    quiet = TRUE, envir = new.env(parent = globalenv()))
  normalizePath(file, mustWork = FALSE)
}


# ===== gamlss_engine_info.R =====

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

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

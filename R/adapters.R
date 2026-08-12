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

# `vcov.gamlss()` (via `gen.likelihood()`) and `predict.gamlss()` both resolve
# the fitted call's `data` argument *by name*, and the lookup only reaches the
# global environment.  A model fitted inside a function, an example, or a test
# therefore breaks the emmeans and marginaleffects engines.  When that name is
# not currently bound, expose the user-supplied data under it for the duration
# of the engine call and restore the workspace afterwards.

# `predict.gamlss()` evaluates the fitted call's `data` argument, so storing the
# data frame itself in the call makes the fit self-contained: the value is
# returned directly instead of being looked up by name.  The user's object is
# never modified; only the local copy handed to the engine is.
.gph_self_contained <- function(object, data) {
  if (!is.data.frame(data) || !is.list(object) || is.null(object$call)) return(object)
  object$call$data <- data
  object
}

# `vcov.gamlss()` goes through `gen.likelihood()`, which resolves the same
# argument *by name* and only searches the global environment.  Dropping `data`
# from the call makes `gen.likelihood()` use the components carried by the fit,
# which is numerically identical.  Returns NULL when no covariance is available.
.gph_safe_vcov <- function(object) {
  v <- try(stats::vcov(object), silent = TRUE)
  if (!inherits(v, "try-error") && is.matrix(v)) return(v)
  o2 <- object
  o2$call$data <- NULL
  v <- try(stats::vcov(o2), silent = TRUE)
  if (inherits(v, "try-error") || !is.matrix(v)) NULL else v
}

# emmeans builds a reference grid for a single distributional parameter, so it
# needs the covariance block of that parameter rather than the full joint
# matrix that `vcov.gamlss()` returns (blocks are stacked in parameter order).
.gph_param_vcov <- function(object, what) {
  v <- .gph_safe_vcov(object)
  if (is.null(v)) return(NULL)
  pars <- .gph_model_parameters(object)
  if (!length(pars) || !what %in% pars) return(NULL)
  sizes <- vapply(pars, function(p) {
    z <- try(stats::coef(object, what = p), silent = TRUE)
    if (inherits(z, "try-error")) NA_integer_ else length(z)
  }, integer(1))
  if (anyNA(sizes) || sum(sizes) != nrow(v)) {
    return(if (nrow(v) == length(try(stats::coef(object, what = what), silent = TRUE))) v else NULL)
  }
  k <- match(what, pars)
  start <- if (k == 1L) 0L else sum(sizes[seq_len(k - 1L)])
  idx <- seq.int(start + 1L, start + sizes[k])
  v[idx, idx, drop = FALSE]
}

# Internal bookkeeping columns (group ids, weights) must never reach
# predict.gamlss(), which indexes the model frame by names(newdata).
.gph_clean_newdata <- function(newdata, data = NULL) {
  if (is.null(newdata) || !is.data.frame(newdata)) return(newdata)
  keep <- !grepl("^\\.gph_", names(newdata))
  if (!is.null(data)) keep <- keep & names(newdata) %in% names(data)
  if (!any(keep)) return(newdata)
  newdata[, names(newdata)[keep], drop = FALSE]
}

.gph_predict_parameter <- function(object, newdata, what, data = NULL,
                                   type = c("response", "link")) {
  type <- match.arg(type)
  pars <- .gph_model_parameters(object)
  if (!what %in% pars) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(pars, collapse = ", "), ".", call. = FALSE)
  }
  newdata <- .gph_clean_newdata(newdata, data)
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

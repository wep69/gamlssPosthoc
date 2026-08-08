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

## `gamlss:::predict.gamlss()` subsets the fitting data with
## `data[match(names(newdata), names(data))]`, so any column present in the
## prediction grid but absent from the fitting data raises
## "undefined columns selected". The evaluation grid carries internal
## bookkeeping columns (`.gph_group_id`, `.gph_weight`), which must be removed
## before any prediction call reaches gamlss.
.gph_clean_newdata <- function(newdata, data = NULL) {
  if (is.null(newdata)) return(NULL)
  newdata <- as.data.frame(newdata)
  keep <- setdiff(names(newdata), grep("^\\.gph_", names(newdata), value = TRUE))
  if (!is.null(data)) keep <- intersect(keep, names(as.data.frame(data)))
  if (!length(keep)) return(newdata)
  newdata[, keep, drop = FALSE]
}

## `gamlss:::gen.likelihood()`, reached from `vcov.gamlss()` and therefore from
## every emmeans reference grid, re-resolves the fitting data by *name* with
## `get(as.character(object$call["data"]))`. That lookup only searches the
## gamlss namespace and the global environment, so it fails whenever the model
## was fitted inside a function, a test, or any local scope. When `object$call`
## carries no `data` element, `gen.likelihood()` skips the lookup entirely and
## reconstructs everything from the fitted object, giving identical results.
## The data itself is always supplied to emmeans explicitly via `data=`.
.gph_emmeans_object <- function(object) {
  if (is.null(object$call) || !"data" %in% names(object$call)) return(object)
  object$call$data <- NULL
  object
}

.gph_predict_parameter <- function(object, newdata, what, data = NULL) {
  pars <- .gph_model_parameters(object)
  if (!what %in% pars) {
    stop("Parameter '", what, "' is not available. Available parameters: ",
         paste(pars, collapse = ", "), ".", call. = FALSE)
  }
  newdata <- .gph_clean_newdata(newdata, data)
  if (.gph_is_zadj(object)) {
    return(as.numeric(stats::predict(object, parameter = what, newdata = newdata,
                                     type = "response", data = data)))
  }
  # Current CRAN gamlss exposes `what` for its distributional parameters.
  as.numeric(stats::predict(object, what = what, newdata = newdata,
                            type = "response", data = data))
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

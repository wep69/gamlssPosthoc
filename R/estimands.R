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

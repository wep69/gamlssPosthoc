# gamlssPosthoc 0.3.0

* Added a data-first `ggplot2` visualization layer with `gamlss_plot_data()`.
* Added multi-parameter, estimand, contrast, predictive-distribution, quantile-fan,
  zero-adjusted, trend, derivative, optimum, surface, fit, diagnostics, and model-comparison plots.
* Added link/response scales for distributional parameters and marginal-mixture quantiles.
* Added `autoplot()`/`plot()` methods for package result classes.
* Added optional `ggdist` displays for retained bootstrap uncertainty.
* Added `tidy()`, `glance()`, `augment()`, and `parameters::model_parameters()` methods.
* Added Word, LaTeX, flextable, and automatic report export with model diagnostics.
* Added `pkgdown` configuration, a two-page cheatsheet, technical glossary, benchmark script,
  advanced examples, and vignettes for visualization, advanced workflows, and missing data.
* Added visual regression specifications with `vdiffr` and expanded numerical/data-layer tests.
* Diagnostics are implemented natively with public `ggplot2` APIs. The archived
  `gamlss.ggplots` package is not a dependency of the CRAN-oriented source tree.

## Documentation

* Seven vignettes, all in English and all with executable code: `workflow`,
  `estimands-and-standardization`, `visualization`, `cookbook`,
  `case-study-agronomy`, `advanced-workflows` and `missing-data`. The last two
  previously contained no runnable code at all.
* Every exported function now carries three executable examples in its help page
  and appears at least three times across the vignettes.
* References moved to `vignettes/references.bib`, with every field transcribed
  from the Crossref API. The audit trail is in `REFERENCES_AUDIT.md`.
* Restored `URL` and `BugReports` in `DESCRIPTION`.

## Bug fixes

* `predict()` no longer receives the internal bookkeeping columns
  (`.gph_group_id`, `.gph_weight`), which previously made the whole graphics
  layer fail with "undefined columns selected".
* `prodist()` is resolved defensively, with a fallback that rebuilds the
  predictive distribution from the predicted parameters through the public
  `gamlss.dist` API. Current `gamlss` releases do not export it.
* Removed `names = FALSE` from `stats::quantile()` calls on `distributions3`
  objects, which those methods do not accept.
* Replaced `rlang::.data` by the imported `.data` pronoun inside `aes()`. The
  namespace-qualified form does not resolve against the data mask in ggplot2 4.0.
* `plot_gamlss_quantiles(style = "lines")` now groups by probability, so a single
  path no longer carries varying linetype and colour.
* `gamlss_cld()` and `autoplot()` for `gamlss_posthoc_plan` follow the current
  capability column names.
* `.gph_refine_extremum()` returns unnamed values.
* Models fitted inside a function no longer break the `emmeans` and
  `marginaleffects` engines. Both `vcov.gamlss()` and `predict.gamlss()` resolve
  the fitted data by name from the global environment; the package now supplies a
  self-contained copy of the fit and a pre-computed covariance block instead,
  without writing to the user's workspace.

# gamlssPosthoc 0.2.0

* Added a generic zero-adjusted distribution engine based on the positive-part
  `distributions3` distribution plus the point mass at zero. Means, variances,
  quantiles, exact zero probabilities, and parametric simulation are available
  for compatible `gamlssZadj` families beyond GA/GAF.
* Introduced an internal adapter layer that discovers parameter names, formulas,
  links, and smoothers dynamically. Conventional parameter names remain only at
  external interfaces which explicitly restrict support to them.
* Expanded `marginaleffects` routing with `avg_predictions()`,
  `avg_comparisons()`, `avg_slopes()`, and simulation-based inference. Scientific
  percent changes are transformed from ratios of marginal means for estimand
  consistency across engines.
* Added formal estimand metadata: target, mathematical definition, target
  population, weighting rule, and scale are retained in every result.
* Added layered uncertainty choices: delta, coefficient simulation where
  supported, refit bootstrap, or plug-in point estimation.
* Added scientific contrast scales: arithmetic difference, ratio, log-ratio,
  and percent change, separated from contrast geometry.
* Extended `gamlss_trend()` to first/second derivatives, turning points,
  locally refined optima, optional simultaneous bootstrap bands, and
  `marginaleffects` first-derivative routing.
* Added `gamlss_posthoc_plan()` as a full diagnostic of parameters, smoothers,
  optional dependencies, engine eligibility, and recommended inference route.
* Restricted automatic `emmeans` routing to explicitly requested reference
  populations so observed-population estimands are never silently changed.

# gamlssPosthoc 0.1.0

- Initial release candidate.
- Conservative automatic routing for GAMLSS post-hoc inference.
- Parameter-wise `emmeans` integration for supported parametric submodels.
- Optional `marginaleffects` average-prediction engine.
- Distribution-derived means, variances, quantiles, zero probabilities, and custom estimands.
- Refit bootstrap for uncertainty propagation.
- Zero-adjusted Gamma/GAF workflows.
- Curves and finite-difference derivatives for smoothers.
- Distinction between quantitative polynomial regression and factor polynomial contrasts.

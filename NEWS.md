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

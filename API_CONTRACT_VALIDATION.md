# API contract validation — gamlssPosthoc 0.2.0

Date: 2026-08-08

This document records the external API contracts used by the 0.2.0 refactor. It is intentionally separate from runtime test results.

## `gamlss.dist` distribution bridge

The positive component of generic zero-adjusted models is reconstructed through `gamlss.dist::GAMLSS()`. The implementation discovers the accepted distributional parameter names from `formals(gamlss.dist::GAMLSS)` instead of assuming a fixed `mu/sigma/nu/tau` vector internally.

Reference: <https://search.r-project.org/CRAN/refmans/gamlss.dist/html/GAMLSS.html>

Safeguard: when a local/custom family cannot be represented by this constructor, `positive_dist_fun` is required rather than silently assuming that `mu` is the mathematical mean.

## `gamlss.inf::gamlssZadj`

`gamlssZadj` adds a point mass at zero (`xi0`) to a positive continuous GAMLSS distribution. The package recovers formulas through the documented S3 method `formula(object, parameter = ...)` and uses the model's prediction interface for parameters.

References:

- <https://rdrr.io/cran/gamlss.inf/man/gamlssZadj.html>
- <https://rdrr.io/cran/gamlss.inf/man/predict.gamlssZadj.html>

Safeguard: exact `P(Y=0)` is treated as probability mass, not as `F(0)` of a continuous distribution.

## `emmeans`

The current `emmeans` model-support documentation lists `gamlss` with `what = "mu"`, `"sigma"`, `"nu"`, or `"tau"` and notes that the selected component cannot contain smoothers such as `pb()`.

Reference: <https://rvlenth.github.io/emmeans/articles/models.html>

Safeguards in `gamlssPosthoc`:

- automatic `emmeans` routing is limited to a simple ordinary distributional parameter;
- the selected component must be free of a detected smoother;
- the target population must explicitly be `population = "reference"`;
- arithmetic response-scale differences are requested after response-scale regridding;
- derived distributional means/quantiles/zero-adjusted estimands are never delegated to this adapter.

## `marginaleffects`

The package uses `avg_predictions()` for parameter-wise standardization, `avg_comparisons()` when the requested contrast geometry is directly supported, and `avg_slopes()` for first derivatives of ordinary distributional parameters. `inferences(method = "simulation")` is used only when explicitly requested and supported.

References:

- <https://marginaleffects.com/man/r/comparisons.html>
- <https://marginaleffects.com/man/r/predictions.html>
- <https://marginaleffects.com/man/r/slopes.html>
- <https://marginaleffects.com/man/r/inferences.html>

Direction safeguard: `marginaleffects`' ordinary `pairwise` shortcut uses the later-minus-earlier orientation. The adapter uses `revpairwise` so that pairwise contrasts agree with the package's earlier-minus-later convention and with the default `emmeans` pairwise orientation.

Scientific-scale safeguard: percent change is derived from the ratio of standardized marginal means, `100 * (ratio - 1)`, instead of averaging row-specific percentage changes.

Weighted nonlinear safeguard: when user-supplied numeric weights are combined with ratio/log-ratio/percent-change contrasts, the adapter does not rely on a direct covariance shortcut whose estimand could be ambiguous. It uses standardized group predictions and local contrast construction for point estimates, or falls back to the distribution/refit-bootstrap route when covariance-based inference is requested.

## Uncertainty contract

The package separates the requested estimand from the available uncertainty engine.

- `delta`: external covariance-based engine where supported;
- `simulation`: coefficient simulation through the external engine where supported;
- `bootstrap`: full refit bootstrap for derived/nonlinear estimands;
- `none`: plug-in point estimates.

For distribution-derived targets under classic `gamlss`, no unsupported joint coefficient covariance is fabricated. Requests for delta/simulation are rerouted to refit bootstrap with an explicit warning.

## Runtime status

These contracts have been checked against current public documentation/source interfaces. Actual R runtime integration tests are provided in `tests/testthat/` but could not be executed in this container because no R executable is available and the container cannot fetch one from CRAN.

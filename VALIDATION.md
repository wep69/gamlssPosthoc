# Validation status — gamlssPosthoc 0.2.0

Date: 2026-08-08

The 0.2.0 refactor was subjected to static, mathematical, API-contract, documentation, and package-layout validation in this container. Runtime validation in R remains pending because no R executable is installed and the container cannot download CRAN resources. No claim is made that `R CMD check` has run here.

## Static validation: PASS

`tools/static_validate.py` checked:

- 7 exported public functions and their definitions;
- exact R-formals versus Rd `usage` for all 7 public functions;
- Rd argument-item coverage;
- Roxygen `@param` coverage;
- delimiter and quoted-string balance heuristics across 12 source files, 3 test-entry/source files, and 14 complete example scripts;
- duplicate R function definitions;
- S3 registration targets;
- line endings and path portability;
- 20 critical architectural invariants introduced by the 0.2.0 refactor.

Result: **0 static failures**.

The 20 verified invariants include dynamic parameter discovery, documented `gamlssZadj` S3 formula access, generic positive-distribution construction, exact zero mass, population-aware `emmeans` routing, `marginaleffects` prediction/comparison/slope integration, harmonized pairwise direction, explicit multiplicity handling, marginal-mean percent change, scientific contrast scales, estimand metadata, layered uncertainty, higher derivatives, turning points, local optimum refinement, and the diagnostic planner.

See `STATIC_VALIDATION.json`.

## Independent mathematical validation: PASS

`tools/mathematical_validation.py` performed **19** independent numerical/Monte Carlo checks:

- zero-adjusted Gamma mean and variance;
- zero-adjusted Gamma quantiles;
- zero-adjusted Lognormal mean and variance;
- zero-adjusted Lognormal quantiles;
- difference, ratio, log-ratio, and percent-change transformations;
- first and second numerical derivatives;
- quadratic turning-point interpolation;
- local quadratic refinement of an internal optimum.

Result: **19/19 PASS**.

See `MATHEMATICAL_VALIDATION.csv` and `MATHEMATICAL_VALIDATION.md`.

## API-contract validation: PASS at documentation/source level

`API_CONTRACT_VALIDATION.md` records the external interfaces relied upon for:

- `gamlss.dist::GAMLSS()`;
- `gamlss.inf::gamlssZadj` formula/prediction interfaces;
- current `emmeans` GAMLSS restrictions;
- `marginaleffects::avg_predictions()`, `avg_comparisons()`, `avg_slopes()`, and simulation inference;
- pairwise direction harmonization and weighted nonlinear safeguards.

This is an interface audit, not a substitute for executing R integration tests.

## R test suite prepared

The `testthat` suite contains **39 `test_that()` blocks**. Coverage includes:

- GA/GAF parameter and full-distribution estimands;
- generic zero-adjusted GG;
- `gamlssZadj` marginal means and `xi0`;
- exact zero mass for continuous distributions;
- class/factor-safe prediction grids;
- `pb()` routing;
- bootstrap with refit;
- unequal-score `opoly`;
- dynamic nonconventional parameter-name discovery;
- `marginaleffects` predictions, comparisons, pairwise direction, multiplicity, ratio and percent-change consistency;
- reference-population-only automatic routing for `emmeans`;
- first/second derivatives, turning points and locally refined optima;
- diagnostic-plan uncertainty and engine fallbacks.

The package contains **14 complete scripts** under `inst/examples/`.

## Correctness safeguards added in 0.2.0

- Generic zero-adjusted moments/quantiles are built from the positive distribution rather than assuming `mu = E(Y+)`.
- Exact `P(Y=0)` is distinguished from a continuous `F(0)`.
- Distributional parameters are discovered dynamically in the core adapter.
- Observed/balanced target populations are not silently replaced by an EMM reference grid.
- Pairwise direction is harmonized across internal, `emmeans`, and `marginaleffects` engines.
- Scientific effect scale is separated from contrast geometry.
- Percent change is a ratio of marginal means, not a mean of unit-level percent changes.
- Generic covariance-based uncertainty is never fabricated for nonlinear multi-parameter estimands.
- Refit bootstrap is capped at two workers and restores any previous future plan.

## Runtime limitation

No `R` or `Rscript` executable is available in this container. Prior attempts to install or download R were blocked by the container's network/DNS isolation. Consequently, the following remain mandatory on a machine with R and access to the declared dependencies:

```sh
Rscript tools/cran_preflight.R
```

That script regenerates Roxygen documentation, executes the `testthat` suite, renders the vignette, runs `R CMD build`, and runs `R CMD check --as-cran` on the exact built source tarball.

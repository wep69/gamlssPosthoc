# gamlssPosthoc 0.2.0 — Implementation report

Date: 2026-08-08

This development report maps the eight requested improvements to code, tests, and design decisions. It is excluded from the CRAN source build.

## 1. Generic zero-adjusted distribution engine

Implemented in `R/zero_adjusted.R`.

- Reconstructs the positive distribution with `gamlss.dist::GAMLSS()` when possible.
- Combines it with the fitted `xi0` mass.
- Generic mean, variance, quantile, exact zero probability, and random generation.
- `positive_dist_fun` is the escape hatch for local/transformed families that cannot be reconstructed automatically.
- `positive_mean_fun` remains only as a deprecated backward-compatibility hook.

Mixture formulas:

`E(Y) = (1-h) E(Y+)`

`Var(Y) = (1-h) Var(Y+) + h(1-h) E(Y+)^2`

`Q(p)=0` for `p <= h`, otherwise `Q+((p-h)/(1-h))`.

## 2. Remove fixed dependence on mu/sigma/nu/tau

Implemented in `R/adapters.R`.

- Parameters are discovered from `object$parameters`; older/atypical objects fall back to dynamic `.formula`/`.link` component discovery.
- Formulas, links and smoothers are inspected through an adapter layer.
- The positive-distribution constructor discovers supported parameter names from `formals(gamlss.dist::GAMLSS)` rather than maintaining another fixed list.
- `gamlssZadj` formulas use the documented S3 method `formula(object, parameter=...)`.
- Conventional names remain isolated only where the current external API itself requires them.

## 3. More responsibility for marginaleffects

Implemented in `R/gamlss_posthoc.R` and `R/gamlss_trend.R`.

- `avg_predictions()` for standardized parameter predictions.
- `avg_comparisons()` for supported factor contrasts.
- response-scale difference, ratio and log-ratio; percent change is obtained by transforming the ratio of marginal means, preserving the same estimand as the distribution engine.
- `avg_slopes()` for first derivatives of ordinary distributional parameters.
- `inferences(method="simulation")` when requested.
- automatic-routing failures fall back to the distribution engine; an explicitly requested `marginaleffects` engine fails loudly instead of masking incompatibility.
- multiplicity adjustment is applied explicitly to returned p-values; unsupported `p.adjust()` names such as Tukey fall back to Holm with a warning.

## 4. Explicit estimand and target population

Implemented in `R/estimands.R` and `R/utils.R`.

Each result records:

- target/estimand;
- mathematical definition;
- scale;
- target population (`observed`, `balanced`, `reference`);
- weighting;
- conditioning/standardization statement.

Important routing safeguard: `emmeans` is eligible automatically only when `population="reference"`. An observed-population request is never silently replaced by an EMM reference grid.

## 5. Layered uncertainty

Implemented in `R/estimands.R` and `R/gamlss_posthoc.R`.

Available layers:

- `delta`;
- coefficient `simulation` where the external engine supports it;
- refit `bootstrap` (parametric/case/cluster);
- `none`.

`auto` avoids launching hundreds of refits without explicit consent. For generic derived distribution targets, the diagnostic recommends explicit bootstrap when intervals are required.

## 6. Scientific-scale contrasts

Implemented in `R/utils.R`.

The package separates contrast geometry from effect scale:

- geometry: pairwise/reference/sequential/poly/opoly;
- scale: difference/ratio/log-ratio/percent change.

The null value is handled correctly: 1 for a ratio, 0 for the other implemented scales.

## 7. Quantitative regression

Implemented in `R/gamlss_trend.R`.

- fitted curves;
- first derivative;
- second derivative;
- turning points by sign change/interpolation;
- locally quadratic-refined interior maximum/minimum with safe grid fallback;
- bootstrap uncertainty for curves/derivatives/optimum location;
- simultaneous bootstrap bands;
- optional delegation of first parameter derivatives to `marginaleffects::avg_slopes()`.

Polynomial factor contrasts remain separate from quantitative polynomial regression.

## 8. True diagnostic planner

Implemented in `R/gamlss_posthoc_plan.R`.

The planner returns:

- model class/family;
- zero-adjusted status;
- dynamic parameter list;
- formula/link/smoother table;
- per-parameter engine capability matrix;
- installed optional dependencies;
- engine eligibility for the current request;
- recommended engine;
- recommended uncertainty layer;
- warnings explaining why an engine is avoided.

`gamlss_engine_info()` is retained as a legacy compact wrapper.

## Validation performed in this environment

### Static package validation

`tools/static_validate.py`

Checks:

- all exported functions exist;
- R formal arguments match Rd `usage` exactly;
- delimiter/string balance heuristics for R source, tests and examples;
- duplicate function definitions;
- S3 registrations point to real methods;
- path/line-ending portability;
- 20 critical architectural invariants from the 0.2.0 refactor.

Result: **PASS, no failures**.

### Independent mathematical validation

`tools/mathematical_validation.py`

19 independent numerical/Monte Carlo checks:

- zero-adjusted Gamma mean, variance and quantiles;
- zero-adjusted Lognormal mean, variance and quantiles;
- difference, ratio, log-ratio and percent change;
- first and second numerical derivatives;
- quadratic turning-point interpolation.

Result: **19/19 PASS**.

### R test suite prepared

`tests/testthat/` currently contains **39 `test_that()` blocks**, including integration tests for:

- GA/GAF;
- `pb()` routing;
- exact probability mass at zero;
- factor-level preservation;
- `gamlssZadj`;
- generic zero-adjusted GG;
- case bootstrap;
- `opoly` with unequal scores;
- `marginaleffects` predictions/comparisons, multiplicity handling, and percent-change consistency with marginal-mean ratios;
- dynamic nonconventional parameter-name discovery;
- second derivatives, turning points and locally refined optima;
- explicit reference-population routing for `emmeans`.

### Runtime limitation

The container does not have R installed and cannot download R because outbound DNS/network access for the container is blocked. Therefore no claim is made that `R CMD check` was executed here. `tools/cran_preflight.R` is included to run Roxygen, tests, vignette build, `R CMD build`, and `R CMD check --as-cran` on a machine with R and the declared dependencies.

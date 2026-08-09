# gamlssPosthoc 0.2.0

<!-- badges: start -->
[![R-CMD-check](https://github.com/wep69/gamlssPosthoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wep69/gamlssPosthoc/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Marginal and distributional post-hoc inference after GAMLSS, with explicit
estimands, a generic zero-adjusted engine, scientific contrast scales and
quantitative regression.**

`gamlssPosthoc` organises the post-processing of `gamlss`/`gamlssZadj` models
without assuming that every "adjusted mean" represents the same quantity.
Version 0.2.0 formally separates:

1. **estimand**: a parameter, the mathematical mean, a variance, a quantile,
   the exact mass at zero, or a custom function;
2. **target population**: observed, balanced, or a reference profile;
3. **comparison geometry**: pairwise, reference, sequential or polynomial;
4. **scientific scale**: difference, ratio, log-ratio or percent change;
5. **uncertainty**: delta, simulation, refit bootstrap, or point estimates
   only.

## Installation

```r
# install.packages("remotes")
remotes::install_github("wep69/gamlssPosthoc", build_vignettes = TRUE)
```

`build_vignettes = TRUE` is recommended, because `install_github()` skips
vignettes by default. Building them requires pandoc, which ships inside
RStudio. Outside RStudio the command above fails.

### Installation with fallback

The block below locates pandoc on its own, installs any missing dependencies
and, if the vignettes still cannot be built, reinstalls without them instead
of aborting. Copy and paste it whole:

```r
## 1. Make sure a CRAN mirror is set -----------------------------------------
## Without this, install.packages() fails in a non-interactive session.
cran <- getOption("repos")[["CRAN"]]
if (is.null(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

## 2. Installer --------------------------------------------------------------
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

## 3. Optional dependencies (in Suggests, but needed in practice) ------------
deps <- c("gamlss", "gamlss.dist", "gamlss.inf",
          "distributions3", "emmeans", "marginaleffects", "multcompView")
missing_deps <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_deps)) install.packages(missing_deps)

## 4. Locate pandoc, needed only for the vignettes ---------------------------
has_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
  rmarkdown::pandoc_available()

if (!has_pandoc && requireNamespace("rmarkdown", quietly = TRUE)) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    "C:/Program Files/Quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools"
  )
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  for (p in candidates[nzchar(candidates)]) {
    if (file.exists(file.path(p, exe))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      rmarkdown::find_pandoc(cache = FALSE, dir = p)
      break
    }
  }
  has_pandoc <- rmarkdown::pandoc_available()
}

## 5. Install, with a fallback without vignettes -----------------------------
installed <- FALSE
if (has_pandoc) {
  installed <- tryCatch({
    remotes::install_github("wep69/gamlssPosthoc", build_vignettes = TRUE)
    TRUE
  }, error = function(e) {
    message("Vignette build failed: ", conditionMessage(e))
    FALSE
  })
} else {
  message("pandoc not found; installing without the vignettes.")
}

if (!installed) {
  remotes::install_github("wep69/gamlssPosthoc", build_vignettes = FALSE)
}

## 6. Check ------------------------------------------------------------------
library(gamlssPosthoc)
packageVersion("gamlssPosthoc")
vignette(package = "gamlssPosthoc")  # empty if installed without vignettes
```

If step 6 does not show the vignettes, the package is installed and fully
functional, only without the long documentation. It can also be read directly
in the repository, under `vignettes/`.

## What changed in 0.2.0

### 1. Generic zero-adjusted engine

For `gamlssZadj`, the package rebuilds the complete positive distribution with
`gamlss.dist::GAMLSS()` whenever possible. If

\[
P(Y=0)=h
\]

and the positive part is \(Y_+\), then

\[
E(Y)=(1-h)E(Y_+)
\]

\[
Var(Y)=(1-h)Var(Y_+)+h(1-h)E(Y_+)^2.
\]

Quantiles come from the complete distribution:

\[
Q_Y(p)=0,\quad p\le h,
\]

\[
Q_Y(p)=Q_+\left(\frac{p-h}{1-h}\right),\quad p>h.
\]

This removes the old GA/GAF restriction. Compatible positive families, such
as `GG`, can be processed without assuming that `mu` is the mean.

For local or custom families that cannot be converted automatically, use
`positive_dist_fun`.

### 2. Model adapter

The core no longer walks a fixed `mu/sigma/nu/tau` list. It discovers
`object$parameters`, formulas, links and smoothers through an internal
adapter layer. This keeps the package usable with the current `gamlss`
infrastructure and makes future adapters easier without exposing internal
details in the public API.

### 3. More responsibility for `marginaleffects`

When the target is a distributional parameter of an ordinary `gamlss` fit,
`marginaleffects` can take over:

- `avg_predictions()` for standardization;
- `avg_comparisons()` for differences, ratios and log-ratios; percent change
  is transformed from the ratio of marginal means, preserving the same
  estimand as the distribution engine;
- `avg_slopes()` for first-order derivatives in quantitative regression;
- `inferences(method = "simulation")` when requested and supported.

Under `engine = "auto"` routing, a capability failure on a specific object
falls back to the distribution engine instead of producing a silent partial
answer. If `engine = "marginaleffects"` is requested explicitly, the failure
is reported as an error so that an incompatibility is not masked.

### 4. The estimand is recorded

Every result of `gamlss_posthoc()` includes `estimand_info`, for example:

```text
Target:       marginal response mean including the zero mass
Definition:   E(Y|x) = [1-P(Y=0|x)] E(Y+|Y>0,x)
Population:   observed
Weighting:    proportional
Scale:        response/distribution
```

### 5. Layered uncertainty

```r
uncertainty = "delta"
uncertainty = "simulation"
uncertainty = "bootstrap"
uncertainty = "none"
```

The full bootstrap can be parametric, by cases or by clusters. `auto` does
not fire hundreds of hidden refits; `gamlss_posthoc_plan()` reports when an
explicit bootstrap is recommended for intervals of derived estimands.

### 6. Contrasts on a scientific scale

The question "which levels?" is separated from the question "which effect?".

```r
contrast = "reference"
comparison = "difference"
```

or

```r
contrast = "reference"
comparison = "ratio"
```

or

```r
comparison = "percent_change"
```

A comparison can therefore be presented directly as

\[
100\left(\frac{E(Y_A)}{E(Y_B)}-1\right),
\]

without depending on an implicit reading of the link scale.

### 7. Extended quantitative regression

`gamlss_trend()` now supports:

```r
method = "curve"
method = "derivative", derivative_order = 1
method = "derivative", derivative_order = 2
method = "turning_points"
method = "optimum", optimum = "maximum"
```

With the bootstrap it can also build simultaneous bands from the maximum
standardized deviation across bootstrap curves. For first-order derivatives
of ordinary parameters, `delta` and `simulation` can be delegated to
`marginaleffects`; for distribution-derived targets, those layers fall back to
the refit bootstrap.

### 8. A true diagnostic

Before running an analysis:

```r
plan <- gamlss_posthoc_plan(
  fit,
  estimand = "mean",
  contrast = "pairwise",
  comparison = "ratio"
)
print(plan)
```

The diagnostic shows:

- class and family;
- the discovered parameters;
- formulas and links;
- smoothers by parameter;
- installed optional dependencies;
- eligibility of `emmeans`, `marginaleffects` and the distribution engine;
- the recommended engine;
- the recommended uncertainty layer;
- structural warnings.

`gamlss_engine_info()` remains available as a compact wrapper for backward
compatibility.

---

## Local installation from source

```r
install.packages(c(
  "gamlss", "gamlss.dist", "distributions3",
  "gamlss.inf", "emmeans", "marginaleffects",
  "future", "future.apply", "multcompView"
))

install.packages("gamlssPosthoc_0.2.0.tar.gz", repos = NULL, type = "source")
```

> The tarball for CRAN submission must be created by `R CMD build`, not by
> manual compression.

---

## Example 1: a scientific contrast under Gamma

```r
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(1)
D <- data.frame(trt = factor(rep(c("T0", "T1", "T2"), each = 50)))
D$y <- rGA(nrow(D), mu = c(T0 = 3, T1 = 3.6, T2 = 4.4)[D$trt], sigma = .25)
fit <- gamlss(y ~ trt, family = GA, data = D, trace = FALSE)

ans <- gamlss_posthoc(
  fit,
  specs = "trt",
  estimand = "mean",
  population = "observed",
  contrast = "reference",
  comparison = "percent_change",
  uncertainty = "none",
  data = D
)

ans$estimand_info
ans$estimates
ans$contrasts
```

## Example 2: zero-adjusted Generalized Gamma

```r
library(gamlss.inf)
library(distributions3)

set.seed(2)
D <- data.frame(trt = factor(rep(c("Control", "Bio"), each = 80)))
mu <- ifelse(D$trt == "Control", 2, 3)
h  <- ifelse(D$trt == "Control", .40, .18)
ypos <- rGG(nrow(D), mu = mu, sigma = .35, nu = .7)
D$y <- ifelse(rbinom(nrow(D), 1, h) == 1, 0, ypos)

fitz <- gamlssZadj(
  y = y,
  mu.formula = ~ trt,
  xi0.formula = ~ trt,
  family = GG,
  data = D,
  trace = FALSE
)

nd <- data.frame(trt = factor(levels(D$trt), levels = levels(D$trt)))
gamlss_distribution_summary(fitz, nd, data = D)

gamlss_posthoc(
  fitz, specs = "trt",
  estimand = "mean",
  contrast = "pairwise",
  comparison = "ratio",
  uncertainty = "none",
  data = D
)
```

## Example 3: a quantitative dose

```r
set.seed(3)
D <- data.frame(dose = seq(0, 150, length.out = 180))
eta <- log(2) + .012 * D$dose - .000045 * D$dose^2
D$biomass <- rGA(nrow(D), mu = exp(eta), sigma = .20)
fit <- gamlss(biomass ~ dose + I(dose^2), family = GA, data = D, trace = FALSE)
g <- seq(0, 150, length.out = 151)

gamlss_trend(fit, "dose", at_x = g,
             method = "derivative", derivative_order = 1,
             estimand = "mean", uncertainty = "none", data = D)

gamlss_trend(fit, "dose", at_x = g,
             method = "turning_points",
             estimand = "mean", uncertainty = "none", data = D)$special_points

gamlss_trend(fit, "dose", at_x = g,
             method = "optimum", optimum = "maximum",
             estimand = "mean", uncertainty = "none", data = D)$special_points
```

## `emmeans`: where it is still preferred

The documented support of `emmeans` for `gamlss` remains focused on
`what = "mu"`, `"sigma"`, `"nu"` or `"tau"`, and does not cover the selected
component when it contains a smoother such as `pb()`. In addition, in this
version of `gamlssPosthoc`, automatic routing to `emmeans` requires
`population = "reference"` and `comparison = "difference"`; this prevents a
request for the observed population from being silently replaced by the
`emmeans` reference grid.

Technical source: <https://rvlenth.github.io/emmeans/articles/models.html>

## `marginaleffects`: where it takes on more work

`marginaleffects` offers standardization of predictions, comparisons on
several scales, slopes and simulation-based inference. The package uses those
capabilities when the target is a parameter of an ordinary `gamlss` fit, and
keeps a run-time capability test. Ratios are ratios of marginalized means;
`percent_change` is defined as `100 * (ratio - 1)`, not as the average of
unit-by-unit percent changes.

Technical sources:

- <https://marginaleffects.com/man/r/predictions.html>
- <https://marginaleffects.com/man/r/comparisons.html>
- <https://marginaleffects.com/man/r/slopes.html>
- <https://marginaleffects.com/man/r/inferences.html>

## Vignettes

- `workflow`: a short overview of the five decisions.
- `gamlssPosthoc-foundations`: statistical background, one section per
  function, worked examples and references.

## Example files

`inst/examples/` contains 14 standalone examples of:

1. a Gamma factorial plus `emmeans`;
2. the `pb()` smoother;
3. GAF and the mean-variance power;
4. quantitative polynomial regression;
5. polynomial contrasts with unequal spacing;
6. zero-adjusted Gamma;
7. a custom ZA-GAF;
8. optional `marginaleffects`;
9. compact letter display;
10. generic zero-adjusted GG;
11. difference, ratio, log-ratio and percent change;
12. derivatives, turning points and optimum;
13. the complete diagnostic;
14. `marginaleffects` as the comparison engine.

## Validation

The package ships a development preflight:

```r
tools/cran_preflight.R
```

It regenerates the documentation with Roxygen, runs the tests, compiles and
verifies both vignettes, builds the tarball with `R CMD build` and runs
`R CMD check --as-cran` on it. Pandoc is located automatically, and the PDF
manual step is skipped with a clear message when LaTeX is absent.

Current status: **0 errors, 0 warnings, 1 note** (the standard "New
submission" note), reproduced on nine environments: win-builder R-devel and
R-release, the macOS builder on arm64, a local Windows install, and five
GitHub Actions configurations covering R-devel, release and oldrel-1 on
Ubuntu plus release on macOS and Windows. The macOS builder reports
`Status: OK` with no note at all.

The test suite contains **40 `test_that()` blocks**, and every exported
function carries three or four executable examples spanning continuous,
discrete and mixed response families (GA, NO, LOGNO, WEI, IG, PO and ZAGA).

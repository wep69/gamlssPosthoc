# gamlssPosthoc 0.3.0

<!-- badges: start -->
[![R-CMD-check](https://github.com/wep69/gamlssPosthoc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/wep69/gamlssPosthoc/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**Marginal, distributional and graphical post-hoc inference after GAMLSS, with
explicit estimands, a generic zero-adjusted engine, scientific contrast scales,
quantitative regression, and a data-first `ggplot2` layer.**

`gamlssPosthoc` organises the post-processing of `gamlss` and `gamlssZadj`
models without assuming that every "adjusted mean" represents the same
quantity. The package separates five decisions and records all of them in the
returned object:

1. **estimand**: a distributional parameter, the mathematical mean, a variance,
   a quantile, the exact mass at zero, or a custom function;
2. **target population**: observed, balanced, or a reference profile;
3. **comparison geometry**: pairwise, reference, sequential or polynomial;
4. **scientific scale**: difference, ratio, log-ratio or percent change;
5. **uncertainty**: delta, simulation, refit bootstrap, or point estimates only.

---

## Installation

### With the vignettes

Seven vignettes ship with the package, and they are the main documentation.
Building them requires **pandoc**, which is bundled with RStudio, Positron and
Quarto. If you work inside any of those, this is all you need:

```r
# install.packages("remotes")
remotes::install_github("wep69/gamlssPosthoc", build_vignettes = TRUE)
```

Then:

```r
vignette(package = "gamlssPosthoc")
vignette("case-study-agronomy", package = "gamlssPosthoc")
```

Two details are worth knowing. `install_github()` skips vignettes by default,
which is why `build_vignettes = TRUE` is explicit. And building them installs
the packages listed in `Suggests`, so the first install takes longer.

### Without the vignettes

Faster, and the right choice on a server, in a container, or anywhere pandoc is
absent. The package is fully functional; only the long documentation is missing,
and it can still be read in the repository under `vignettes/`.

```r
# install.packages("remotes")
remotes::install_github("wep69/gamlssPosthoc", build_vignettes = FALSE)
```

### A specific version

Releases are tagged, so an exact version is one argument away:

```r
remotes::install_github("wep69/gamlssPosthoc@v0.3.0", build_vignettes = TRUE)
remotes::install_github("wep69/gamlssPosthoc@v0.2.0")   # previous release
```

### Robust installation, with a fallback

The block below finds pandoc on its own, installs the optional dependencies,
and falls back to an install without vignettes instead of aborting. Copy and
paste it whole:

```r
## 1. Make sure a CRAN mirror is set -----------------------------------------
cran <- getOption("repos")[["CRAN"]]
if (is.null(cran) || !nzchar(cran) || identical(cran, "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

## 2. Installer --------------------------------------------------------------
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

## 3. Optional dependencies (in Suggests, but needed in practice) ------------
deps <- c("gamlss", "gamlss.dist", "gamlss.inf", "distributions3",
          "emmeans", "marginaleffects", "multcompView", "ggplot2")
missing_deps <- deps[!vapply(deps, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_deps)) install.packages(missing_deps)

## 4. Locate pandoc, needed only for the vignettes ---------------------------
has_pandoc <- requireNamespace("rmarkdown", quietly = TRUE) &&
  rmarkdown::pandoc_available()

if (!has_pandoc && requireNamespace("rmarkdown", quietly = TRUE)) {
  candidates <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
    "C:/Program Files/Pandoc",
    "C:/Program Files/Quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Positron/resources/app/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/usr/local/bin", "/opt/homebrew/bin"
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

### From a source tarball

```r
install.packages("gamlssPosthoc_0.3.0.tar.gz", repos = NULL, type = "source")
```

> A tarball for CRAN submission must be created by `R CMD build`, never by
> manual compression.

---

## Documentation

Seven vignettes, all with executable code:

| Vignette | What it covers |
|---|---|
| `workflow` | The five decisions, the estimand as a functional, Jensen's inequality, the ICH E9(R1) mapping, the delta method |
| `estimands-and-standardization` | The g-formula, the three weighting rules, how much a conclusion moves between them |
| `visualization` | The data-first graphics layer, mixture quantiles, diagnostics, accessibility |
| `cookbook` | Three worked calls for every exported function |
| `case-study-agronomy` | One complete analysis, from design to a written Results paragraph |
| `advanced-workflows` | Meta-analysis, longitudinal data, two-part models, proper scoring rules |
| `missing-data` | Multiple imputation, Rubin's rules, pooling scale, MNAR sensitivity |

```r
vignette("workflow", package = "gamlssPosthoc")
```

---

## What changed in 0.3.0

### 1. A data-first graphics layer

The chain is explicit:

```
model -> estimand or distribution -> plot data -> ggplot
```

`gamlss_plot_data()` returns the auditable table behind every figure, and the
`plot_gamlss_*()` functions return ordinary `ggplot` objects the user can
modify. No figure is a black box.

Thirteen plotting functions cover distributional parameters, marginal
estimands, contrasts, predictive densities, quantile fans, trends, derivatives,
optima, surfaces, observed-versus-fitted, diagnostics, model comparison and the
zero-adjusted decomposition. Three themes are provided, plus `autoplot()` and
`plot()` methods for the four result classes.

### 2. Interoperability

`tidy()`, `glance()`, `augment()`, `model_parameters()` and `as_flextable()`
methods, plus `export_to_latex()`, `export_to_word()` and `generate_report()`.

### 3. Documentation

Every exported function carries three executable examples, and every function
also appears at least three times across the vignettes. References are managed
in `vignettes/references.bib`, with all metadata transcribed from the Crossref
API rather than typed by hand; the audit trail is in `REFERENCES_AUDIT.md`.

Full history is in [`NEWS.md`](NEWS.md).

---

## A first example

```r
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(1)
D <- data.frame(trt = factor(rep(c("T0", "T1", "T2"), each = 50)))
D$y <- rGA(nrow(D), mu = c(T0 = 3, T1 = 3.6, T2 = 4.4)[D$trt], sigma = .25)
fit <- gamlss(y ~ trt, family = GA, data = D, trace = FALSE)

# Always check what is possible before computing a contrast
gamlss_posthoc_plan(fit, estimand = "mean", contrast = "pairwise")

ans <- gamlss_posthoc(
  fit, specs = "trt",
  estimand = "mean",
  population = "observed",
  contrast = "reference",
  comparison = "percent_change",
  uncertainty = "none",
  data = D
)

ans$estimand_info
ans$contrasts

plot_gamlss_contrasts(ans, null = 0)
```

---

## Check status

| Platform | R | Status |
|---|---|---|
| win-builder | R-devel, r90389 ucrt | 1 NOTE |
| win-builder | 4.6.1 ucrt | 1 NOTE |
| macOS builder | 4.6.1 Patched, arm64 | **OK**, no notes |
| local | 4.6.0, Windows 11 | 1 NOTE |

No errors and no warnings anywhere. The single note is the expected
`New submission` note plus surnames and the acronym GAMLSS flagged by the
spell checker; both are justified in `cran-comments.md`.

---

## Citation

```r
citation("gamlssPosthoc")
```

The methodology follows the distributional-regression principles of Rigby and
Stasinopoulos (2005) <doi:10.1111/j.1467-9876.2005.00510.x> and the
model-interpretation principles of Arel-Bundock, Greifer and Heiss (2024)
<doi:10.18637/jss.v111.i09>.

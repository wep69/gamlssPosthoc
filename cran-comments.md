## Submission

This is a new submission of `gamlssPosthoc`.

The package provides post-hoc inference for GAMLSS models, keeping the
estimand, the target population, the contrast geometry, the effect scale and
the uncertainty layer as separate and explicitly recorded choices.

## Test environments

* Local: Windows 11 x64, R 4.6.0 (2026-04-24 ucrt), `R CMD check --as-cran`.
* GitHub Actions, all with `--as-cran`:
  * ubuntu-latest, R-devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
  * macos-latest, R release
  * windows-latest, R release

## R CMD check results

0 errors | 0 warnings | 1 note

The note is the standard one for a first-time submission:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Walter Esfrain Pereira <walterufpb@yahoo.com.br>'
New submission
```

## Notes for the reviewer

* All modelling dependencies (`gamlss`, `gamlss.dist`, `gamlss.inf`,
  `emmeans`, `marginaleffects`, `distributions3`, `multcompView`) are in
  `Suggests`. Every use is conditional: the package code routes through an
  internal `requireNamespace()` helper, all seven examples are wrapped in
  `requireNamespace()`, the tests use `skip_if_not_installed()`, and the
  vignette chunks are gated on availability. `Imports` is limited to
  `graphics`, `stats` and `utils`.
* Examples are executable and fast: seven examples, the slowest at 0.6
  seconds, under one second in total.
* The package does not write to the user's file space, and does not modify
  `options()`, `par()` or the working directory.
* The reference to Rigby and Stasinopoulos (2005) in the Description field
  uses the requested `<doi:...>` form.

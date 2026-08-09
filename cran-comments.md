## Submission

This is a new submission of `gamlssPosthoc`.

The package provides post-hoc inference for GAMLSS models, keeping the
estimand, the target population, the contrast geometry, the effect scale and
the uncertainty layer as separate and explicitly recorded choices.

## Test environments

* win-builder, R 4.6.1 release, Windows Server 2022 x64.
* macOS builder, R 4.6.1 release, macOS 26.6, arm64 (Apple M1).
* Local: Windows 11 x64, R 4.6.0 (2026-04-24 ucrt), `R CMD check --as-cran`.
* GitHub Actions, all with `--as-cran`:
  * ubuntu-latest, R-devel
  * ubuntu-latest, R release
  * ubuntu-latest, R oldrel-1
  * macos-latest, R release
  * windows-latest, R release

## R CMD check results

0 errors | 0 warnings | 1 note

The macOS builder reports `Status: OK`, with no errors, warnings or notes.

On win-builder the single note is the expected one for a first-time
submission, together with the usual spelling remark:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Walter Esfrain Pereira <walterufpb@yahoo.com.br>'

New submission

Possibly misspelled words in DESCRIPTION:
  GAMLSS (3:59, 9:43)
  Rigby (16:8)
  Stasinopoulos (16:18)
  estimands (13:49)
```

All four flagged words are correct and intentional:

* **GAMLSS** is the standard acronym for generalized additive models for
  location, scale and shape. It appears in the Title and in the Description,
  and the Description introduces it with its full expansion, "generalized
  additive models for location, scale and shape (GAMLSS)". It denotes the
  model class, not a package; the 'gamlss' package itself is quoted in the
  Description as required.
* **Rigby** and **Stasinopoulos** are the surnames of the authors of the
  cited reference, Rigby and Stasinopoulos (2005)
  <doi:10.1111/j.1467-9876.2005.00510.x>.
* **estimand** is established statistical terminology, used here in the sense
  of ICH E9(R1) and of the causal-inference literature. It is central to the
  package, which records the estimand of every result explicitly.

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

# cran-comments

## Submission

This is an initial submission of `gamlssPosthoc` 0.3.0.

The package provides a conservative post-hoc and visualization workflow for
GAMLSS models. It separates estimands, target populations, contrast scales and
uncertainty layers, routes supported operations to `emmeans` or
`marginaleffects`, reconstructs full predictive distributions for derived
quantities and zero-adjusted models, and supplies an auditable `ggplot2`
data-first graphics layer.

## Test environments

* Local: Windows 11 x86_64, R 4.6.0
* win-builder: R Under development (unstable) (2026-08-10 r90389 ucrt),
  x86_64-w64-mingw32, Windows Server 2022
* win-builder: R 4.6.1 (2026-06-24 ucrt), x86_64-w64-mingw32, Windows Server 2022
* macOS builder: R 4.6.1 Patched (2026-07-27 r90311), aarch64-apple-darwin23

## R CMD check results

There were no ERRORs and no WARNINGs on any platform.

macOS builder returned `Status: OK` with no notes at all. Both Windows builders
returned one NOTE, reproduced and justified below.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Walter Esfrain Pereira <walterufpb@yahoo.com.br>'

New submission

Possibly misspelled words in DESCRIPTION:
  Arel (20:40)
  Bundock (20:45)
  GAMLSS (4:9, 10:64)
  Greifer (20:54)
  Heiss (20:66)
  Rigby (18:64)
  Stasinopoulos (19:5)
  estimands (11:23, 16:5)
```

`New submission` is expected for a first submission.

Regarding the flagged words, none is a misspelling.

* `Arel` and `Bundock` are one surname, Arel-Bundock, split by the spell checker
  at the hyphen. `Greifer` and `Heiss` are the co-authors of the same cited work
  (DOI 10.18637/jss.v111.i09).
* `Rigby` and `Stasinopoulos` are the authors of the cited GAMLSS paper
  (DOI 10.1111/j.1467-9876.2005.00510.x).
* `GAMLSS` is an established acronym. The Description expands it in full at
  first use, "generalized additive models for location, scale and shape
  (GAMLSS)". It also appears in the Title, where expanding it would make the
  title impractically long.
* `estimands` is standard statistical terminology, the vocabulary of the
  ICH E9(R1) addendum, and is the central concept of the package.

## Notes for the reviewers

* All heavy dependencies are in `Suggests` and every example, test and vignette
  chunk that touches them is guarded by `requireNamespace()`. The package
  installs and loads with `Imports` only.
* `gamlss` and `gamlss.inf` resolve some distribution functions by name from the
  search path. The tests and the vignettes therefore attach `gamlss.dist`
  explicitly. This is a documented requirement of those packages and not a
  workaround for a check.
* Uncertainty layers that require model refitting use small replication counts in
  examples, tests and vignettes. The slowest check stage is the vignette
  re-build, at 119 seconds on win-builder R-devel.
* Nothing is written outside `tempdir()`. The examples that produce `.tex`,
  `.docx` and `.md` files write to `tempdir()` only.
* The package never assigns to the global environment.
* Two `vdiffr` visual regression tests are skipped on CRAN, which is the
  recommended practice for graphics snapshots.

## Downstream dependencies

None. This is a new package.

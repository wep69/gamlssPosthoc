# CRAN audit status — gamlssPosthoc 0.2.0

Development-only file, excluded by `.Rbuildignore`.

Static and mathematical pre-submission audit on 2026-08-08:

- 20 critical architectural invariants: PASS;
- 19/19 independent mathematical checks: PASS;
- 7 exported public functions;
- 8 Rd manuals;
- 5 registered S3 methods;
- 1 executable vignette source;
- 14 complete example scripts;
- 39 prepared `testthat` blocks in two test files plus the test runner;
- 12 R source files;
- `.Rbuildignore` preview: approximately 44 files / 163 KiB before `R CMD build`;
- all documented public function formals match their Rd `usage` in the static audit;
- no static delimiter/string, duplicate-definition, S3-target, CRLF, or path-portability failures detected.

External dependency versions were rechecked against CRAN. In particular, the
minimum `distributions3` requirement was corrected from an impossible 0.2.4 to
`>= 0.2.1`; CRAN currently provides 0.2.3, and `is_discrete()` was introduced
in 0.2.1.

R installation was attempted on Debian 13, targeting the official CRAN R 4.6.x
repository and the R 4.6.1 source tarball. The container has no R executable,
and its network/DNS isolation blocks package/source downloads. Therefore a real
`R CMD build`, `R CMD check --as-cran`, Roxygen regeneration, vignette execution,
and the 39 runtime `testthat` blocks could not be executed here.

Do not submit until `Rscript tools/cran_preflight.R` runs successfully in an
R-enabled environment and every ERROR/WARNING/NOTE is resolved or justified.

## Upstream watch

As of 2026-08-08, the CRAN check page for `gamlss.dist` 6.1-1 reports Rd-file NOTEs in that upstream package. These are external to `gamlssPosthoc`, but should be rechecked before submission because `gamlss.dist` is central to the distribution bridge.

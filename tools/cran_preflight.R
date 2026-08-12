#!/usr/bin/env Rscript
# Development-only CRAN preflight for gamlssPosthoc.
# Run from the package root. Excluded from the source package by .Rbuildignore.

pkg <- normalizePath(".", mustWork = TRUE)
required <- c(
  "roxygen2", "testthat", "knitr", "rmarkdown", "rcmdcheck", "ggplot2", "generics",
  "gamlss", "gamlss.dist", "gamlss.inf", "distributions3", "emmeans",
  "marginaleffects"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required development packages first: ", paste(missing, collapse = ", "))
}

cat("R version:\n"); print(R.version.string)
cat("roxygen2 version:\n"); print(as.character(utils::packageVersion("roxygen2")))

cat("\n[1/7] Regenerating Rd and NAMESPACE from Roxygen...\n")
roxygen2::roxygenise(pkg, roclets = c("rd", "namespace"))

cat("\n[2/7] Running package tests from the source tree...\n")
testthat::test_local(pkg, reporter = "summary", stop_on_failure = TRUE)

cat("\n[3/7] Rendering the vignette in isolation...\n")
vig_dir <- tempfile("gamlssPosthoc-vignette-")
dir.create(vig_dir, recursive = TRUE)
rmarkdown::render(
  input = file.path(pkg, "vignettes", "workflow.Rmd"),
  output_format = "html_vignette",
  output_dir = vig_dir,
  output_file = "workflow.html",
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
vig_out <- file.path(vig_dir, "workflow.html")
stopifnot(file.exists(vig_out), file.info(vig_out)$size > 0)

cat("\n[4/7] Building the exact CRAN source tarball with R CMD build...\n")
parent <- dirname(pkg)
old <- setwd(parent)
on.exit(setwd(old), add = TRUE)
pkgbase <- basename(pkg)
build_log <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "build", shQuote(pkgbase)),
  stdout = TRUE, stderr = TRUE
)
cat(paste(build_log, collapse = "\n"), "\n")
tarballs <- list.files(parent, pattern = "^gamlssPosthoc_[0-9].*[.]tar[.]gz$", full.names = TRUE)
if (!length(tarballs)) stop("R CMD build did not produce a source tarball.")
tarball <- tarballs[which.max(file.info(tarballs)$mtime)]
cat("Built: ", tarball, "\n", sep = "")

cat("\n[5/7] Running R CMD check --as-cran on the built tarball...\n")
chk_dir <- tempfile("gamlssPosthoc-check-")
dir.create(chk_dir)
chk <- rcmdcheck::rcmdcheck(
  tarball,
  args = "--as-cran",
  error_on = "never",
  check_dir = chk_dir,
  quiet = FALSE
)
print(chk)
cat("Errors:", length(chk$errors),
    "Warnings:", length(chk$warnings),
    "Notes:", length(chk$notes), "\n")

cat("\n[6/7] Optional URL and spelling diagnostics...\n")
if (requireNamespace("urlchecker", quietly = TRUE)) {
  try(print(urlchecker::url_check(pkg)), silent = TRUE)
}
if (requireNamespace("spelling", quietly = TRUE)) {
  try(print(spelling::spell_check_package(pkg)), silent = TRUE)
}

cat("\n[7/7] Session information...\n")
print(utils::sessionInfo())

if (length(chk$errors) || length(chk$warnings) || length(chk$notes)) quit(status = 1L)
cat("\nPRE-FLIGHT CLEAN: 0 errors | 0 warnings | 0 notes\n")

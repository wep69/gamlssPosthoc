#!/usr/bin/env Rscript
# Development-only CRAN preflight for gamlssPosthoc.
# Run from the package root. Excluded from the source package by .Rbuildignore.

pkg <- normalizePath(".", mustWork = TRUE)
required <- c(
  "roxygen2", "testthat", "knitr", "rmarkdown", "rcmdcheck",
  "gamlss", "gamlss.dist", "gamlss.inf", "distributions3", "emmeans",
  "marginaleffects"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required development packages first: ", paste(missing, collapse = ", "))
}

## Pandoc is required to build the vignette but is not on PATH in a plain
## Rscript session: it ships inside RStudio and Quarto. Discover it here so
## vignette compilation is part of the default run instead of something the
## caller has to arrange by hand.
if (!rmarkdown::pandoc_available()) {
  cand <- c(
    Sys.getenv("RSTUDIO_PANDOC"),
    "C:/Program Files/Quarto/bin/tools",
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "/usr/lib/rstudio/bin/quarto/bin/tools",
    "/usr/lib/rstudio-server/bin/quarto/bin/tools",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools",
    "/Applications/quarto/bin/tools"
  )
  exe <- if (.Platform$OS.type == "windows") "pandoc.exe" else "pandoc"
  for (p in cand[nzchar(cand)]) {
    if (file.exists(file.path(p, exe))) {
      Sys.setenv(RSTUDIO_PANDOC = p)
      rmarkdown::find_pandoc(cache = FALSE, dir = p)
      break
    }
  }
}
if (!rmarkdown::pandoc_available()) {
  stop("Pandoc was not found. Vignette compilation is a required preflight step. ",
       "Install Pandoc or point RSTUDIO_PANDOC at the Pandoc directory bundled ",
       "with RStudio or Quarto.")
}

cat("R version:\n"); print(R.version.string)
cat("roxygen2 version:\n"); print(as.character(utils::packageVersion("roxygen2")))
cat("pandoc version:\n"); print(as.character(rmarkdown::pandoc_version()))

cat("\n[1/7] Regenerating Rd and NAMESPACE from Roxygen...\n")
roxygen2::roxygenise(pkg, roclets = c("rd", "namespace"))

cat("\n[2/7] Running package tests from the source tree...\n")
testthat::test_local(pkg, reporter = "summary", stop_on_failure = TRUE)

cat("\n[3/7] Compiling and verifying the vignettes...\n")
vig_all <- list.files(file.path(pkg, "vignettes"), pattern = "[.]Rmd$",
                      full.names = TRUE)
if (!length(vig_all)) stop("No vignette sources found.")
for (vig_src in vig_all) {
vig_dir <- tempfile("gamlssPosthoc-vignette-")
dir.create(vig_dir, recursive = TRUE)
cat("\n-- ", basename(vig_src), " --\n", sep = "")

## Every code chunk is gated by an `eval=has_*` guard, so a missing suggested
## package makes the vignette render successfully while silently evaluating
## nothing. Knitting to markdown first lets us confirm that the chunks really
## ran: `comment = "#>"` prefixes every line of evaluated output.
vig_md <- file.path(vig_dir, "vignette.md")
## Knit from inside the temporary directory so that generated figures land
## there instead of polluting the package sources.
old_wd <- setwd(vig_dir)
invisible(knitr::knit(vig_src, output = vig_md, quiet = TRUE,
                      envir = new.env(parent = globalenv())))
setwd(old_wd)
md_lines <- readLines(vig_md, warn = FALSE)
n_eval <- length(grep("^#>", md_lines))
n_chunks <- length(grep("^```\\{r", readLines(vig_src, warn = FALSE)))
cat("Vignette code chunks: ", n_chunks,
    " | evaluated output lines: ", n_eval, "\n", sep = "")
if (n_eval == 0L) {
  stop("The vignette produced no evaluated output. Its `eval=has_*` guards ",
       "silently disabled every chunk, which means a suggested package is ",
       "missing and the vignette is not actually exercising the package.")
}

rmarkdown::render(
  input = vig_src,
  output_format = "html_vignette",
  output_dir = vig_dir,
  output_file = "vignette.html",
  quiet = TRUE,
  envir = new.env(parent = globalenv())
)
vig_out <- file.path(vig_dir, "vignette.html")
stopifnot(file.exists(vig_out), file.info(vig_out)$size > 0)
cat("Vignette rendered: ", vig_out, "\n", sep = "")
}

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
build_status <- attr(build_log, "status")
if (!is.null(build_status) && build_status != 0L) {
  stop("R CMD build failed with status ", build_status,
       "; see the log above. No tarball was produced.")
}
## Match only the canonical name R CMD build produces. A looser pattern also
## matches hand-assembled preview archives left in the parent directory, which
## would silently be checked instead of the real build.
tarballs <- list.files(
  parent,
  pattern = "^gamlssPosthoc_[0-9]+([.][0-9]+)*[.]tar[.]gz$",
  full.names = TRUE
)
if (!length(tarballs)) stop("R CMD build did not produce a source tarball.")
tarball <- tarballs[which.max(file.info(tarballs)$mtime)]
cat("Built: ", tarball, "\n", sep = "")

cat("\n[5/7] Running R CMD check --as-cran on the built tarball...\n")
chk_dir <- tempfile("gamlssPosthoc-check-")
dir.create(chk_dir)
## The vignettes are always rebuilt by R CMD check; only the PDF reference
## manual needs LaTeX. Without pdflatex that step reports an ERROR that says
## nothing about the package, so it is skipped explicitly and flagged instead
## of being silently mistaken for a real defect.
have_latex <- nzchar(Sys.which("pdflatex"))
chk_args <- "--as-cran"
if (!have_latex) {
  chk_args <- c(chk_args, "--no-manual")
  message("NOTE: pdflatex was not found, so the PDF reference manual is not ",
          "checked here. Vignettes are still built and checked. Run the ",
          "preflight on a machine with LaTeX before submitting to CRAN.")
}
chk <- rcmdcheck::rcmdcheck(
  tarball,
  args = chk_args,
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

#!/usr/bin/env Rscript
Sys.setenv(GAMLSSPOSTHOC_VDIFFR="true", NOT_CRAN="true")
if (!requireNamespace("testthat", quietly=TRUE) || !requireNamespace("vdiffr", quietly=TRUE)) stop("Install testthat and vdiffr")
testthat::test_file("tests/testthat/test-vdiffr-v03.R", reporter="summary")
cat("If snapshots are new, review them with testthat::snapshot_review().\n")

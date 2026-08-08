#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) normalizePath(args[[1]]) else normalizePath(".")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) {
  stop("Install 'rcmdcheck' in the temporary validation library first.")
}
rcmdcheck::rcmdcheck(pkg, args = "--as-cran", error_on = "never",
                     check_dir = tempfile("check-"))

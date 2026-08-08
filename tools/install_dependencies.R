# Install recommended dependencies for gamlssPosthoc
pkgs <- c(
  "gamlss", "gamlss.dist", "gamlss.inf", "emmeans",
  "marginaleffects", "distributions3", "future", "future.apply",
  "testthat", "devtools", "rmarkdown", "knitr"
)
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
message("Dependencies available.")

# `gamlss.inf::gamlssZadj()` builds the zero-adjusted family by evaluating the
# density/CDF names of the positive family (for example "pGA", "pGG") with
# `eval(parse(text = ...))`, which resolves them only through the search path.
# Namespace-qualified access is therefore not enough: gamlss.dist must be
# attached for those tests to run at all. The same applies to gamlss itself,
# whose `gen.likelihood()` re-resolves family functions the same way.
if (requireNamespace("gamlss.dist", quietly = TRUE)) {
  suppressMessages(library(gamlss.dist))
}
if (requireNamespace("gamlss", quietly = TRUE)) {
  suppressMessages(library(gamlss))
}

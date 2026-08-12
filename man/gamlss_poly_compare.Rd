\name{gamlss_poly_compare}
\alias{gamlss_poly_compare}
\title{Compare Already-Fitted Nested Polynomial GAMLSS Models}
\usage{
gamlss_poly_compare(..., k = 2)
}
\arguments{
\item{...}{Two or more fitted GAMLSS models, or one named list of models,
ordered from simpler to more complex.}
\item{k}{Positive finite penalty multiplying effective degrees of freedom in
GAIC. \code{k = 2} gives AIC.}
}
\description{
Summarizes GAIC and sequential likelihood-ratio comparisons for already-fitted
nested GAMLSS models. This helper is intended for quantitative polynomial
regression and is separate from polynomial contrasts among ordered factor
levels.
}
\details{
Models should be fitted to the same response and observations and supplied in
increasing order of complexity. Formal nestedness is not automatically verified.
}
\value{
A data frame with model name, effective degrees of freedom, global deviance,
GAIC, and sequential LR comparison statistics and p-values.
}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE)) {
  set.seed(15)
  d <- data.frame(dose = rep(seq(0, 100, length.out = 8), each = 7))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(.5 + .01*d$dose - .00004*d$dose^2), sigma=.22)
  m1 <- gamlss::gamlss(y ~ dose, family=gamlss.dist::GA, data=d, trace=FALSE)
  m2 <- gamlss::gamlss(y ~ dose + I(dose^2), family=gamlss.dist::GA, data=d, trace=FALSE)
m3 <- gamlss::gamlss(y ~ dose + I(dose^2) + I(dose^3), family=gamlss.dist::GA, data=d,
                       trace=FALSE)
  # 1. AIC penalty
  gamlss_poly_compare(linear=m1, quadratic=m2, cubic=m3, k=2)
  # 2. BIC-like penalty
  gamlss_poly_compare(linear=m1, quadratic=m2, cubic=m3, k=log(nrow(d)))
  # 3. Models can also be supplied as one named list
  gamlss_poly_compare(list(linear=m1, quadratic=m2, cubic=m3), k=2)
}
}

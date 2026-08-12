\name{gamlss_engine_info}
\alias{gamlss_engine_info}
\title{Legacy One-Row Engine Summary}
\usage{gamlss_engine_info(object, estimand = "parameter", what = "mu",
                          population = "observed", weights = NULL)}
\arguments{
\item{object}{A fitted \code{gamlss} or \code{gamlssZadj} model.}
\item{estimand}{Requested estimand.}
\item{what}{Distributional parameter name.}
\item{population}{Target population used for routing diagnostics.}
\item{weights}{Optional weighting rule used for routing diagnostics.}
}
\description{Compatibility wrapper around \code{gamlss_posthoc_plan()}. New code should prefer the richer diagnostic plan.}
\value{A one-row data frame with smoother detection, zero-adjusted status, engine recommendation, and uncertainty recommendation.}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE)) {
  set.seed(17)
  d <- data.frame(x=seq(0,1,length.out=50), trt=factor(rep(c("A","B"),25)))
  d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.3+.4*d$x), sigma=.25)
  fit <- gamlss::gamlss(y~x, sigma.formula=~trt, family=gamlss.dist::GA,
                        data=d, trace=FALSE)
  # 1. mu parameter diagnostics
  gamlss_engine_info(fit, estimand="parameter", what="mu")
  # 2. sigma parameter diagnostics
  gamlss_engine_info(fit, estimand="parameter", what="sigma", population="reference")
  # 3. Full-distribution mean diagnostics
  gamlss_engine_info(fit, estimand="mean", what="mu")
}
}

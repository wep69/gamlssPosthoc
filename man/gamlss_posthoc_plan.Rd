\name{gamlss_posthoc_plan}
\alias{gamlss_posthoc_plan}
\title{Diagnose Post-Hoc Capabilities for a Fitted GAMLSS Model}
\usage{
gamlss_posthoc_plan(
  object,
  estimand = "parameter",
  what = "mu",
  contrast = "none",
  comparison = "difference",
  population = "observed",
  weights = NULL,
  uncertainty = "auto"
)
}
\arguments{
\item{object}{A fitted \code{gamlss} or \code{gamlssZadj} model.}
\item{estimand}{Requested estimand accepted by \code{gamlss_posthoc()}.}
\item{what}{Distributional parameter for a parameter-wise estimand.}
\item{contrast}{Requested level-comparison geometry.}
\item{comparison}{Scientific contrast scale.}
\item{population}{Target standardization population.}
\item{weights}{Weighting rule used to determine engine eligibility. Numeric observation weights rule out the \pkg{emmeans} reference-grid engine.}
\item{uncertainty}{Requested uncertainty layer.}
}
\description{Inspects model class, family, dynamically discovered parameters, smooth terms, optional dependencies, engine eligibility, and the requested estimand before inference is run.}
\value{An object of class \code{gamlss_posthoc_plan} containing overview, parameter table, per-parameter capability matrix, dependencies, engine capability table, request, recommendation, and warnings.}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE)) {
  set.seed(13)
  d <- data.frame(x = seq(0, 1, length.out = 60),
                  trt = factor(rep(c("A", "B"), 30)))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(.5 + .4*d$x + .15*(d$trt=="B")), sigma=.25)
  fit <- gamlss::gamlss(y ~ trt + x, sigma.formula = ~ trt,
                        family = gamlss.dist::GA, data = d, trace = FALSE)
  # 1. Marginal response mean on the observed population
  gamlss_posthoc_plan(fit, estimand = "mean", population = "observed")
  # 2. Parameter-wise reference-grid inference
  gamlss_posthoc_plan(fit, estimand = "parameter", what = "sigma",
                      population = "reference", uncertainty = "delta")
  # 3. Distribution-derived variance
  gamlss_posthoc_plan(fit, estimand = "variance", contrast = "pairwise")
}
}

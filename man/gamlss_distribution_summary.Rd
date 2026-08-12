\name{gamlss_distribution_summary}
\alias{gamlss_distribution_summary}
\title{Summarize Full Predicted Distributions from GAMLSS Models}
\usage{
gamlss_distribution_summary(
  object,
  newdata,
  probs = c(0.025, 0.5, 0.975),
  data = NULL,
  positive_dist_fun = NULL
)
}
\arguments{
\item{object}{A fitted \code{gamlss} or \code{gamlssZadj} model.}
\item{newdata}{Prediction data frame.}
\item{probs}{Finite probabilities between zero and one.}
\item{data}{Optional original model data.}
\item{positive_dist_fun}{Optional positive-part distribution constructor for zero-adjusted models that cannot be reconstructed generically.}
}
\description{
Returns plug-in mean, variance, exact probability mass at zero, standard deviation, and quantiles from the complete predicted distribution. Zero-adjusted models are handled as a point mass at zero plus a positive \pkg{distributions3} distribution.
}
\value{A data frame containing \code{newdata} and distribution summaries.}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE) &&
    requireNamespace("distributions3", quietly = TRUE)) {
  set.seed(12)
  d <- data.frame(x = seq(0.2, 2, length.out = 60))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.1 + 0.35*d$x), sigma = 0.28)
  fit <- gamlss::gamlss(y ~ x, data = d, family = gamlss.dist::GA, trace = FALSE)
  # 1. Default 2.5%, 50%, and 97.5% quantiles
  gamlss_distribution_summary(fit, data.frame(x = c(.5, 1, 1.5)), data = d)
  # 2. Agronomic lower/median/upper predictive summaries
  gamlss_distribution_summary(fit, data.frame(x = c(.75, 1.25)),
                              probs = c(.10, .50, .90), data = d)
  # 3. A single prediction profile
  gamlss_distribution_summary(fit, data.frame(x = 1), probs = c(.25,.5,.75), data = d)
}
}

\name{gamlss_posthoc}
\alias{gamlss_posthoc}
\title{Reliable Marginal and Distributional Post-Hoc Inference for GAMLSS Models}
\usage{
gamlss_posthoc(
  object,
  specs,
  by = NULL,
  estimand = c("parameter", "mean", "variance", "quantile", "prob_zero", "custom"),
  what = "mu",
  contrast = c("none", "pairwise", "reference", "sequential", "poly", "opoly"),
  comparison = c("difference", "ratio", "log_ratio", "percent_change"),
  adjust = "tukey",
  at = list(),
  population = NULL,
  weights = NULL,
  grid = NULL,
  engine = c("auto", "emmeans", "marginaleffects", "distribution"),
  uncertainty = c("auto", "delta", "simulation", "bootstrap", "none"),
  bootstrap = c("parametric", "case", "cluster"),
  B = 499L,
  cluster = NULL,
  ref = 1L,
  scores = NULL,
  degree = NULL,
  prob = 0.5,
  custom_fun = NULL,
  positive_dist_fun = NULL,
  positive_mean_fun = NULL,
  data = NULL,
  refit_fun = NULL,
  simulate_fun = NULL,
  parallel = FALSE,
  cores = 2L,
  seed = 1234,
  keep_draws = FALSE,
  ...
)
}
\arguments{
\item{object}{A fitted \code{gamlss} or \code{gamlssZadj} model.}
\item{specs}{Character vector of focal variables.}
\item{by}{Optional conditioning variables.}
\item{estimand}{Statistical target: a distributional parameter, response mean, variance, quantile, exact zero mass, or custom function.}
\item{what}{Distributional parameter for \code{estimand = "parameter"}. Available parameters are discovered from the fitted object.}
\item{contrast}{Which levels are compared: none, pairwise, reference, sequential, polynomial, or orthogonal polynomial.}
\item{comparison}{Scientific effect scale for ordinary level contrasts: arithmetic difference, ratio, log-ratio, or percentage change.}
\item{adjust}{Multiplicity adjustment.}
\item{at}{Named list of evaluation values.}
\item{population}{Target population for standardization: \code{"observed"}, \code{"balanced"}, or \code{"reference"}.}
\item{weights}{\code{"proportional"}, \code{"equal"}, or one non-negative numeric weight per original data row.}
\item{grid}{Deprecated compatibility alias for \code{population}.}
\item{engine}{Inference engine: automatic routing, \pkg{emmeans}, \pkg{marginaleffects}, or full-distribution prediction.}
\item{uncertainty}{Uncertainty layer: automatic, delta method, coefficient simulation where supported, refit bootstrap, or none.}
\item{bootstrap}{Parametric, case, or cluster bootstrap.}
\item{B}{Number of simulation draws or bootstrap refits.}
\item{cluster}{Cluster column for cluster bootstrap.}
\item{ref}{Reference-level index.}
\item{scores}{Numeric scores for orthogonal polynomial contrasts.}
\item{degree}{Maximum polynomial contrast degree.}
\item{prob}{Probability used for a quantile estimand.}
\item{custom_fun}{Function of \code{pars}, \code{newdata}, and \code{object} returning one value per prediction row.}
\item{positive_dist_fun}{Optional constructor for the positive-part \pkg{distributions3} object when a zero-adjusted family cannot be reconstructed by \code{gamlss.dist::GAMLSS()}.}
\item{positive_mean_fun}{Deprecated compatibility hook. Prefer \code{positive_dist_fun}.}
\item{data}{Original model data. Explicit supply is strongly recommended.}
\item{refit_fun}{Optional refit function \code{function(object, boot_data)}.}
\item{simulate_fun}{Optional custom response simulator for parametric bootstrap.}
\item{parallel}{Use optional \pkg{future.apply} parallel bootstrap.}
\item{cores}{Number of workers; values above two are reduced to two.}
\item{seed}{Random seed.}
\item{keep_draws}{Retain successful bootstrap draws.}
\item{...}{Additional arguments passed to compatible external engines.}
}
\description{
Separates the estimand, target population, contrast geometry, scientific effect scale, and uncertainty method. Supported simple parameter-wise requests can use \pkg{emmeans} or \pkg{marginaleffects}; zero-adjusted and distribution-derived targets use full predictive distributions.
}
\details{
For a generic zero-adjusted model with zero probability \eqn{h} and positive-part distribution \eqn{Y_+}, the package computes
\deqn{E(Y)=(1-h)E(Y_+)}
and
\deqn{Var(Y)=(1-h)Var(Y_+)+h(1-h)E(Y_+)^2.}
Quantiles are also obtained from the complete two-part distribution. Consequently, no assumption that a parameter named \code{mu} is the mathematical expectation is required.

\code{comparison} is deliberately distinct from \code{contrast}: the former defines the scientific scale of an effect, while the latter defines which levels are compared.
}
\value{
An object of class \code{gamlss_posthoc} with estimates, contrasts, formal estimand metadata, engine and uncertainty information, and optionally bootstrap draws.
}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE)) {
  set.seed(11)
  d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = 20)))
  d$y <- gamlss.dist::rGA(nrow(d),
    mu = c(A = 2, B = 2.5, C = 3.0)[d$trt], sigma = 0.25)
  fit <- gamlss::gamlss(y ~ trt, sigma.formula = ~ trt, data = d,
                        family = gamlss.dist::GA, trace = FALSE)
  # 1. Difference between marginal response means
  gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise",
                 comparison = "difference", uncertainty = "none", data = d)
  # 2. Ratios relative to the first treatment
  gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "reference",
                 comparison = "ratio", uncertainty = "none", data = d)
  # 3. Compare the distributional sigma parameter on a reference grid
  gamlss_posthoc(fit, "trt", estimand = "parameter", what = "sigma",
                 population = "reference", contrast = "none",
                 uncertainty = "none", data = d)
}
}

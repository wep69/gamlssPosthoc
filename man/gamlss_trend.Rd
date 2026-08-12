\name{gamlss_trend}
\alias{gamlss_trend}
\title{Quantitative Trends, Derivatives, Turning Points, and Optima from GAMLSS}
\usage{
gamlss_trend(
  object,
  x,
  by = NULL,
  at_x = NULL,
  n = 100L,
  method = c("curve", "derivative", "turning_points", "optimum"),
  derivative_order = 1L,
  optimum = c("maximum", "minimum"),
  band = c("pointwise", "simultaneous"),
  estimand = "mean",
  what = "mu",
  engine = c("auto", "marginaleffects", "distribution"),
  population = NULL,
  weights = NULL,
  grid = NULL,
  data = NULL,
  uncertainty = c("auto", "delta", "simulation", "bootstrap", "none"),
  bootstrap = "case",
  B = 499L,
  cluster = NULL,
  custom_fun = NULL,
  positive_dist_fun = NULL,
  refit_fun = NULL,
  simulate_fun = NULL,
  parallel = FALSE,
  cores = 2L,
  seed = 1234
)
}
\arguments{
\item{object}{Fitted GAMLSS model.}
\item{x}{Name of a numeric predictor.}
\item{by}{Optional grouping variables.}
\item{at_x}{Numeric evaluation grid.}
\item{n}{Number of grid points when \code{at_x} is null.}
\item{method}{Curve, derivative, turning points, or optimum.}
\item{derivative_order}{First or second derivative.}
\item{optimum}{Maximum or minimum when \code{method="optimum"}.}
\item{band}{Pointwise or simultaneous bootstrap band.}
\item{estimand}{Distributional estimand passed to \code{gamlss_posthoc()}.}
\item{what}{Distributional parameter for parameter-wise trends.}
\item{engine}{Automatic, marginaleffects, or distributional trend engine. First derivatives of ordinary parameters can use \code{marginaleffects::avg_slopes()}.}
\item{population}{Target standardization population.}
\item{weights}{Weighting rule passed to \code{gamlss_posthoc()}.}
\item{grid}{Deprecated alias for \code{population}.}
\item{data}{Original model data.}
\item{uncertainty}{Automatic, bootstrap, or no interval uncertainty.}
\item{bootstrap}{Bootstrap type.}
\item{B}{Number of bootstrap refits.}
\item{cluster}{Cluster column for cluster bootstrap.}
\item{custom_fun}{Optional custom estimand function.}
\item{positive_dist_fun}{Optional positive-part distribution constructor for zero-adjusted models.}
\item{refit_fun}{Optional model-refit function.}
\item{simulate_fun}{Optional custom response simulator.}
\item{parallel}{Use optional parallel bootstrap.}
\item{cores}{Number of workers; capped at two.}
\item{seed}{Random seed.}
}
\description{Evaluates an estimand over a numeric predictor and can derive first/second finite-difference derivatives, detect sign-changing stationary points, estimate extrema locally refined around an interior grid optimum, and construct simultaneous bootstrap bands.}
\value{An object of class \code{gamlss_trend}. The \code{special_points} component contains turning points or extrema when requested.}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE) &&
    requireNamespace("distributions3", quietly = TRUE)) {
  set.seed(14)
  d <- data.frame(dose = rep(seq(0, 120, length.out = 10), each = 8))
  mu <- exp(.4 + .015*d$dose - .00007*d$dose^2)
  d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = .22)
  fit <- gamlss::gamlss(y ~ dose + I(dose^2), family = gamlss.dist::GA,
                        data = d, trace = FALSE)
  # 1. Predicted response-mean curve
  gamlss_trend(fit, "dose", method = "curve", n = 25,
               uncertainty = "none", data = d)
  # 2. First derivative of the mean curve
  gamlss_trend(fit, "dose", method = "derivative", derivative_order = 1,
               n = 25, uncertainty = "none", data = d)
  # 3. Refined maximum on the supplied dose range
  gamlss_trend(fit, "dose", method = "optimum", optimum = "maximum",
               n = 25, uncertainty = "none", data = d)
}
}

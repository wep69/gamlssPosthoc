% Manually synchronized with Roxygen source for v0.3.0
\name{gamlss_plot_data}
\alias{gamlss_plot_data}
\title{Build standardized data for GAMLSS graphics}
\usage{
gamlss_plot_data(object, type = c("parameters", "estimand", "contrasts", "distribution",
"quantiles", "zero_adjusted", "trend", "derivative",
                                              "optimum", "surface"),
x = NULL, y = NULL, by = NULL, parameter = "mu", parameters = NULL,
                             parameter_scale = c("response", "link"),
estimand = "mean", what = "mu", at = list(), newdata = NULL,
population = "observed", weights = NULL, probs = c(.05,.25,.5,.75,.95),
curve = c("density","cdf","survival"), response_grid = NULL,
n = 100L, n_response = 200L, uncertainty = "none", B = 199L,
                             positive_dist_fun = NULL, data = NULL, ...)
}
\arguments{
  \item{object}{Fitted GAMLSS model or a \code{gamlss_posthoc}/\code{gamlss_trend} object for compatible types.}
  \item{type}{Plot-data type: \code{parameters}, \code{estimand}, \code{contrasts}, \code{distribution},}
  \item{x}{Predictor names. \code{y} is used only for surfaces.}
  \item{y}{Predictor names. \code{y} is used only for surfaces.}
  \item{by}{Optional conditioning variables.}
  \item{parameter}{Distributional parameter(s).}
  \item{parameters}{Distributional parameter(s).}
  \item{parameter_scale}{Parameter prediction scale, \code{response} or \code{link}.}
  \item{estimand}{Estimand and parameter passed to post-hoc/trend engines.}
  \item{what}{Estimand and parameter passed to post-hoc/trend engines.}
  \item{at}{Named list of evaluation values.}
  \item{newdata}{Optional explicit prediction rows.}
  \item{population}{Standardization population and weights.}
  \item{weights}{Standardization population and weights.}
  \item{probs}{Quantile probabilities.}
  \item{curve}{Distribution curve type: density, CDF, or survival.}
  \item{response_grid}{Optional response grid for distribution curves.}
  \item{n}{Number of predictor grid points.}
  \item{n_response}{Number of response grid points.}
  \item{uncertainty}{Bootstrap/inference controls forwarded when relevant.}
  \item{B}{Bootstrap/inference controls forwarded when relevant.}
  \item{positive_dist_fun}{Optional positive-distribution constructor for zero-adjusted models.}
  \item{data}{Optional original data.}
  \item{...}{Additional arguments forwarded to the underlying post-hoc or trend function.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Creates the auditable data layer used by the graphical API. Statistical calculation is separated from ggplot construction so users can inspect, test, export, or customize every plotted quantity.
}
\details{
The graphical API follows a data-first design: the statistical quantity is calculated before the plot is built. This makes estimands, standardization, contrast scales, and uncertainty auditable and allows the returned ggplot object to be modified with ordinary ggplot2 layers.
}
\references{
Rigby RA, Stasinopoulos DM (2005). Generalized additive models for location, scale and shape. \emph{JRSS C}, 54, 507--554. \doi{10.1111/j.1467-9876.2005.00510.x}

Arel-Bundock V, Greifer N, Heiss A (2024). How to Interpret Statistical Models Using marginaleffects for R and Python. \emph{Journal of Statistical Software}, 111(9), 1--32. \doi{10.18637/jss.v111.i09}

Kay M (2024). ggdist: Visualizations of Distributions and Uncertainty in the Grammar of Graphics. \emph{IEEE TVCG}, 30(1), 414--424. \doi{10.1109/TVCG.2023.3327195}
}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE)) {
  set.seed(310)
  d <- data.frame(
    dose = rep(seq(0, 120, length.out = 8), each = 8),
    trt = factor(rep(c("A", "B"), length.out = 64))
  )
  mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
  sig <- exp(log(0.22) + 0.001 * d$dose)
  d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
  fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
                        sigma.formula = ~ dose,
                        family = gamlss.dist::GA, data = d, trace = FALSE)
p1 <- gamlss_plot_data(fit, "parameters", x = "dose", parameters = c("mu", "sigma"), data = d,
                         n = 12)
p2 <- gamlss_plot_data(fit, "distribution", x = "trt", curve = "density", data = d,
                         n_response = 40)
  p3 <- gamlss_plot_data(fit, "quantiles", x = "dose", probs = c(.1, .5, .9), data = d, n = 12)
}
}

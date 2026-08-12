% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_distribution}
\alias{plot_gamlss_distribution}
\title{Plot standardized predictive distributions}
\usage{
plot_gamlss_distribution(object,x,by=NULL,type=c("density","cdf","survival"),at=list(),
                         newdata=NULL,population="observed",weights=NULL,n=100L,
                         n_response=200L,data=NULL,...)
}
\arguments{
  \item{object}{Fitted model.}
  \item{x}{Grouping/predictor variable.}
  \item{by}{Optional conditioning variables.}
  \item{type}{Density, CDF, or survival curve.}
  \item{at}{Prediction standardization controls.}
  \item{newdata}{Prediction standardization controls.}
  \item{population}{Prediction standardization controls.}
  \item{weights}{Prediction standardization controls.}
  \item{n}{Grid sizes.}
  \item{n_response}{Grid sizes.}
  \item{data}{Original data.}
  \item{...}{Additional arguments.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Plots standardized predictive density, cumulative distribution, or survival curves. For zero-adjusted models a point mass at zero is represented separately from the positive density.
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
  p1 <- plot_gamlss_distribution(fit, "trt", type = "density", data = d, n_response = 40)
  p2 <- plot_gamlss_distribution(fit, "trt", type = "cdf", data = d, n_response = 40)
p3 <- plot_gamlss_distribution(fit, "dose", at = list(dose = c(0, 60, 120)), type = "survival",
                                 data = d, n_response = 40)
}
}

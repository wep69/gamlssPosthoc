% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_quantiles}
\alias{plot_gamlss_quantiles}
\title{Plot predictive quantiles and quantile fans}
\usage{
plot_gamlss_quantiles(object,x,probs=c(.05,.1,.25,.5,.75,.9,.95),by=NULL,
                      style=c("fan","lines"),n=100L,data=NULL,...)
}
\arguments{
  \item{object}{Fitted model.}
  \item{x}{Predictor.}
  \item{probs}{Quantile probabilities.}
  \item{by}{Optional group.}
  \item{style}{\code{lines} or \code{fan}.}
  \item{n}{Grid size.}
  \item{data}{Original data.}
  \item{...}{Additional arguments.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Plots quantile curves or nested fan bands. Quantiles are calculated from the standardized predictive mixture, rather than by averaging conditional quantiles.
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
  p1 <- plot_gamlss_quantiles(fit, "dose", style = "fan", data = d, n = 12)
p2 <- plot_gamlss_quantiles(fit, "dose", probs = c(.1, .5, .9), style = "lines", data = d, n = 12)
  p3 <- plot_gamlss_quantiles(fit, "dose", by = "trt", style = "fan", data = d, n = 12)
}
}

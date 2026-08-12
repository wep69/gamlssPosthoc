% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_diagnostics}
\alias{plot_gamlss_diagnostics}
\title{Plot GAMLSS diagnostics with ggplot2}
\usage{
plot_gamlss_diagnostics(object,
    type=c("residual_fitted","qq","worm","density","pit","rootogram","all"),
    bins=20L,nsim=100L,engine=c("native"),data=NULL)
}
\arguments{
  \item{object}{Fitted model.}
  \item{type}{One of residual_fitted, qq, worm, density, pit, rootogram, or all.}
  \item{bins}{Number of bins.}
  \item{nsim}{Simulations for rootogram.}
  \item{engine}{Diagnostic engine; currently \code{native}, implemented with public \pkg{ggplot2} APIs.}
  \item{data}{Original data.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Produces quantile-residual diagnostics, PIT displays, and rootograms using the package native \pkg{ggplot2} implementation.
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
  p1 <- plot_gamlss_diagnostics(fit, "qq", engine = "native", data = d)
  p2 <- plot_gamlss_diagnostics(fit, "worm", engine = "native", data = d)
  p3 <- plot_gamlss_diagnostics(fit, "density", engine = "native", data = d)
}
}

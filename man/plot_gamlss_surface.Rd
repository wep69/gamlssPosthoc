% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_surface}
\alias{plot_gamlss_surface}
\title{Plot two-predictor GAMLSS surfaces}
\usage{
plot_gamlss_surface(object,x,y,estimand="mean",what="mu",
                    type=c("heatmap","contour","both"),n=45L,data=NULL,...)
}
\arguments{
  \item{object}{Fitted model.}
  \item{x}{Numeric predictors.}
  \item{y}{Numeric predictors.}
  \item{estimand}{Target estimand.}
  \item{what}{Target estimand.}
  \item{type}{Heatmap, contour, or both.}
  \item{n}{Approximate grid density.}
  \item{data}{Original data.}
  \item{...}{Additional arguments.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Creates heatmap, contour, or combined representations of a declared estimand over two quantitative predictors.
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
  set.seed(311)
  d <- expand.grid(dose = seq(0, 100, length.out = 8), salinity = seq(0, 3, length.out = 6))
  d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.5 + 0.01*d$dose - 0.18*d$salinity), sigma = 0.25)
  fit <- gamlss::gamlss(y ~ dose * salinity, family = gamlss.dist::GA, data = d, trace = FALSE)
  p1 <- plot_gamlss_surface(fit, "dose", "salinity", type = "heatmap", data = d, n = 16)
  p2 <- plot_gamlss_surface(fit, "dose", "salinity", type = "contour", data = d, n = 16)
p3 <- plot_gamlss_surface(fit, "dose", "salinity", estimand = "variance", type = "both", data = d,
                            n = 16)
}
}

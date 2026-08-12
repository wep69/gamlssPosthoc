% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_zero_adjusted}
\alias{plot_gamlss_zero_adjusted}
\title{Plot zero-adjusted model components}
\usage{
plot_gamlss_zero_adjusted(object,x,by=NULL,style=c("facets","normalized"),n=100L,
                          data=NULL,...)
}
\arguments{
  \item{object}{Fitted \code{gamlssZadj} model.}
  \item{x}{Predictor/group.}
  \item{by}{Optional conditioning variables.}
  \item{style}{\code{facets} or \code{normalized}.}
  \item{n}{Grid size.}
  \item{data}{Original data.}
  \item{...}{Additional arguments.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Displays the zero probability, positive-part mean, and marginal response mean so the two-part mechanism remains interpretable.
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
    requireNamespace("gamlss.dist", quietly = TRUE) &&
    requireNamespace("gamlss.inf", quietly = TRUE) &&
    requireNamespace("distributions3", quietly = TRUE)) {
  # gamlss.inf resolves pGA() by name from the search path.
  library(gamlss.dist)
  set.seed(312)
  d <- data.frame(trt = factor(rep(c("A", "B"), each = 45)),
                  dose = rep(seq(0, 80, length.out = 9), 10))
  mu <- exp(0.2 + 0.012*d$dose + 0.25*(d$trt == "B"))
  h <- plogis(0.2 - 0.7*(d$trt == "B") - 0.01*d$dose)
  yp <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.35)
  d$y <- ifelse(stats::rbinom(nrow(d), 1, h) == 1, 0, yp)
  fit <- gamlss.inf::gamlssZadj(y = y, mu.formula = ~ trt + dose,
                                sigma.formula = ~ 1,
                                xi0.formula = ~ trt + dose,
                                family = gamlss.dist::GA, data = d, trace = FALSE)
  p1 <- plot_gamlss_zero_adjusted(fit, "trt", data = d)
  p2 <- plot_gamlss_zero_adjusted(fit, "dose", style = "facets", data = d, n = 12)
  p3 <- plot_gamlss_zero_adjusted(fit, "dose", by = "trt", style = "normalized", data = d, n = 12)
}
}

% Manually synchronized with Roxygen source for v0.3.0
\name{plot_gamlss_compare}
\alias{plot_gamlss_compare}
\title{Compare fitted GAMLSS models graphically}
\usage{
plot_gamlss_compare(...,type=c("criteria","estimand"),
                    criterion=c("AIC","global_deviance","df","all"),x=NULL,
                    estimand="mean",what="mu",data=NULL)
}
\arguments{
  \item{...}{Named fitted GAMLSS models.}
  \item{type}{\code{criteria} or \code{estimand}.}
  \item{criterion}{Criterion for \code{type="criteria"}: \code{AIC}, \code{global_deviance}, \code{df}, or \code{all}.}
  \item{x}{Predictor for estimand comparison.}
  \item{estimand}{Target estimand.}
  \item{what}{Target estimand.}
  \item{data}{Optional original data.}
}
\value{
See the function description. Graphical functions return a \code{ggplot} object; data and export functions return the documented data or file representation.
}
\description{
Compares model criteria or declared estimand trajectories across fitted GAMLSS models.
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
  fit2 <- gamlss::gamlss(y ~ trt + dose, family = gamlss.dist::GA, data = d, trace = FALSE)
  p1 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "criteria")
p2 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose", data = d)
p3 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose",
                            estimand = "variance", data = d)
}
}

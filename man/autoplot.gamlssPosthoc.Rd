\name{autoplot.gamlssPosthoc}
\alias{plot.gamlss_posthoc}
\alias{plot.gamlss_trend}
\alias{plot.gamlss_distribution_summary}
\alias{plot.gamlss_posthoc_plan}
\alias{autoplot.gamlss_posthoc}
\alias{autoplot.gamlss_trend}
\alias{autoplot.gamlss_distribution_summary}
\alias{autoplot.gamlss_posthoc_plan}
\title{Plot and Autoplot Methods for gamlssPosthoc Result Classes}
\description{Creates complete, modifiable \pkg{ggplot2} graphics from result classes owned by \pkg{gamlssPosthoc}. Base \code{plot()} methods print and invisibly return the corresponding \code{autoplot()} graphic.}
\usage{
\method{autoplot}{gamlss_posthoc}(object, ...)
\method{autoplot}{gamlss_trend}(object, ...)
\method{autoplot}{gamlss_distribution_summary}(object, x = NULL, ...)
\method{autoplot}{gamlss_posthoc_plan}(object, ...)
\method{plot}{gamlss_posthoc}(x, ...)
\method{plot}{gamlss_trend}(x, ...)
\method{plot}{gamlss_distribution_summary}(x, ...)
\method{plot}{gamlss_posthoc_plan}(x, ...)
}
\arguments{
\item{object}{Result object for an \code{autoplot()} method.}
\item{x}{Result object for a \code{plot()} method; for distribution-summary autoplot, \code{x} may also name the horizontal-axis column.}
\item{...}{Additional graphical arguments forwarded to the specialized constructor when supported.}
}
\value{\code{autoplot()} returns a \code{ggplot}; \code{plot()} prints it and returns it invisibly.}
\examples{
if (requireNamespace("gamlss", quietly=TRUE) &&
    requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(331)
  d <- data.frame(dose=rep(seq(0,100,length.out=8),each=6),
                  trt=factor(rep(c("A","B"),length.out=48)))
  d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.4+.01*d$dose+.15*(d$trt=="B")), sigma=.22)
  fit <- gamlss::gamlss(y~trt+dose, family=gamlss.dist::GA, data=d, trace=FALSE)
  ph <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  tr <- gamlss_trend(fit,"dose",n=15,uncertainty="none",data=d)
  ds <- gamlss_distribution_summary(fit,
    data.frame(dose=c(25,75),trt=factor(c("A","B"),levels=levels(d$trt))),data=d)
  ggplot2::autoplot(ph)
  ggplot2::autoplot(tr)
  ggplot2::autoplot(ds, x="dose")
}
}

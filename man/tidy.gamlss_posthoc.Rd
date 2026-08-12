\name{tidy.gamlss_posthoc}
\alias{tidy.gamlss_posthoc}
\title{Tidy a gamlssPosthoc Result}
\usage{
\method{tidy}{gamlss_posthoc}(x, component = c("estimates", "contrasts"),
                              conf.int = TRUE, ...)
}
\arguments{
\item{x}{A \code{gamlss_posthoc} object.}
\item{component}{Estimates or contrasts.}
\item{conf.int}{Keep confidence intervals when present.}
\item{...}{Unused.}
}
\description{Interoperability method for \pkg{gamlssPosthoc} result objects.}
\value{A tidy data frame.}
\examples{
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(330)
  d <- data.frame(trt=factor(rep(c("A","B"),each=20)))
  d$y <- gamlss.dist::rGA(nrow(d),mu=ifelse(d$trt=="A",2,2.7),sigma=.22)
  fit <- gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  generics::tidy(result)
  generics::tidy(result,component="contrasts")
  generics::tidy(result,conf.int=FALSE)
}
}

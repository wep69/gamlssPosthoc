\name{glance.gamlss_posthoc}
\alias{glance.gamlss_posthoc}
\title{Glance at a gamlssPosthoc Result}
\usage{
\method{glance}{gamlss_posthoc}(x, ...)
}
\arguments{
\item{x}{A \code{gamlss_posthoc} object.}
\item{...}{Unused.}
}
\description{Interoperability method for \pkg{gamlssPosthoc} result objects.}
\value{A one-row data frame.}
\examples{
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(330)
  d <- data.frame(trt=factor(rep(c("A","B"),each=20)))
  d$y <- gamlss.dist::rGA(nrow(d),mu=ifelse(d$trt=="A",2,2.7),sigma=.22)
  fit <- gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  generics::glance(result)
  as.data.frame(generics::glance(result))
  generics::glance(result)[,c("engine","estimand")]
}
}

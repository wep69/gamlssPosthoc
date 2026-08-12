\name{augment.gamlss_posthoc}
\alias{augment.gamlss_posthoc}
\title{Augment Data with Row-Level Fitted Estimands}
\usage{
\method{augment}{gamlss_posthoc}(x, data = NULL, ...)
}
\arguments{
\item{x}{A \code{gamlss_posthoc} object.}
\item{data}{Optional data; defaults to retained data.}
\item{...}{Unused.}
}
\description{Interoperability method for \pkg{gamlssPosthoc} result objects.}
\value{Original data plus fitted estimand and, when available, residuals.}
\examples{
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(330)
  d <- data.frame(trt=factor(rep(c("A","B"),each=20)))
  d$y <- gamlss.dist::rGA(nrow(d),mu=ifelse(d$trt=="A",2,2.7),sigma=.22)
  fit <- gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  head(generics::augment(result))
  head(generics::augment(result,data=d),3)
  summary(generics::augment(result)$.fitted_estimand)
}
}

\name{model_parameters.gamlss_posthoc}
\alias{model_parameters.gamlss_posthoc}
\title{Extract Parameters in easystats Style}
\usage{
\method{model_parameters}{gamlss_posthoc}(model, ...)
}
\arguments{
\item{model}{A \code{gamlss_posthoc} object.}
\item{...}{Unused.}
}
\description{Interoperability method for \pkg{gamlssPosthoc} result objects.}
\value{A data frame compatible with parameters conventions.}
\examples{
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(330)
  d <- data.frame(trt=factor(rep(c("A","B"),each=20)))
  d$y <- gamlss.dist::rGA(nrow(d),mu=ifelse(d$trt=="A",2,2.7),sigma=.22)
  fit <- gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  if (requireNamespace("parameters",quietly=TRUE)) parameters::model_parameters(result)
if (requireNamespace("parameters",
      quietly=TRUE)) as.data.frame(parameters::model_parameters(result))
if (requireNamespace("parameters",quietly=TRUE)) parameters::model_parameters(result)[,
      c("Parameter","Coefficient")]
}
}

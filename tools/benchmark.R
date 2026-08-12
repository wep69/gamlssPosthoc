# Development benchmark for gamlssPosthoc. Not executed on package load/check.
if (!requireNamespace("gamlss", quietly=TRUE) || !requireNamespace("gamlss.dist", quietly=TRUE)) stop("Install gamlss and gamlss.dist")
library(gamlssPosthoc)
set.seed(3001)
d <- data.frame(trt=factor(rep(LETTERS[1:3],each=80)), x=runif(240,0,2))
d$y <- gamlss.dist::rGA(nrow(d),mu=exp(.8+.3*d$x+.15*(d$trt=="B")+.3*(d$trt=="C")),sigma=.25)
fit <- gamlss::gamlss(y~trt+x,family=gamlss.dist::GA,data=d,trace=FALSE)
bench <- function(expr,n=5) { z<-replicate(n,system.time(force(expr))["elapsed"]); c(mean=mean(z),median=median(z),min=min(z),max=max(z)) }
res <- list(
 distribution=bench(gamlss_posthoc(fit,"trt",estimand="mean",engine="distribution",uncertainty="none",data=d)),
 emmeans=if(requireNamespace("emmeans",quietly=TRUE))bench(gamlss_posthoc(fit,"trt",estimand="parameter",what="mu",population="reference",engine="emmeans",data=d)) else NA,
 marginaleffects=if(requireNamespace("marginaleffects",quietly=TRUE))bench(gamlss_posthoc(fit,"trt",estimand="parameter",what="mu",engine="marginaleffects",data=d)) else NA
)
print(res)
# Bootstrap cost (use B=19 only for timing; scientific work needs larger B).
print(system.time(gamlss_posthoc(fit,"trt",estimand="mean",uncertainty="bootstrap",bootstrap="case",B=19,data=d)))

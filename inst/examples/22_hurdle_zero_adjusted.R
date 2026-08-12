# Continuous zero-adjusted workflow.
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("gamlss.inf", quietly=TRUE) && requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(2203)
  d <- data.frame(trt=factor(rep(c("Control","Bio"),each=60)))
  mu <- ifelse(d$trt=="Control",2,3); h <- ifelse(d$trt=="Control",.45,.18)
  yp <- gamlss.dist::rGA(nrow(d),mu=mu,sigma=.28)
  d$y <- ifelse(rbinom(nrow(d),1,h)==1,0,yp)
  fit <- gamlss.inf::gamlssZadj(y=y,mu.formula=~trt,xi0.formula=~trt,
                                family=gamlss.dist::GA,data=d,trace=FALSE)
  plot_gamlss_zero_adjusted(fit,"trt",data=d)
  gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
                 uncertainty="none",data=d)$contrasts
}

# Longitudinal GAMLSS workflow with a subject-level random intercept smoother.
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE)) {
  set.seed(2103)
  d <- expand.grid(subject=factor(1:24), time=0:5)
  u <- rnorm(24,0,.18); d$trt <- factor(rep(rep(c("A","B"),each=12),each=6))
  mu <- exp(.4+.10*d$time+.12*(d$trt=="B")+u[as.integer(d$subject)])
  d$y <- gamlss.dist::rGA(nrow(d),mu=mu,sigma=.20)
  fit <- gamlss::gamlss(y ~ trt*time + gamlss::random(subject),
                        family=gamlss.dist::GA,data=d,trace=FALSE)
  gamlss_posthoc_plan(fit,estimand="mean")
  plot_gamlss_estimand(fit,"time",by="trt",estimand="mean",data=d,n=20)
}

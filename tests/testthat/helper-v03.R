.make_v03_fit <- function() {
  testthat::skip_if_not_installed("gamlss")
  testthat::skip_if_not_installed("gamlss.dist")
  set.seed(303)
  d <- data.frame(trt=factor(rep(c("A","B"), each=30)), dose=rep(seq(0,100,length.out=10),6))
  d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.5+.01*d$dose+.2*(d$trt=="B")), sigma=.25)
  fit <- gamlss::gamlss(y~trt+dose, sigma.formula=~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
  list(fit=fit,d=d)
}

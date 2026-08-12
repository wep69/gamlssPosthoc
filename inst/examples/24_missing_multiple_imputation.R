# Multiple imputation + GAMLSS + Rubin pooling for an approximately normal contrast.
if (requireNamespace("mice", quietly=TRUE) && requireNamespace("gamlss", quietly=TRUE) &&
    requireNamespace("gamlss.dist", quietly=TRUE) && requireNamespace("emmeans", quietly=TRUE)) {
  set.seed(2403)
  d <- data.frame(x=rnorm(100),trt=factor(rep(c("A","B"),50)))
  d$y <- gamlss.dist::rGA(100,mu=exp(.5+.2*d$x+.18*(d$trt=="B")),sigma=.22)
  d$x[sample(100,15)] <- NA
  imp <- mice::mice(d,m=3,maxit=3,printFlag=FALSE,seed=2403)
  q <- u <- numeric(3)
  for(i in 1:3){
    di<-mice::complete(imp,i)
    fit<-gamlss::gamlss(y~trt+x,family=gamlss.dist::GA,data=di,trace=FALSE)
    co<-gamlss_posthoc(fit,"trt",estimand="parameter",what="mu",
      population="reference",contrast="pairwise",engine="emmeans",uncertainty="delta",data=di)$contrasts[1,]
    q[i]<-co$estimate; u[i]<-co$SE^2
  }
  Qbar<-mean(q); Ubar<-mean(u); B<-stats::var(q); Tvar<-Ubar+(1+1/3)*B
  data.frame(estimate=Qbar,SE=sqrt(Tvar),lower=Qbar-1.96*sqrt(Tvar),upper=Qbar+1.96*sqrt(Tvar))
}

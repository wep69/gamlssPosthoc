# Meta-analysis style synthesis of the same GAMLSS contrast across studies.
if (requireNamespace("gamlss", quietly=TRUE) &&
    requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("emmeans", quietly=TRUE)) {
  set.seed(2003)
  dat <- do.call(rbind, lapply(1:4, function(s) {
    d <- data.frame(study=factor(s), trt=factor(rep(c("Control","Bio"),each=35)))
    d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="Control",2,2.4+.12*s), sigma=.24)
    d
  }))
  est <- lapply(split(dat, dat$study), function(d) {
    fit <- gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
    z <- gamlss_posthoc(fit,"trt",estimand="parameter",what="mu",
      population="reference",contrast="pairwise",comparison="difference",
      engine="emmeans",uncertainty="delta",data=d)$contrasts
    z[1,c("estimate","SE")]
  })
  tab <- do.call(rbind,est); w <- 1/tab$SE^2
  pooled <- sum(w*tab$estimate)/sum(w)
  pooled_se <- sqrt(1/sum(w))
  data.frame(effect=pooled,SE=pooled_se,lower=pooled-1.96*pooled_se,upper=pooled+1.96*pooled_se)
}

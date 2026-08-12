# Reporting helpers and website workflow.
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE)) {
  set.seed(2503)
  d<-data.frame(trt=factor(rep(c("A","B"),each=20)))
  d$y<-gamlss.dist::rGA(40,mu=ifelse(d$trt=="A",2,2.6),sigma=.22)
  fit<-gamlss::gamlss(y~trt,family=gamlss.dist::GA,data=d,trace=FALSE)
  res<-gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",uncertainty="none",data=d)
  generate_report(res,output="md",file=tempfile(fileext=".md"))
  export_to_latex(res,tempfile(fileext=".tex"))
  # From a package checkout with pkgdown installed: pkgdown::build_site()
}

# K-fold predictive validation using log score.
if (requireNamespace("gamlss", quietly=TRUE) && requireNamespace("gamlss.dist", quietly=TRUE) &&
    requireNamespace("distributions3", quietly=TRUE)) {
  set.seed(2303)
  d <- data.frame(x=runif(120),trt=factor(sample(c("A","B"),120,TRUE)))
  d$y <- gamlss.dist::rGA(120,mu=exp(.4+.5*d$x+.15*(d$trt=="B")),sigma=.23)
  fold <- sample(rep(1:4,length.out=nrow(d)))
  score <- sapply(1:4,function(k){
    train<-d[fold!=k,]; test<-d[fold==k,]
    fit<-gamlss::gamlss(y~trt+x,family=gamlss.dist::GA,data=train,trace=FALSE)
    D<-gamlss::prodist(fit,newdata=test,data=train)
    -mean(log(pmax(distributions3::pdf(D,test$y),.Machine$double.xmin)))
  })
  data.frame(fold=1:4,negative_log_score=score)
}

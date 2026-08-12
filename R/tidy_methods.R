# Interoperability -----------------------------------------------------------

#' Tidy a gamlssPosthoc result
#' @param x A `gamlss_posthoc` object.
#' @param component `estimates` or `contrasts`.
#' @param conf.int Keep confidence intervals when present.
#' @param ... Unused.
#' @return A tidy data frame.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   generics::tidy(result)
#'   generics::tidy(result, component="contrasts")
#'   generics::tidy(result, conf.int=FALSE)
#' }
tidy.gamlss_posthoc <- function(x,component=c("estimates","contrasts"),conf.int=TRUE,...) {
  component<-match.arg(component); d<-if(component=="estimates")x$estimates else x$contrasts
  if(is.null(d))return(data.frame()); d<-as.data.frame(d)
  if(!conf.int)d<-d[,setdiff(names(d),c("lower.CL","upper.CL")),drop=FALSE]
  attr(d,"estimand")<-x$estimand; attr(d,"component")<-component; d
}

#' Glance at a gamlssPosthoc result
#' @param x A `gamlss_posthoc` object.
#' @param ... Unused.
#' @return One-row data frame.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   generics::glance(result)
#'   as.data.frame(generics::glance(result))
#'   generics::glance(result)[, c("engine","estimand")]
#' }
glance.gamlss_posthoc <- function(x,...) data.frame(engine=x$engine,estimand=x$estimand,population=x$population,weighting=x$weighting,uncertainty=x$uncertainty_method,n_estimates=nrow(x$estimates),n_contrasts=if(is.null(x$contrasts))0L else nrow(x$contrasts),stringsAsFactors=FALSE)

#' Augment data with row-level fitted estimands
#' @param x A `gamlss_posthoc` object.
#' @param data Optional data; defaults to data retained by the result.
#' @param ... Unused.
#' @return Original data plus `.fitted_estimand` and, when possible, `.resid`.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   head(generics::augment(result))
#'   head(generics::augment(result, data=d), 3)
#'   summary(generics::augment(result)$.fitted_estimand)
#' }
augment.gamlss_posthoc <- function(x,data=NULL,...) {
  object<-x$model; data<-data%||%x$data
  if(is.null(object)||is.null(data))stop("The result does not retain model/data; supply `data` and use a 0.3.0 result.",call.=FALSE)
  out<-as.data.frame(data); out$.fitted_estimand<-.gph_predict_estimand(object,out,x$estimand,x$what,data=out)
  resp<-try(all.vars(.gph_get_formula(object,.gph_model_parameters(object)[1]))[1],silent=TRUE); if(!inherits(resp,"try-error")&&resp%in%names(out))out$.resid<-out[[resp]]-out$.fitted_estimand
  out
}

#' Extract parameters in the easystats style
#' @param model A `gamlss_posthoc` object.
#' @param ... Unused.
#' @return Data frame compatible with `parameters` conventions.
#' @exportS3Method parameters::model_parameters
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(330)
#'   d <- data.frame(trt=factor(rep(c("A","B"), each=20)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=ifelse(d$trt=="A",2,2.7), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   result <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                            uncertainty="none",data=d)
#'   if (requireNamespace("parameters", quietly=TRUE)) {
#'     parameters::model_parameters(result)
#'     as.data.frame(parameters::model_parameters(result))
#'     parameters::model_parameters(result)[, c("Parameter","Coefficient")]
#'   }
#' }
model_parameters.gamlss_posthoc <- function(model,...) {
  d<-model$contrasts%||%model$estimates; d<-as.data.frame(d)
  out<-data.frame(Parameter=if("contrast"%in%names(d))d$contrast else seq_len(nrow(d)),Coefficient=d$estimate,stringsAsFactors=FALSE)
  if("SE"%in%names(d))out$SE<-d$SE; if("lower.CL"%in%names(d))out$CI_low<-d$lower.CL; if("upper.CL"%in%names(d))out$CI_high<-d$upper.CL; if("p.value"%in%names(d))out$p<-d$p.value
  class(out)<-c("parameters_model",class(out)); out
}

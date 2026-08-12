# ggplot2 visualization layer -------------------------------------------------

.gph_ggrequire <- function() .gph_require("ggplot2", "for visualization.")
.gph_aes <- function(x=NULL,y=NULL,colour=NULL,fill=NULL,group=NULL) {
  .gph_ggrequire()
  args <- list()
  if (!is.null(x)) args$x <- rlang::sym(x)
  if (!is.null(y)) args$y <- rlang::sym(y)
  if (!is.null(colour)) args$colour <- rlang::sym(colour)
  if (!is.null(fill)) args$fill <- rlang::sym(fill)
  if (!is.null(group)) args$group <- rlang::sym(group)
  do.call(ggplot2::aes, lapply(args, rlang::new_quosure, env=parent.frame()))
}

.gph_first_group <- function(d, exclude=character()) {
  cand <- setdiff(names(d), c(exclude,"estimate","SE","lower.CL","upper.CL","response","value","curve","prob","quantile","parameter","component","comparison","p.value","p.value.adjusted"))
  cand[vapply(d[cand], function(z) is.factor(z)||is.character(z), logical(1))][1L] %||% NULL
}

#' Scientific theme for GAMLSS figures
#' @param base_size Base font size.
#' @param base_family Font family.
#' @param grid Show major horizontal grid lines.
#' @return A complete ggplot2 theme.
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
#'   # theme_gamlss(): three typical uses
#'   p + theme_gamlss()
#'   p + theme_gamlss(base_size = 12)
#'   p + theme_gamlss(grid = FALSE)
#'   # journal variant
#'   p + theme_gamlss_journal()
#'   p + theme_gamlss_journal(base_size = 10)
#'   p + theme_gamlss_journal(grid = TRUE)
#'   # presentation variant
#'   p + theme_gamlss_presentation()
#'   p + theme_gamlss_presentation(base_size = 18)
#'   p + theme_gamlss_presentation(grid = FALSE)
#' }
theme_gamlss <- function(base_size=11, base_family="", grid=TRUE) {
  .gph_ggrequire()
  ggplot2::theme_minimal(base_size=base_size, base_family=base_family) +
    ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
                   panel.grid.major.x=ggplot2::element_blank(),
                   panel.grid.major.y=if (grid) ggplot2::element_line(linewidth=.25) else ggplot2::element_blank(),
                   legend.position="right", plot.title.position="plot",
                   strip.text=ggplot2::element_text(face="bold"),
                   axis.title=ggplot2::element_text(face="bold"))
}
#' @rdname theme_gamlss
#' @export
theme_gamlss_journal <- function(base_size=9, base_family="", grid=FALSE) theme_gamlss(base_size,base_family,grid)+ggplot2::theme(legend.position="top")
#' @rdname theme_gamlss
#' @export
theme_gamlss_presentation <- function(base_size=16, base_family="", grid=TRUE) theme_gamlss(base_size,base_family,grid)+ggplot2::theme(legend.position="bottom")

#' Plot one or more GAMLSS distributional parameters
#' @param object Fitted GAMLSS model.
#' @param x Predictor shown on the horizontal axis.
#' @param parameters Parameters to draw; `"all"` uses all detected parameters.
#' @param by Optional grouping variable(s).
#' @param n Grid size for numeric predictors.
#' @param scale `response` or `link`. Link scale is obtained directly from the model's parameter predictor.
#' @param uncertainty Inference layer passed to the data engine.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return A ggplot object.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_parameters(fit, "dose", parameters = c("mu", "sigma"), data = d, n = 12)
#'   p2 <- plot_gamlss_parameters(fit, "dose", parameters = "mu", scale = "link", data = d, n = 12)
#'   p3 <- plot_gamlss_parameters(fit, "trt", parameters = "all", data = d)
#' }
plot_gamlss_parameters <- function(object,x,parameters="all",by=NULL,n=100L,scale=c("response","link"),uncertainty="none",data=NULL,...) {
  scale <- match.arg(scale)
  pars <- if (identical(parameters,"all")) .gph_model_parameters(object) else parameters
  d <- gamlss_plot_data(object,"parameters",x=x,by=by,parameters=pars,parameter_scale=scale,
                        n=n,uncertainty=uncertainty,data=data,...)
  num <- is.numeric(.gph_get_data(object,data)[[x]]); grp <- if(length(by)) by[1] else NULL
  p <- ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  if (num) p <- p + ggplot2::geom_line(linewidth=.8) else p <- p + ggplot2::geom_point(size=2.3)
  if (all(c("lower.CL","upper.CL")%in%names(d))) {
    if (num) p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$lower.CL,ymax=.data$upper.CL,fill=if(!is.null(grp)) .data[[grp]] else NULL),alpha=.15,colour=NA)
    else p <- p + ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$lower.CL,ymax=.data$upper.CL),width=.08)
  }
  p + ggplot2::facet_wrap(~parameter,scales="free_y") + ggplot2::labs(x=x,y=NULL,title="Distributional parameters") + theme_gamlss()
}

#' Plot a declared GAMLSS estimand
#' @param object Fitted model.
#' @param x Predictor.
#' @param estimand Target estimand.
#' @param what Parameter when `estimand="parameter"`.
#' @param by Optional grouping variable(s).
#' @param n Numeric grid size.
#' @param uncertainty Inference layer.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_estimand(fit, "trt", estimand = "mean", data = d)
#'   p2 <- plot_gamlss_estimand(fit, "dose", estimand = "variance", data = d, n = 12)
#'   p3 <- plot_gamlss_estimand(fit, "dose", estimand = "quantile", prob = .9, data = d, n = 12)
#' }
plot_gamlss_estimand <- function(object,x,estimand="mean",what="mu",by=NULL,n=100L,uncertainty="none",data=NULL,...) {
  d <- gamlss_plot_data(object,"estimand",x=x,by=by,estimand=estimand,what=what,n=n,uncertainty=uncertainty,data=data,...)
  num <- is.numeric(.gph_get_data(object,data)[[x]]); grp <- if(length(by)) by[1] else NULL
  p <- ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  if (num) p <- p+ggplot2::geom_line(linewidth=.85) else p <- p+ggplot2::geom_point(size=2.4)
  if (all(c("lower.CL","upper.CL")%in%names(d))) {
    if(num) p <- p+ggplot2::geom_ribbon(ggplot2::aes(ymin=.data$lower.CL,ymax=.data$upper.CL,fill=if(!is.null(grp)) .data[[grp]] else NULL),alpha=.15,colour=NA)
    else p <- p+ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$lower.CL,ymax=.data$upper.CL),width=.08)
  }
  p+ggplot2::labs(x=x,y=estimand,title=paste("GAMLSS",estimand))+theme_gamlss()
}

#' Forest/estimation plot for post-hoc contrasts
#' @param object `gamlss_posthoc` object or fitted model.
#' @param x Focal factor when `object` is a model.
#' @param null Optional null value; inferred from comparison when omitted.
#' @param show_p Show adjusted p-values as labels when available.
#' @param style `interval` for estimates and confidence intervals or `distribution` for retained bootstrap draws with optional `ggdist`.
#' @param ... Arguments forwarded when a model is supplied.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   r1 <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   p1 <- plot_gamlss_contrasts(r1)
#'   r2 <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "reference", comparison = "percent_change", uncertainty = "none", data = d)
#'   p2 <- plot_gamlss_contrasts(r2)
#'   p3 <- plot_gamlss_contrasts(r1, show_p = FALSE, style = "interval")
#' }
plot_gamlss_contrasts <- function(object,x=NULL,null=NULL,show_p=FALSE,style=c("interval","distribution"),...) {
  style <- match.arg(style)
  z <- if(inherits(object,"gamlss_posthoc")) object else gamlss_posthoc(object,specs=x,contrast="pairwise",...)
  d <- z$contrasts; if(is.null(d)||!nrow(d)) stop("No contrasts available.",call.=FALSE)
  comp <- z$comparison %||% (d$comparison[1] %||% "difference"); if(is.null(null)) null <- .gph_comparison_null(comp)
  d$.label <- d$contrast %||% seq_len(nrow(d))
  draws <- attr(d, "draws")
  if (style == "distribution" && !is.null(draws) && requireNamespace("ggdist", quietly = TRUE)) {
    dl <- do.call(rbind, lapply(seq_len(ncol(draws)), function(j) data.frame(.label=d$.label[j], draw=draws[,j])))
    p <- ggplot2::ggplot(dl, ggplot2::aes(x=.data$draw, y=.data$.label)) +
      ggdist::stat_halfeye(.width=c(.5,.8,.95)) + ggplot2::geom_vline(xintercept=null,linetype=2)
  } else {
    if (style == "distribution" && (is.null(draws) || !requireNamespace("ggdist", quietly = TRUE))) warning("Distribution style requires retained draws and package 'ggdist'; using interval style.", call.=FALSE)
    p <- ggplot2::ggplot(d,ggplot2::aes(x=.data$estimate,y=stats::reorder(.data$.label,.data$estimate)))+
      ggplot2::geom_vline(xintercept=null,linetype=2)+ggplot2::geom_point(size=2.4)
    if(all(c("lower.CL","upper.CL")%in%names(d))) p <- p+ggplot2::geom_errorbar(ggplot2::aes(xmin=.data$lower.CL,xmax=.data$upper.CL),orientation="y",width=.15)
  }
  if(show_p) { pc <- if("p.value.adjusted"%in%names(d)) "p.value.adjusted" else if("p.value"%in%names(d)) "p.value" else NULL; if(!is.null(pc)) { d$.ptxt <- paste0("p=",formatC(d[[pc]],digits=3,format="fg")); p <- p+ggplot2::geom_text(data=d,ggplot2::aes(label=.data$.ptxt),hjust=-.1,size=3) } }
  p+ggplot2::labs(x=.gph_comparison_label(comp),y=NULL,title="Post-hoc contrasts")+theme_gamlss()
}

#' Plot standardized predictive distributions
#' @param object Fitted model.
#' @param x Grouping/predictor variable.
#' @param by Optional conditioning variables.
#' @param type Density, CDF, or survival curve.
#' @param at,newdata,population,weights Prediction standardization controls.
#' @param n,n_response Grid sizes.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_distribution(fit, "trt", type = "density", data = d, n_response = 40)
#'   p2 <- plot_gamlss_distribution(fit, "trt", type = "cdf", data = d, n_response = 40)
#'   p3 <- plot_gamlss_distribution(fit, "dose", at = list(dose = c(0, 60, 120)), type = "survival", data = d, n_response = 40)
#' }
plot_gamlss_distribution <- function(object,x,by=NULL,type=c("density","cdf","survival"),at=list(),newdata=NULL,population="observed",weights=NULL,n=100L,n_response=200L,data=NULL,...) {
  type<-match.arg(type); d<-gamlss_plot_data(object,"distribution",x=x,by=by,curve=type,at=at,newdata=newdata,population=population,weights=weights,n=n,n_response=n_response,data=data,...)
  gc <- if (".gph_label" %in% names(d)) ".gph_label" else .gph_first_group(d,exclude=c("response","value")); if(is.null(gc)&&x%in%names(d)) gc<-x
  p<-ggplot2::ggplot(d,.gph_aes("response","value",colour=gc,group=gc))+ggplot2::geom_line(linewidth=.85)
  if(.gph_is_zadj(object)&&"prob_zero"%in%names(d)&&type=="density") { spike<-unique(d[c(intersect(gc,names(d)),"prob_zero")]); if(nrow(spike)) p<-p+ggplot2::geom_segment(data=spike,ggplot2::aes(x=0,xend=0,y=0,yend=.data$prob_zero),inherit.aes=FALSE,linewidth=.8) }
  p+ggplot2::labs(x="Response",y=type,title=paste("Predictive",type))+theme_gamlss()
}

#' Plot marginal predictive quantiles as lines or fan bands
#' @param object Fitted model.
#' @param x Predictor.
#' @param probs Quantile probabilities.
#' @param by Optional group.
#' @param style `lines` or `fan`.
#' @param n Grid size.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_quantiles(fit, "dose", style = "fan", data = d, n = 12)
#'   p2 <- plot_gamlss_quantiles(fit, "dose", probs = c(.1, .5, .9), style = "lines", data = d, n = 12)
#'   p3 <- plot_gamlss_quantiles(fit, "dose", by = "trt", style = "fan", data = d, n = 12)
#' }
plot_gamlss_quantiles <- function(object,x,probs=c(.05,.1,.25,.5,.75,.9,.95),by=NULL,style=c("fan","lines"),n=100L,data=NULL,...) {
  style<-match.arg(style); d<-gamlss_plot_data(object,"quantiles",x=x,by=by,probs=sort(unique(probs)),n=n,data=data,...)
  grp<-if(length(by)) by[1] else NULL
  # Each probability is a separate line; grouping must therefore include `prob`,
  # otherwise a single path would carry varying linetype and colour.
  if(style=="lines") return(ggplot2::ggplot(d,.gph_aes(x,"quantile",colour=grp))+ggplot2::geom_line(ggplot2::aes(linetype=factor(.data$prob),group=if(!is.null(grp)) interaction(.data[[grp]],.data$prob) else factor(.data$prob)),linewidth=.75)+ggplot2::labs(x=x,y="Quantile",linetype="p",title="Predictive quantiles")+theme_gamlss())
  # Build symmetric intervals around median when matching probabilities exist.
  lower<-sort(probs[probs<.5]); upper<-sort(probs[probs>.5],decreasing=TRUE); k<-min(length(lower),length(upper)); if(k<1L) stop("Fan style requires probabilities below and above 0.5.",call.=FALSE)
  med<-d[which.min(abs(d$prob-.5)),,drop=FALSE]; bands<-list()
  keys<-setdiff(names(d),c("prob","quantile")); basekeys<-keys
  for(i in seq_len(k)) { lo<-d[abs(d$prob-lower[i])<1e-12,,drop=FALSE]; hi<-d[abs(d$prob-upper[i])<1e-12,,drop=FALSE]; m<-merge(lo,hi,by=basekeys,suffixes=c(".lo",".hi")); m$.width<-upper[i]-lower[i]; bands[[i]]<-m }
  b<-do.call(rbind,bands); p<-ggplot2::ggplot()
  if(nrow(b)) p<-p+ggplot2::geom_ribbon(data=b,ggplot2::aes(x=.data[[x]],ymin=.data$quantile.lo,ymax=.data$quantile.hi,group=if(!is.null(grp)) interaction(.data[[grp]],.data$.width) else .data$.width,fill=factor(.data$.width)),alpha=.18,colour=NA)
  medall<-d[abs(d$prob-.5)==min(abs(d$prob-.5)),,drop=FALSE]; p+ggplot2::geom_line(data=medall,.gph_aes(x,"quantile",colour=grp,group=grp),linewidth=.9)+ggplot2::labs(x=x,y="Response quantiles",fill="Central width",title="Predictive quantile fan")+theme_gamlss()
}

#' Decompose zero-adjusted predictions
#' @param object Fitted `gamlssZadj` model.
#' @param x Predictor/group.
#' @param by Optional conditioning variables.
#' @param style `facets` or `normalized`.
#' @param n Grid size.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("gamlss.inf", quietly = TRUE) &&
#'     requireNamespace("distributions3", quietly = TRUE)) {
#'   set.seed(312)
#'   d <- data.frame(trt = factor(rep(c("A", "B"), each = 45)), dose = rep(seq(0, 80, length.out = 9), 10))
#'   mu <- exp(0.2 + 0.012*d$dose + 0.25*(d$trt == "B"))
#'   h <- plogis(0.2 - 0.7*(d$trt == "B") - 0.01*d$dose)
#'   yp <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = 0.35)
#'   d$y <- ifelse(stats::rbinom(nrow(d), 1, h) == 1, 0, yp)
#'   fit <- gamlss.inf::gamlssZadj(y = y, mu.formula = ~ trt + dose,
#'                                 sigma.formula = ~ 1,
#'                                 xi0.formula = ~ trt + dose,
#'                                 family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_zero_adjusted(fit, "trt", data = d)
#'   p2 <- plot_gamlss_zero_adjusted(fit, "dose", style = "facets", data = d, n = 12)
#'   p3 <- plot_gamlss_zero_adjusted(fit, "dose", by = "trt", style = "normalized", data = d, n = 12)
#' }
plot_gamlss_zero_adjusted <- function(object,x,by=NULL,style=c("facets","normalized"),n=100L,data=NULL,...) {
  style<-match.arg(style); d<-gamlss_plot_data(object,"zero_adjusted",x=x,by=by,n=n,data=data,...); grp<-if(length(by)) by[1] else NULL
  if(style=="normalized") d$plot_value<-ave(d$estimate,d$component,FUN=function(z) z/max(abs(z),na.rm=TRUE)) else d$plot_value<-d$estimate
  num<-is.numeric(.gph_get_data(object,data)[[x]]); p<-ggplot2::ggplot(d,.gph_aes(x,"plot_value",colour=grp,group=grp))
  if(num) p<-p+ggplot2::geom_line(linewidth=.85) else p<-p+ggplot2::geom_point(size=2.3)
  p+ggplot2::facet_wrap(~component,scales=if(style=="facets")"free_y" else "fixed")+ggplot2::labs(x=x,y=if(style=="normalized")"Normalized value" else "Estimate",title="Zero-adjusted decomposition")+theme_gamlss()
}

#' Plot a quantitative GAMLSS trend with confidence bands
#' @param object Fitted model or `gamlss_trend` object.
#' @param x Predictor when a model is supplied.
#' @param by Optional group.
#' @param band Pointwise or simultaneous band when available.
#' @param intervals Optional interval widths for ggdist draws when retained.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_trend(fit, "dose", data = d, n = 15)
#'   p2 <- plot_gamlss_trend(fit, "dose", by = "trt", data = d, n = 15)
#'   tr <- gamlss_trend(fit, "dose", n = 15, uncertainty = "none", data = d)
#'   p3 <- plot_gamlss_trend(tr)
#' }
plot_gamlss_trend <- function(object,x=NULL,by=NULL,band=c("pointwise","simultaneous"),intervals=c(.5,.8,.95),data=NULL,...) {
  band<-match.arg(band); tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="curve",band=band,data=data,...); d<-tr$values; x<-tr$x; grp<-if(length(tr$by)) tr$by[1] else NULL
  draws <- tr$source$draws %||% NULL
  if (!is.null(draws) && requireNamespace("ggdist", quietly=TRUE) && nrow(draws) > 1L) {
    dl <- do.call(rbind, lapply(seq_len(nrow(draws)), function(i) { z <- d; z$.draw <- i; z$.value <- as.numeric(draws[i,]); z }))
    p <- ggplot2::ggplot(dl, .gph_aes(x, ".value", colour=grp, group=grp)) +
      ggdist::stat_lineribbon(ggplot2::aes(group=if(!is.null(grp)) .data[[grp]] else 1), .width=intervals, alpha=.15)
  } else p<-ggplot2::ggplot(d,.gph_aes(x,"estimate",colour=grp,group=grp))
  lo<-if(band=="simultaneous"&&"simultaneous_lower"%in%names(d))"simultaneous_lower" else if("lower.CL"%in%names(d))"lower.CL" else NULL
  hi<-if(band=="simultaneous"&&"simultaneous_upper"%in%names(d))"simultaneous_upper" else if("upper.CL"%in%names(d))"upper.CL" else NULL
  if (is.null(draws) || !requireNamespace("ggdist", quietly=TRUE)) {
    if(!is.null(lo)&&!is.null(hi)) p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[lo]],ymax=.data[[hi]],fill=if(!is.null(grp)) .data[[grp]] else NULL),alpha=.16,colour=NA)
    p<-p+ggplot2::geom_line(linewidth=.9)
  }
  p+ggplot2::labs(x=x,y=tr$estimand,title="GAMLSS trend")+theme_gamlss()
}

#' Plot first or second derivative of a GAMLSS trend
#' @param object Fitted model or derivative `gamlss_trend` object.
#' @param x Predictor when model supplied.
#' @param order Derivative order, 1 or 2.
#' @param by Optional group.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_derivative(fit, "dose", order = 1, data = d, n = 15)
#'   p2 <- plot_gamlss_derivative(fit, "dose", order = 2, data = d, n = 15)
#'   p3 <- plot_gamlss_derivative(fit, "dose", order = 1, by = "trt", data = d, n = 15)
#' }
plot_gamlss_derivative <- function(object,x=NULL,order=1L,by=NULL,data=NULL,...) {
  tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="derivative",derivative_order=order,data=data,...); d<-tr$values; x<-tr$x; ycol<-paste0("derivative",order); grp<-if(length(tr$by)) tr$by[1] else NULL
  p<-ggplot2::ggplot(d,.gph_aes(x,ycol,colour=grp,group=grp))+ggplot2::geom_hline(yintercept=0,linetype=2)
  lo<-paste0(ycol,"_lower"); hi<-paste0(ycol,"_upper"); if(all(c(lo,hi)%in%names(d))) p<-p+ggplot2::geom_ribbon(ggplot2::aes(ymin=.data[[lo]],ymax=.data[[hi]],fill=if(!is.null(grp)) .data[[grp]] else NULL),alpha=.16,colour=NA)
  p+ggplot2::geom_line(linewidth=.85)+ggplot2::labs(x=x,y=paste0("d",order,"/d",x,order),title=paste("Derivative",order))+theme_gamlss()
}

#' Plot a maximum or minimum and its uncertainty
#' @param object Fitted model or optimum `gamlss_trend` object.
#' @param x Predictor when model supplied.
#' @param optimum Maximum or minimum.
#' @param by Optional group.
#' @param show_distribution If bootstrap optimum draws are available, use them in a distributional display.
#' @param data Original data.
#' @param ... Arguments to `gamlss_trend()`.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_optimum(fit, "dose", optimum = "maximum", show_distribution = FALSE, data = d, n = 21)
#'   p2 <- plot_gamlss_optimum(fit, "dose", optimum = "minimum", show_distribution = FALSE, data = d, n = 21)
#'   p3 <- plot_gamlss_optimum(fit, "dose", by = "trt", optimum = "maximum", show_distribution = FALSE, data = d, n = 21)
#' }
plot_gamlss_optimum <- function(object,x=NULL,optimum=c("maximum","minimum"),by=NULL,show_distribution=TRUE,data=NULL,...) {
  optimum<-match.arg(optimum); tr<-if(inherits(object,"gamlss_trend")) object else gamlss_trend(object,x=x,by=by,method="optimum",optimum=optimum,data=data,...); d<-tr$special_points; if(is.null(d)||!nrow(d)) stop("No optimum detected.",call.=FALSE); x<-tr$x; grp<-if(length(tr$by)) tr$by[1] else NULL
  d$.grp<-if(!is.null(grp)) as.character(d[[grp]]) else "All"
  odraws <- attr(d, "optimum_draws")
  if (isTRUE(show_distribution) && !is.null(odraws) && requireNamespace("ggdist", quietly=TRUE)) {
    dl <- do.call(rbind, lapply(seq_along(odraws), function(j) data.frame(.grp=d$.grp[j], location=as.numeric(odraws[[j]]))))
    p <- ggplot2::ggplot(dl, ggplot2::aes(x=.data$location,y=.data$.grp)) +
      ggdist::stat_halfeye(.width=c(.5,.8,.95))
  } else {
    if (isTRUE(show_distribution) && !is.null(odraws) && !requireNamespace("ggdist", quietly=TRUE))
      warning("Package 'ggdist' is not installed; showing interval representation.", call.=FALSE)
    p<-ggplot2::ggplot(d,ggplot2::aes(x=.data[[x]],y=.data$.grp))+ggplot2::geom_point(size=2.5)
    if(all(c("x_lower","x_upper")%in%names(d))) p<-p+ggplot2::geom_errorbar(ggplot2::aes(xmin=.data$x_lower,xmax=.data$x_upper),orientation="y",width=.15)
  }
  p+ggplot2::labs(x=paste("Location of",optimum),y=NULL,title=paste("Estimated",optimum))+theme_gamlss()
}

#' Plot a two-predictor estimand surface
#' @param object Fitted model.
#' @param x,y Numeric predictors.
#' @param estimand,what Target estimand.
#' @param type Heatmap, contour, or both.
#' @param n Approximate grid density.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(311)
#'   d <- expand.grid(dose = seq(0, 100, length.out = 8), salinity = seq(0, 3, length.out = 6))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = exp(0.5 + 0.01*d$dose - 0.18*d$salinity), sigma = 0.25)
#'   fit <- gamlss::gamlss(y ~ dose * salinity, family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_surface(fit, "dose", "salinity", type = "heatmap", data = d, n = 16)
#'   p2 <- plot_gamlss_surface(fit, "dose", "salinity", type = "contour", data = d, n = 16)
#'   p3 <- plot_gamlss_surface(fit, "dose", "salinity", estimand = "variance", type = "both", data = d, n = 16)
#' }
plot_gamlss_surface <- function(object,x,y,estimand="mean",what="mu",type=c("heatmap","contour","both"),n=45L,data=NULL,...) {
  type<-match.arg(type); d<-gamlss_plot_data(object,"surface",x=x,y=y,estimand=estimand,what=what,n=n,data=data,...); p<-ggplot2::ggplot(d,ggplot2::aes(x=.data[[x]],y=.data[[y]]))
  if(type%in%c("heatmap","both")) p<-p+ggplot2::geom_raster(ggplot2::aes(fill=.data$estimate),interpolate=TRUE)
  if(type%in%c("contour","both")) p<-p+ggplot2::geom_contour(ggplot2::aes(z=.data$estimate),colour="black",linewidth=.35)
  p+ggplot2::labs(x=x,y=y,fill=estimand,title="GAMLSS estimand surface")+theme_gamlss()
}

#' Plot observations with a GAMLSS central curve and predictive quantiles
#' @param object Fitted model.
#' @param x Predictor.
#' @param response Optional response name; inferred from model when omitted.
#' @param by Optional group.
#' @param probs Lower, median, upper predictive probabilities.
#' @param n Grid size.
#' @param jitter Jitter observations when x is discrete.
#' @param data Original data.
#' @param ... Additional arguments.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_fit(fit, "dose", response = "y", data = d, n = 15)
#'   p2 <- plot_gamlss_fit(fit, "dose", response = "y", by = "trt", data = d, n = 15)
#'   p3 <- plot_gamlss_fit(fit, "trt", response = "y", jitter = TRUE, data = d)
#' }
plot_gamlss_fit <- function(object,x,response=NULL,by=NULL,probs=c(.05,.5,.95),n=100L,jitter=FALSE,data=NULL,...) {
  data<-.gph_get_data(object,data); if(is.null(response)) { f<-.gph_get_formula(object,.gph_model_parameters(object)[1]); response<-all.vars(f)[1] }
  q<-gamlss_plot_data(object,"quantiles",x=x,by=by,probs=probs,n=n,data=data,...); medp<-probs[which.min(abs(probs-.5))]; med<-q[abs(q$prob-medp)<1e-10,,drop=FALSE]; lo<-q[abs(q$prob-min(probs))<1e-10,,drop=FALSE]; hi<-q[abs(q$prob-max(probs))<1e-10,,drop=FALSE]; keys<-setdiff(names(q),c("prob","quantile")); band<-merge(lo,hi,by=keys,suffixes=c(".lo",".hi")); grp<-if(length(by)) by[1] else NULL
  p<-ggplot2::ggplot(data,.gph_aes(x,response,colour=grp)); p<-if(jitter) p+ggplot2::geom_jitter(alpha=.35,width=.08,height=0) else p+ggplot2::geom_point(alpha=.35)
  if(is.numeric(data[[x]])) {
    p<-p+ggplot2::geom_ribbon(data=band,ggplot2::aes(x=.data[[x]],ymin=.data$quantile.lo,ymax=.data$quantile.hi,fill=if(!is.null(grp)) .data[[grp]] else NULL,group=if(!is.null(grp)) .data[[grp]] else 1),inherit.aes=FALSE,alpha=.12)+ggplot2::geom_line(data=med,.gph_aes(x,"quantile",colour=grp,group=grp),linewidth=.9)
  } else {
    # For discrete predictors, show predictive median and interval at each level.
    p<-p+ggplot2::geom_errorbar(data=band,ggplot2::aes(x=.data[[x]],ymin=.data$quantile.lo,ymax=.data$quantile.hi,colour=if(!is.null(grp)) .data[[grp]] else NULL),inherit.aes=FALSE,width=.12)+
      ggplot2::geom_point(data=med,ggplot2::aes(x=.data[[x]],y=.data$quantile,colour=if(!is.null(grp)) .data[[grp]] else NULL),inherit.aes=FALSE,size=2.5)
  }
  p+ggplot2::labs(x=x,y=response,title="Observed data and predictive distribution")+theme_gamlss()
}

#' GAMLSS diagnostics in ggplot2
#' @param object Fitted model.
#' @param type One of residual_fitted, qq, worm, density, pit, rootogram, or all.
#' @param bins Number of bins.
#' @param nsim Simulations for rootogram.
#' @param engine Diagnostic engine; currently `native`, implemented with public `ggplot2` APIs.
#' @param data Original data.
#' @return A ggplot, a patchwork object when available, or a named list of plots for `type="all"`.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_diagnostics(fit, "qq", engine = "native", data = d)
#'   p2 <- plot_gamlss_diagnostics(fit, "worm", engine = "native", data = d)
#'   p3 <- plot_gamlss_diagnostics(fit, "density", engine = "native", data = d)
#' }
plot_gamlss_diagnostics <- function(object,type=c("residual_fitted","qq","worm","density","pit","rootogram","all"),bins=20L,nsim=100L,engine=c("native"),data=NULL) {
  type<-match.arg(type); engine<-match.arg(engine); .gph_ggrequire(); data<-.gph_get_data(object,data)
  r<-try(stats::residuals(object,type="z-scores"),silent=TRUE); if(inherits(r,"try-error")) r<-stats::residuals(object); r<-as.numeric(r); fitted<-try(stats::fitted(object),silent=TRUE); if(inherits(fitted,"try-error")) fitted<-seq_along(r); fitted<-as.numeric(fitted)
  one<-function(tp){
    if(tp=="residual_fitted") return(ggplot2::ggplot(data.frame(fitted=fitted,resid=r),ggplot2::aes(.data$fitted,.data$resid))+ggplot2::geom_hline(yintercept=0,linetype=2)+ggplot2::geom_point(alpha=.5)+ggplot2::geom_smooth(se=FALSE,method="loess",formula=y~x)+ggplot2::labs(x="Fitted",y="Quantile residual",title="Residuals vs fitted")+theme_gamlss())
    if(tp=="qq") { n<-sum(is.finite(r)); d<-data.frame(theoretical=stats::qnorm(stats::ppoints(n)),sample=sort(r[is.finite(r)])); return(ggplot2::ggplot(d,ggplot2::aes(.data$theoretical,.data$sample))+ggplot2::geom_abline(slope=1,intercept=0,linetype=2)+ggplot2::geom_point()+ggplot2::labs(x="Normal quantile",y="Residual quantile",title="QQ plot")+theme_gamlss()) }
    if(tp=="worm") { n<-sum(is.finite(r)); theo<-stats::qnorm(stats::ppoints(n)); samp<-sort(r[is.finite(r)]); d<-data.frame(theoretical=theo,deviation=samp-theo); return(ggplot2::ggplot(d,ggplot2::aes(.data$theoretical,.data$deviation))+ggplot2::geom_hline(yintercept=0,linetype=2)+ggplot2::geom_point(alpha=.5)+ggplot2::geom_smooth(se=FALSE,formula=y~stats::poly(x,2),method="lm")+ggplot2::labs(x="Normal quantile",y="Detrended residual",title="Worm-style detrended QQ")+theme_gamlss()) }
    if(tp=="density") return(ggplot2::ggplot(data.frame(resid=r),ggplot2::aes(.data$resid))+ggplot2::geom_density()+ggplot2::stat_function(fun=stats::dnorm,linetype=2)+ggplot2::labs(x="Quantile residual",y="Density",title="Residual density")+theme_gamlss())
    if(tp=="pit") { u<-stats::pnorm(r); return(ggplot2::ggplot(data.frame(pit=u),ggplot2::aes(.data$pit))+ggplot2::geom_histogram(bins=bins,boundary=0)+ggplot2::geom_hline(yintercept=length(u)/bins,linetype=2)+ggplot2::labs(x="PIT from quantile residual",y="Count",title="PIT histogram")+theme_gamlss()) }
    # rootogram for integer responses via simulation
    resp<-all.vars(.gph_get_formula(object,.gph_model_parameters(object)[1]))[1]; yobs<-data[[resp]]; if(!is.numeric(yobs)||any(abs(yobs-round(yobs))>1e-8,na.rm=TRUE)) stop("Rootogram is intended for discrete/integer responses.",call.=FALSE)
    sims<-replicate(as.integer(nsim),.gph_simulate_response(object,data)); br<-seq(min(yobs,na.rm=TRUE),max(yobs,na.rm=TRUE)); obs<-tabulate(match(yobs,br),nbins=length(br)); expc<-rowMeans(vapply(seq_len(ncol(sims)),function(j)tabulate(match(round(sims[,j]),br),nbins=length(br)),numeric(length(br)))); dd<-data.frame(value=br,observed=sqrt(obs),expected=sqrt(expc),diff=sqrt(obs)-sqrt(expc)); ggplot2::ggplot(dd,ggplot2::aes(.data$value,.data$diff))+ggplot2::geom_hline(yintercept=0)+ggplot2::geom_col()+ggplot2::labs(x=resp,y="sqrt(observed)-sqrt(expected)",title="Hanging rootogram")+theme_gamlss()
  }
  if(type!="all") return(one(type)); types<-c("residual_fitted","qq","worm","density","pit"); ps<-stats::setNames(lapply(types,one),types); if(requireNamespace("patchwork",quietly=TRUE)) return(Reduce(`+`,ps)+patchwork::plot_layout(ncol=2)); ps
}

#' Compare fitted GAMLSS models graphically
#' @param ... Named fitted GAMLSS models.
#' @param type `criteria` or `estimand`.
#' @param criterion Criterion drawn when `type='criteria'`: `AIC`, `global_deviance`, `df`, or `all`.
#' @param x Predictor for estimand comparison.
#' @param estimand,what Target estimand.
#' @param data Optional original data.
#' @return ggplot.
#' @export
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(310)
#'   d <- data.frame(
#'     dose = rep(seq(0, 120, length.out = 8), each = 8),
#'     trt = factor(rep(c("A", "B"), length.out = 64))
#'   )
#'   mu <- exp(0.5 + 0.012 * d$dose - 0.00005 * d$dose^2 + 0.15 * (d$trt == "B"))
#'   sig <- exp(log(0.22) + 0.001 * d$dose)
#'   d$y <- gamlss.dist::rGA(nrow(d), mu = mu, sigma = sig)
#'   fit <- gamlss::gamlss(y ~ trt + dose + I(dose^2),
#'                         sigma.formula = ~ dose,
#'                         family = gamlss.dist::GA, data = d, trace = FALSE)
#'   fit2 <- gamlss::gamlss(y ~ trt + dose, family = gamlss.dist::GA, data = d, trace = FALSE)
#'   p1 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "criteria")
#'   p2 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose", data = d)
#'   p3 <- plot_gamlss_compare(quadratic = fit, linear = fit2, type = "estimand", x = "dose", estimand = "variance", data = d)
#' }
plot_gamlss_compare <- function(...,type=c("criteria","estimand"),criterion=c("AIC","global_deviance","df","all"),x=NULL,estimand="mean",what="mu",data=NULL) {
  type<-match.arg(type); criterion<-match.arg(criterion); mods<-list(...); if(!length(mods)) stop("Supply fitted models.",call.=FALSE); nm<-names(mods); if(is.null(nm)||any(!nzchar(nm))) nm<-paste0("Model",seq_along(mods))
  if(type=="criteria") {
    d<-do.call(rbind,lapply(seq_along(mods),function(i){m<-mods[[i]]; data.frame(model=nm[i],AIC=tryCatch(stats::AIC(m),error=function(e)NA_real_),global_deviance=m$G.deviance%||%NA_real_,df=m$df.fit%||%NA_real_)}))
    if (criterion == "all") {
      long<-do.call(rbind,lapply(c("AIC","global_deviance","df"),function(z)data.frame(model=d$model,criterion=z,value=d[[z]])))
      return(ggplot2::ggplot(long,ggplot2::aes(.data$model,.data$value))+ggplot2::geom_col()+ggplot2::facet_wrap(~criterion,scales="free_y")+ggplot2::labs(x=NULL,y=NULL,title="Model comparison")+theme_gamlss())
    }
    return(ggplot2::ggplot(d,ggplot2::aes(x=.data$model,y=.data[[criterion]]))+ggplot2::geom_col()+ggplot2::labs(x=NULL,y=criterion,title="Model comparison")+theme_gamlss())
  }
  if(is.null(x)) stop("`x` is required for estimand comparison.",call.=FALSE); rows<-lapply(seq_along(mods),function(i){z<-gamlss_plot_data(mods[[i]],"estimand",x=x,estimand=estimand,what=what,data=data);z$model<-nm[i];z}); d<-do.call(rbind,rows); ggplot2::ggplot(d,ggplot2::aes(x=.data[[x]],y=.data$estimate,colour=.data$model,group=.data$model))+ggplot2::geom_line(linewidth=.85)+ggplot2::labs(x=x,y=estimand,title="Model estimand comparison")+theme_gamlss()
}

# S3 plot/autoplot ------------------------------------------------------------
#' Plot and autoplot methods for gamlssPosthoc result classes
#'
#' @description `plot()` prints and invisibly returns the corresponding
#' `ggplot2::autoplot()` result. `autoplot()` returns a complete, modifiable
#' `ggplot` object for post-hoc, trend, distribution-summary, and diagnostic-plan
#' objects owned by this package.
#' @name autoplot.gamlssPosthoc
#' @aliases plot.gamlss_posthoc plot.gamlss_trend
#'   plot.gamlss_distribution_summary plot.gamlss_posthoc_plan
#'   autoplot.gamlss_posthoc autoplot.gamlss_trend
#'   autoplot.gamlss_distribution_summary autoplot.gamlss_posthoc_plan
#' @param x Result object for `plot()` methods.
#' @param object Result object for `autoplot()` methods.
#' @param ... Additional graphical arguments forwarded to the specialized
#'   constructor when supported.
#' @param x_axis Optional predictor used as horizontal axis for a distribution
#'   summary; use the method argument `x` in direct calls.
#' @return `autoplot()` returns a `ggplot`; `plot()` prints it and returns it invisibly.
#' @examples
#' if (requireNamespace("gamlss", quietly=TRUE) &&
#'     requireNamespace("gamlss.dist", quietly=TRUE) &&
#'     requireNamespace("distributions3", quietly=TRUE)) {
#'   set.seed(331)
#'   d <- data.frame(dose=rep(seq(0,100,length.out=8),each=6),
#'                   trt=factor(rep(c("A","B"),length.out=48)))
#'   d$y <- gamlss.dist::rGA(nrow(d), mu=exp(.4+.01*d$dose+.15*(d$trt=="B")), sigma=.22)
#'   fit <- gamlss::gamlss(y~trt+dose, family=gamlss.dist::GA, data=d, trace=FALSE)
#'   ph <- gamlss_posthoc(fit,"trt",estimand="mean",contrast="pairwise",
#'                        uncertainty="none",data=d)
#'   tr <- gamlss_trend(fit,"dose",n=15,uncertainty="none",data=d)
#'   ds <- gamlss_distribution_summary(fit,
#'     data.frame(dose=c(25,75),trt=factor(c("A","B"),levels=levels(d$trt))),data=d)
#'   ggplot2::autoplot(ph)
#'   ggplot2::autoplot(tr)
#'   ggplot2::autoplot(ds, x="dose")
#' }
NULL

#' @export
plot.gamlss_posthoc <- function(x,...) { p<-ggplot2::autoplot(x,...); print(p); invisible(p) }
#' @export
plot.gamlss_trend <- function(x,...) { p<-ggplot2::autoplot(x,...); print(p); invisible(p) }

#' @export
plot.gamlss_distribution_summary <- function(x, ...) { p <- ggplot2::autoplot(x, ...); print(p); invisible(p) }
#' @export
plot.gamlss_posthoc_plan <- function(x, ...) { p <- ggplot2::autoplot(x, ...); print(p); invisible(p) }
#' @export
autoplot.gamlss_posthoc <- function(object,...) { if(!is.null(object$contrasts)) plot_gamlss_contrasts(object,...) else { d<-object$estimates; x<-object$specs[1]; p<-ggplot2::ggplot(d,.gph_aes(x,"estimate"))+ggplot2::geom_point(size=2.3); if(all(c("lower.CL","upper.CL")%in%names(d)))p<-p+ggplot2::geom_errorbar(ggplot2::aes(ymin=.data$lower.CL,ymax=.data$upper.CL),width=.08); p+ggplot2::labs(x=x,y=object$estimand_info$target[1],title="GAMLSS post-hoc estimates")+theme_gamlss() } }
#' @export
autoplot.gamlss_trend <- function(object,...) { if(grepl("derivative",object$method)) plot_gamlss_derivative(object,...) else if(object$method=="optimum") plot_gamlss_optimum(object,...) else plot_gamlss_trend(object,...) }
#' @export
autoplot.gamlss_distribution_summary <- function(object,x=NULL,...) { d<-as.data.frame(object); if(is.null(x)) x<-names(d)[1]; qcols<-grep("^q",names(d),value=TRUE); long<-do.call(rbind,lapply(qcols,function(q)data.frame(d[x],stat=q,value=d[[q]]))); ggplot2::ggplot(long,ggplot2::aes(x=.data[[x]],y=.data$value,colour=.data$stat,group=.data$stat))+ggplot2::geom_line()+ggplot2::geom_point()+ggplot2::labs(x=x,y="Value",title="Distribution summary")+theme_gamlss() }
#' @export
autoplot.gamlss_posthoc_plan <- function(object,...) { d<-object$parameter_capabilities; if(is.null(d)||!nrow(d))stop("No parameter capability table.",call.=FALSE); labels<-c(emmeans_eligible="emmeans",marginaleffects_candidate="marginaleffects",distribution_prediction="distribution",emmeans="emmeans",marginaleffects="marginaleffects",distribution="distribution"); cols<-intersect(names(labels),names(d)); if(!length(cols))stop("No engine capability columns are available.",call.=FALSE); long<-do.call(rbind,lapply(cols,function(z)data.frame(parameter=d$parameter,engine=labels[[z]],available=as.logical(d[[z]])))); ggplot2::ggplot(long,ggplot2::aes(x=.data$engine,y=.data$parameter,fill=.data$available))+ggplot2::geom_tile()+ggplot2::labs(x=NULL,y=NULL,title="Post-hoc engine capability")+theme_gamlss() }

\name{gamlss_cld}
\alias{gamlss_cld}
\title{Compact Letter Display for Pairwise GAMLSS Post-Hoc Results}
\usage{
gamlss_cld(
  x,
  alpha = 0.05,
  p_adjust = NULL,
  Letters = c(letters, LETTERS, ".")
)
}
\arguments{
\item{x}{A \code{gamlss_posthoc} result created with pairwise contrasts.}
\item{alpha}{Significance threshold strictly between zero and one.}
\item{p_adjust}{Optional method accepted by \code{stats::p.adjust()}. If
\code{NULL}, an existing adjusted-p-value column is preferred when available.}
\item{Letters}{Character vector passed to
\code{multcompView::multcompLetters()}.}
}
\description{
Creates compact letters from pairwise comparisons in a
\code{gamlss_posthoc} object. The letters are only a presentation layer and
should not replace effect estimates and uncertainty intervals.
}
\value{
The estimates data frame with an additional \code{.group} column.
}
\examples{
if (requireNamespace("gamlss", quietly = TRUE) &&
    requireNamespace("gamlss.dist", quietly = TRUE) &&
    requireNamespace("emmeans", quietly = TRUE) &&
    requireNamespace("multcompView", quietly = TRUE)) {
  set.seed(16)
  d <- data.frame(trt=factor(rep(c("A","B","C"), each=25)))
  d$y <- gamlss.dist::rGA(nrow(d), mu=c(A=2,B=2.5,C=3.2)[d$trt], sigma=.20)
  fit <- gamlss::gamlss(y~trt, family=gamlss.dist::GA, data=d, trace=FALSE)
  ph <- gamlss_posthoc(fit, "trt", estimand="parameter", what="mu",
                       population="reference", contrast="pairwise",
                       engine="emmeans", uncertainty="delta", data=d)
  # 1. Default 5% CLD
  gamlss_cld(ph)
  # 2. More liberal display threshold
  gamlss_cld(ph, alpha=.10)
  # 3. Holm-adjust raw p-values before constructing letters
  gamlss_cld(ph, p_adjust="holm")
}
}

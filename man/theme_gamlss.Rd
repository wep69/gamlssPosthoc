% Manually synchronized with Roxygen source for v0.3.0
\name{theme_gamlss}
\alias{theme_gamlss}
\alias{theme_gamlss_journal}
\alias{theme_gamlss_presentation}
\title{Scientific themes for GAMLSS figures}
\usage{
theme_gamlss(base_size = 11, base_family = "", grid = TRUE)

theme_gamlss_journal(base_size = 9, base_family = "", grid = FALSE)

theme_gamlss_presentation(base_size = 16, base_family = "", grid = TRUE)
}
\arguments{
  \item{base_size}{Base font size.}
  \item{base_family}{Base font family.}
  \item{grid}{Logical; whether major horizontal grid lines are shown.}
}
\value{A complete \code{ggplot2} theme object.}
\description{Three restrained themes for ordinary scientific figures, compact manuscript figures, and projected presentations. They do not impose a color palette on data layers.}
\examples{
if (requireNamespace("ggplot2", quietly = TRUE)) {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  p + theme_gamlss()
  p + theme_gamlss(base_size = 12)
  p + theme_gamlss(grid = FALSE)
  p + theme_gamlss_journal()
  p + theme_gamlss_journal(base_size = 10)
  p + theme_gamlss_journal(grid = TRUE)
  p + theme_gamlss_presentation()
  p + theme_gamlss_presentation(base_size = 18)
  p + theme_gamlss_presentation(grid = FALSE)
}
}

#' gamlssPosthoc: Marginal, Distributional, and Graphical Post-Hoc Inference for GAMLSS
#'
#' Tools for post-hoc inference and data-first scientific visualization after
#' generalized additive models for location, scale and shape (GAMLSS). The package treats the estimand, target population,
#' comparison scale, and uncertainty method as separate choices. It routes
#' supported parameter-wise requests to `emmeans` or `marginaleffects` and uses
#' full predictive distributions plus refit bootstrap for zero-adjusted,
#' smoother-based, and multi-parameter targets.
#'
#' @section Main functions:
#' * [gamlss_posthoc()] computes standardized estimates and scientific contrasts.
#' * [gamlss_posthoc_plan()] diagnoses model capabilities and recommends an engine.
#' * [gamlss_trend()] evaluates curves, derivatives, turning points, and optima.
#' * [gamlss_distribution_summary()] summarizes ordinary and zero-adjusted distributions.
#' * [gamlss_poly_compare()] compares already-fitted nested polynomial models.
#' * [gamlss_engine_info()] provides a compact legacy routing summary.
#' * [gamlss_cld()] adds optional compact-letter displays to pairwise results.
#' * [gamlss_plot_data()] constructs auditable plot data before rendering.
#' * `plot_gamlss_*()` functions visualize parameters, estimands, distributions,
#'   quantiles, contrasts, zero-adjusted components, trends, surfaces, fit,
#'   diagnostics, and model comparisons.
#' * `theme_gamlss*()` provides modifiable scientific ggplot2 themes.
#'
#' @references
#' Rigby, R. A. and Stasinopoulos, D. M. (2005). Generalized additive models
#' for location, scale and shape. *Journal of the Royal Statistical Society:
#' Series C (Applied Statistics)*, 54, 507--554.
#' \doi{10.1111/j.1467-9876.2005.00510.x}
#'
#' @importFrom graphics plot
#' @importFrom stats ave setNames
#' @importFrom utils capture.output
#' @keywords internal
"_PACKAGE"

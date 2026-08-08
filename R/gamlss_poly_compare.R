#' Compare already-fitted nested polynomial GAMLSS models
#'
#' Summarizes the generalized AIC (GAIC) and sequential likelihood-ratio (LR)
#' comparisons for nested GAMLSS models. This helper is intended for
#' quantitative polynomial regression and is deliberately separate from
#' polynomial contrasts among ordered factor levels.
#'
#' Models must be fitted to the same response and observations and supplied in
#' increasing order of complexity. The function does not itself verify formal
#' nestedness; the user remains responsible for supplying scientifically and
#' statistically comparable models.
#'
#' @param ... Two or more fitted GAMLSS models, or one named list containing
#'   such models, ordered from simpler to more complex.
#' @param k Positive penalty multiplying the effective degrees of freedom in
#'   GAIC. `k = 2` gives AIC; `k = log(n)` corresponds to a BIC-like penalty
#'   when `n` is the relevant sample size.
#' @return A data frame with model name, effective degrees of freedom, global
#'   deviance, GAIC, and sequential LR statistic, degrees-of-freedom difference,
#'   and p-value.
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE)) {
#'   set.seed(4)
#'   d <- data.frame(dose = rep(c(0, 2, 4, 6, 8), each = 10))
#'   d$y <- gamlss.dist::rGA(nrow(d),
#'     mu = 2 + 0.6 * d$dose - 0.05 * d$dose^2, sigma = 0.2)
#'   m1 <- gamlss::gamlss(y ~ dose, data = d, family = gamlss.dist::GA,
#'                        trace = FALSE)
#'   m2 <- gamlss::gamlss(y ~ dose + I(dose^2), data = d,
#'                        family = gamlss.dist::GA, trace = FALSE)
#'
#'   # Models must be ordered from simpler to more complex
#'   gamlss_poly_compare(linear = m1, quadratic = m2)
#' }
#' @export
gamlss_poly_compare <- function(..., k = 2) {
  if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
    stop("`k` must be one positive finite number.", call. = FALSE)
  }
  mods <- list(...)
  if (length(mods) == 1L && is.list(mods[[1]]) && !.gph_is_gamlss(mods[[1]])) mods <- mods[[1]]
  if (length(mods) < 2L) stop("Supply at least two fitted nested GAMLSS models.", call. = FALSE)
  if (!all(vapply(mods, .gph_is_gamlss, logical(1)))) stop("All supplied models must be GAMLSS fits.", call. = FALSE)
  if (is.null(names(mods)) || any(names(mods) == "")) names(mods) <- paste0("model", seq_along(mods))
  tab <- data.frame(
    model = names(mods),
    df = vapply(mods, function(m) m$df.fit %||% NA_real_, numeric(1)),
    global_deviance = vapply(mods, function(m) m$G.deviance %||% NA_real_, numeric(1)),
    GAIC = vapply(mods, function(m) (m$G.deviance %||% NA_real_) + k * (m$df.fit %||% NA_real_), numeric(1)),
    stringsAsFactors = FALSE
  )
  tab$LR_from_previous <- NA_real_
  tab$df_diff <- NA_real_
  tab$p_value <- NA_real_
  for (i in 2:length(mods)) {
    ddev <- tab$global_deviance[i - 1] - tab$global_deviance[i]
    ddf <- tab$df[i] - tab$df[i - 1]
    tab$LR_from_previous[i] <- ddev
    tab$df_diff[i] <- ddf
    if (is.finite(ddev) && is.finite(ddf) && ddf > 0) tab$p_value[i] <- stats::pchisq(ddev, df = ddf, lower.tail = FALSE)
  }
  tab
}

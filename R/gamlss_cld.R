#' Compact letter display for pairwise GAMLSS post-hoc results
#'
#' Creates compact letters from pairwise comparisons stored in a
#' `gamlss_posthoc` object. Compact-letter displays are intended only as a
#' presentation layer; effect estimates and uncertainty intervals should remain
#' the primary inferential output.
#'
#' @param x A `gamlss_posthoc` result created with `contrast = "pairwise"`.
#' @param alpha Numeric significance threshold strictly between 0 and 1.
#' @param p_adjust Optional method accepted by [stats::p.adjust()]. If `NULL`,
#'   an existing `p.value.adjusted` column is preferred; otherwise the stored
#'   `p.value` column is used as supplied.
#' @param Letters Character vector passed to `multcompView::multcompLetters()`.
#' @return The estimates data frame with an additional `.group` column.
#' @details
#' The letters are derived from the p-values stored in `x`. With bootstrap
#' uncertainty, the smallest attainable p-value is bounded by the number of
#' resamples, roughly `2/(B+1)` for a two-sided comparison, so a small `B`
#' combined with a multiplicity adjustment can leave every level in the same
#' group even when the estimates are far apart. Either raise `B` or use an
#' analytic uncertainty layer when a compact letter display is the goal.
#' @examples
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("emmeans", quietly = TRUE) &&
#'     requireNamespace("multcompView", quietly = TRUE)) {
#'   set.seed(3)
#'   d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = 25)))
#'   d$y <- gamlss.dist::rGA(nrow(d),
#'     mu = c(A = 2, B = 3, C = 4.5)[d$trt], sigma = 0.2)
#'   fit <- gamlss::gamlss(y ~ trt, data = d, family = gamlss.dist::GA,
#'                         trace = FALSE)
#'   ph <- gamlss_posthoc(fit, specs = "trt", estimand = "parameter",
#'                        what = "mu", population = "reference",
#'                        contrast = "pairwise", data = d)
#'   gamlss_cld(ph)
#' }
#'
#' # Normal response.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("emmeans", quietly = TRUE) &&
#'     requireNamespace("multcompView", quietly = TRUE)) {
#'   set.seed(41)
#'   d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = 20)))
#'   d$y <- gamlss.dist::rNO(nrow(d),
#'     mu = c(A = 10, B = 13, C = 17)[d$trt], sigma = 1.5)
#'   fit <- gamlss::gamlss(y ~ trt, family = gamlss.dist::NO,
#'                         data = d, trace = FALSE)
#'   ph <- gamlss_posthoc(fit, specs = "trt", estimand = "parameter",
#'                        what = "mu", population = "reference",
#'                        contrast = "pairwise", data = d)
#'   gamlss_cld(ph)
#' }
#'
#' # Weibull response, with a stricter significance threshold.
#' if (requireNamespace("gamlss", quietly = TRUE) &&
#'     requireNamespace("gamlss.dist", quietly = TRUE) &&
#'     requireNamespace("emmeans", quietly = TRUE) &&
#'     requireNamespace("multcompView", quietly = TRUE)) {
#'   set.seed(42)
#'   d <- data.frame(trt = factor(rep(c("A", "B", "C"), each = 25)))
#'   d$y <- gamlss.dist::rWEI(nrow(d),
#'     mu = c(A = 5, B = 7, C = 10)[d$trt], sigma = 3)
#'   fit <- gamlss::gamlss(y ~ trt, family = gamlss.dist::WEI,
#'                         data = d, trace = FALSE)
#'   ph <- gamlss_posthoc(fit, specs = "trt", estimand = "parameter",
#'                        what = "mu", population = "reference",
#'                        contrast = "pairwise", data = d)
#'   gamlss_cld(ph, alpha = 0.01)
#' }
#' @export
gamlss_cld <- function(x, alpha = 0.05, p_adjust = NULL,
                       Letters = c(letters, LETTERS, ".")) {
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  if (!is.null(p_adjust) && !p_adjust %in% stats::p.adjust.methods) {
    stop("`p_adjust` must be NULL or a method in stats::p.adjust.methods.", call. = FALSE)
  }
  if (!inherits(x, "gamlss_posthoc")) stop("`x` must be a gamlss_posthoc result.", call. = FALSE)
  if (!identical(x$contrast_method, "pairwise") || is.null(x$contrasts)) {
    stop("Run gamlss_posthoc(..., contrast='pairwise') first.", call. = FALSE)
  }
  .gph_require("multcompView", "to construct compact letter displays.")
  est <- x$estimates; con <- x$contrasts
  if (length(x$specs) != 1L) stop("CLD currently requires exactly one focal variable.", call. = FALSE)
  s <- x$specs[1]; by <- x$by
  pcol <- if (is.null(p_adjust) && "p.value.adjusted" %in% names(con)) "p.value.adjusted" else "p.value"
  if (!pcol %in% names(con)) stop("No p-values are available. For the distribution engine, request bootstrap uncertainty.", call. = FALSE)

  est_key <- if (length(by)) interaction(est[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(est)))
  con_key <- if (length(by)) interaction(con[by], drop = TRUE, lex.order = TRUE) else factor(rep("all", nrow(con)))
  est$.group <- NA_character_
  for (lev in unique(as.character(est_key))) {
    ei <- which(as.character(est_key) == lev)
    ci <- which(as.character(con_key) == lev)
    levels_here <- as.character(est[[s]][ei])
    # Map arbitrary level labels to safe tokens understood by multcompView.
    safe <- paste0("L", seq_along(levels_here))
    names(safe) <- levels_here
    p <- con[[pcol]][ci]
    if (!is.null(p_adjust) && pcol == "p.value") p <- stats::p.adjust(p, method = p_adjust)
    cn <- con$contrast[ci]
    mapped <- vapply(cn, function(z) {
      parts <- if (grepl(" vs ", z, fixed = TRUE)) strsplit(z, " vs ", fixed = TRUE)[[1]] else strsplit(z, " - ", fixed = TRUE)[[1]]
      if (length(parts) != 2L || !all(parts %in% names(safe))) return(NA_character_)
      paste0(safe[parts[1]], "-", safe[parts[2]])
    }, character(1))
    ok <- is.finite(p) & !is.na(mapped)
    if (!any(ok)) next
    pv <- p[ok]; names(pv) <- mapped[ok]
    L <- multcompView::multcompLetters(pv, threshold = alpha, Letters = Letters)$Letters
    reverse <- stats::setNames(names(safe), safe)
    letters_by_level <- stats::setNames(rep(NA_character_, length(levels_here)), levels_here)
    for (tok in names(L)) letters_by_level[reverse[tok]] <- L[tok]
    est$.group[ei] <- letters_by_level[as.character(est[[s]][ei])]
  }
  est
}

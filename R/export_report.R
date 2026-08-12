# Export and reporting --------------------------------------------------------

.gph_report_model_info <- function(res) {
  m <- res$model %||% NULL
  if (is.null(m)) return(data.frame())
  data.frame(
    family = tryCatch(.gph_model_family(m), error = function(e) NA_character_),
    converged = if (!is.null(m$converged)) as.logical(m$converged)[1L] else NA,
    iterations = if (!is.null(m$iter)) as.numeric(m$iter)[1L] else if (!is.null(m$no.cyc)) as.numeric(m$no.cyc)[1L] else NA_real_,
    global_deviance = m$G.deviance %||% NA_real_,
    effective_df = m$df.fit %||% NA_real_,
    AIC = tryCatch(stats::AIC(m), error = function(e) NA_real_),
    stringsAsFactors = FALSE
  )
}

.gph_result_table <- function(x,component=c("estimates","contrasts")) {
  component<-match.arg(component); if(inherits(x,"gamlss_posthoc")) return(as.data.frame(if(component=="estimates")x$estimates else x$contrasts))
  if(inherits(x,"gamlss_trend"))return(as.data.frame(x$values)); as.data.frame(x)
}

#' Convert a gamlssPosthoc result to a flextable
#' @param x Result object or data frame.
#' @param component Estimates or contrasts.
#' @param digits Number of numeric digits.
#' @param ... Arguments forwarded to `flextable::flextable()`.
#' @return A flextable object.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   if (requireNamespace("flextable", quietly = TRUE)) {
#'     f1 <- as_flextable(result)
#'     f2 <- as_flextable(result, component = "contrasts")
#'     f3 <- as_flextable(result, digits = 2)
#'   }
#' }
as_flextable <- function(x,component=c("estimates","contrasts"),digits=3,...) {
  .gph_require("flextable","for Word-ready tables."); d<-.gph_result_table(x,match.arg(component)); num<-vapply(d,is.numeric,logical(1)); d[num]<-lapply(d[num],round,digits=digits); ft<-flextable::flextable(d,...); flextable::autofit(ft)
}

#' Export results to a Word document
#' @param x Result object.
#' @param file Output `.docx` path.
#' @param component Estimates or contrasts.
#' @param title Document title.
#' @param digits Number of digits.
#' @return Invisibly, the file path.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   if (requireNamespace("flextable", quietly = TRUE) && requireNamespace("officer", quietly = TRUE)) {
#'     f1 <- export_to_word(result, tempfile(fileext = ".docx"))
#'     f2 <- export_to_word(result, tempfile(fileext = ".docx"), component = "contrasts")
#'     f3 <- export_to_word(result, tempfile(fileext = ".docx"), title = "GAMLSS results")
#'   }
#' }
export_to_word <- function(x,file,component=c("estimates","contrasts"),title="gamlssPosthoc results",digits=3) {
  .gph_require("officer","for Word export."); .gph_require("flextable","for Word export."); ft<-as_flextable(x,match.arg(component),digits); doc<-officer::read_docx(); doc<-officer::body_add_par(doc,title,style="heading 1"); doc<-flextable::body_add_flextable(doc,ft); print(doc,target=file); invisible(normalizePath(file,mustWork=FALSE))
}

#' Export results as a LaTeX table
#' @param x Result object.
#' @param file Optional output `.tex` path. If `NULL`, the LaTeX string is returned.
#' @param component Estimates or contrasts.
#' @param digits Number of digits.
#' @param caption Optional caption.
#' @return Character LaTeX table or output path invisibly when a file is written.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   t1 <- export_to_latex(result)
#'   t2 <- export_to_latex(result, component = "contrasts")
#'   f3 <- export_to_latex(result, tempfile(fileext = ".tex"), caption = "GAMLSS results")
#' }
export_to_latex <- function(x, file = NULL, component = c("estimates", "contrasts"),
                            digits = 3, caption = NULL) {
  d <- .gph_result_table(x, match.arg(component))
  num <- vapply(d, is.numeric, logical(1))
  d[num] <- lapply(d[num], round, digits = digits)

  esc <- function(z) {
    z <- as.character(z)
    z <- gsub("\\\\", "\\\\textbackslash{}", z, fixed = TRUE)
    for (ch in c("_", "&", "#", "%", "$"))
      z <- gsub(ch, paste0("\\\\", ch), z, fixed = TRUE)
    z
  }

  cols <- names(d)
  align <- paste0("l", paste(rep("r", max(0L, ncol(d) - 1L)), collapse = ""))
  body <- if (nrow(d)) vapply(seq_len(nrow(d)), function(i) {
    vals <- vapply(unname(as.list(d[i, , drop = FALSE])), esc, character(1))
    paste0(paste(vals, collapse = " & "), " \\\\")
  }, character(1)) else character()

  lines <- c(
    "\\begin{table}[ht]",
    "\\centering",
    if (!is.null(caption)) paste0("\\caption{", esc(caption), "}") else NULL,
    paste0("\\begin{tabular}{", align, "}"),
    "\\hline",
    paste0(paste(esc(cols), collapse = " & "), " \\\\"),
    "\\hline",
    body,
    "\\hline",
    "\\end{tabular}",
    "\\end{table}"
  )
  tex <- paste(lines, collapse = "\n")
  if (is.null(file)) return(tex)
  writeLines(tex, file, useBytes = TRUE)
  invisible(normalizePath(file, mustWork = FALSE))
}

#' Generate an automatic HTML, Word, or Markdown report
#' @param object Fitted GAMLSS model or `gamlss_posthoc` result.
#' @param output `html`, `word`, or `md`.
#' @param file Optional destination file.
#' @param title Report title.
#' @param include_plots Include an automatic post-hoc figure when possible.
#' @param ... Arguments used to construct a post-hoc result when `object` is a model.
#' @return Path to the generated report.
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
#'   result <- gamlss_posthoc(fit, "trt", estimand = "mean", contrast = "pairwise", uncertainty = "none", data = d)
#'   f1 <- generate_report(result, "md", tempfile(fileext = ".md"), include_plots = FALSE)
#'   f2 <- generate_report(result, "md", tempfile(fileext = ".md"), title = "Agronomic GAMLSS", include_plots = FALSE)
#'   f3 <- generate_report(fit, "md", tempfile(fileext = ".md"), include_plots = FALSE, specs = "trt", estimand = "mean", uncertainty = "none", data = d)
#' }
generate_report <- function(object, output = c("html", "word", "md"), file = NULL,
                            title = "gamlssPosthoc report", include_plots = TRUE, ...) {
  output <- match.arg(output)
  res <- if (inherits(object, "gamlss_posthoc")) object else gamlss_posthoc(object, ...)
  if (is.null(file)) file <- tempfile(fileext = switch(output, html = ".html", word = ".docx", md = ".md"))

  make_plot <- function() {
    if (!isTRUE(include_plots) || !requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
    tryCatch(ggplot2::autoplot(res), error = function(e) NULL)
  }

  if (output == "md") {
    fig_path <- NULL
    p <- make_plot()
    if (!is.null(p)) {
      fig_path <- paste0(tools::file_path_sans_ext(file), "_figure.png")
      tryCatch(ggplot2::ggsave(fig_path, p, width = 7, height = 4.5, dpi = 180), error = function(e) fig_path <<- NULL)
    }
    est_txt <- paste(capture.output(print(res$estimates, row.names = FALSE)), collapse = "\n")
    con_txt <- if (is.null(res$contrasts)) "None" else paste(capture.output(print(res$contrasts, row.names = FALSE)), collapse = "\n")
    diag <- .gph_report_model_info(res)
    diag_txt <- if (!nrow(diag)) "Unavailable" else paste(capture.output(print(diag, row.names = FALSE)), collapse = "\n")
    lines <- c(
      paste0("# ", title), "",
      paste0("- Engine: `", res$engine, "`"),
      paste0("- Estimand: ", res$estimand_info$target[1]),
      paste0("- Population: `", res$population, "`"),
      paste0("- Weighting: `", res$weighting, "`"),
      paste0("- Uncertainty: `", res$uncertainty_method, "`"), "",
      "## Model diagnostics", "", "```text", diag_txt, "```", "",
      "## Estimates", "", "```text", est_txt, "```", "",
      "## Contrasts", "", "```text", con_txt, "```",
      if (!is.null(fig_path)) c("", "## Figure", "", paste0("![](", basename(fig_path), ")")) else NULL
    )
    writeLines(lines, file, useBytes = TRUE)
    return(normalizePath(file, mustWork = FALSE))
  }

  .gph_require("rmarkdown", "for HTML/Word report generation.")
  rmd <- tempfile(fileext = ".Rmd")
  fmt <- if (output == "html") "html_document" else "word_document"
  tmp <- tempfile(fileext = ".rds")
  saveRDS(res, tmp)
  tmp_norm <- gsub("\\\\", "/", tmp)
  rmd_lines <- c(
    "---", paste0('title: "', title, '"'), paste0("output: ", fmt), "---", "",
    "```{r setup, echo=FALSE, message=FALSE, warning=FALSE}",
    paste0("res <- readRDS('", tmp_norm, "')"),
    "```", "",
    "## Estimand", "", "```{r}", "res$estimand_info", "```", "",
    "## Model diagnostics", "", "```{r}", ".gph_report_model_info <- getFromNamespace('.gph_report_model_info', 'gamlssPosthoc')", ".gph_report_model_info(res)", "```", "",
    "## Estimates", "", "```{r}", "res$estimates", "```", "",
    "## Contrasts", "", "```{r}", "res$contrasts", "```"
  )
  if (isTRUE(include_plots)) {
    rmd_lines <- c(rmd_lines, "", "## Figure", "",
                   "```{r, fig.width=7, fig.height=4.5}",
                   "if (requireNamespace('ggplot2', quietly=TRUE)) print(ggplot2::autoplot(res))",
                   "```")
  }
  writeLines(rmd_lines, rmd, useBytes = TRUE)
  rmarkdown::render(rmd, output_file = normalizePath(file, mustWork = FALSE),
                    quiet = TRUE, envir = new.env(parent = globalenv()))
  normalizePath(file, mustWork = FALSE)
}

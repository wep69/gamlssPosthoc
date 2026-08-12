# Conteúdo do gamlssPosthoc 0.3.0

## Núcleo

- `R/`: adapters, estimands, zero-adjusted, posthoc, trends, gráficos, métodos tidy e exportação.
- `man/`: 32 páginas Rd.
- `tests/testthat/`: testes unitários, integração e regressão visual.

## Documentação de usuário

- `README.md`
- `README_interactive.html` (árvore de desenvolvimento)
- `vignettes/workflow.Rmd`
- `vignettes/visualization.Rmd`
- `vignettes/advanced-workflows.Rmd`
- `vignettes/missing-data.Rmd`
- `inst/GLOSSARY.md`
- `inst/cheatsheet/gamlssPosthoc-cheatsheet.pdf`

## Exemplos avançados

`inst/examples/` contém 25 scripts, incluindo meta-análise de resultados, longitudinal, zero-adjusted, cross-validation, multiple imputation e report generation.

## Ferramentas

- `tools/benchmark.R` é destinado a benchmark e é mantido no source.
- scripts de auditoria/preflight permanecem apenas na árvore de desenvolvimento e são filtrados por `.Rbuildignore`.

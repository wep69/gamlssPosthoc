# Implementação do gamlssPosthoc 0.3.0

## Objetivo

A versão 0.3.0 transforma `gamlssPosthoc` em uma camada de inferência e visualização científica **data-first** para modelos GAMLSS. O núcleo 0.2.0 de estimandos explícitos, padronização, zero-adjusted genérico, contrastes científicos e incerteza em camadas foi preservado e ampliado.

A arquitetura é:

\[
\text{modelo}\rightarrow\text{estimando/distribuição}\rightarrow\text{dados gráficos}\rightarrow\text{ggplot}\rightarrow\text{exportação/relatório}.
\]

## Implementações 1–14 da camada gráfica

1. `plot_gamlss_parameters()`: efeitos de múltiplos parâmetros detectados dinamicamente, nas escalas `response` ou `link`.
2. `plot_gamlss_estimand()`: média, variância, quantil, massa em zero, parâmetro ou estimando customizado.
3. `plot_gamlss_contrasts()`: forest/estimation plots; integração opcional com `ggdist` quando draws existem.
4. `plot_gamlss_distribution()`: densidade, CDF e sobrevivência de distribuições preditivas padronizadas.
5. `plot_gamlss_quantiles()`: linhas de quantis e fan plots; quantis são resolvidos sobre a **mistura marginal**, não obtidos pela média de quantis condicionais.
6. `plot_gamlss_zero_adjusted()`: decomposição gráfica de `prob_zero`, `positive_mean` e `marginal_mean`.
7. `plot_gamlss_trend()`: curvas com intervalos ponto a ponto ou simultâneos e `ggdist::stat_lineribbon()` quando aplicável.
8. `plot_gamlss_derivative()`: derivadas de primeira e segunda ordem e referência em zero.
9. `plot_gamlss_optimum()`: máximo/mínimo, refinamento local e distribuição bootstrap da localização quando disponível.
10. `plot_gamlss_surface()`: heatmap, contornos ou combinação para dois preditores quantitativos.
11. `plot_gamlss_fit()`: observações + mediana/quantis preditivos; aceita eixo quantitativo ou fator.
12. `plot_gamlss_diagnostics()`: residual-vs-fitted, QQ, detrended-QQ/worm-style, densidade, PIT e rootogram quando aplicável.
13. `plot_gamlss_compare()`: critérios de modelos (`AIC`, global deviance, df) ou comparação de um estimando.
14. `theme_gamlss()`, `theme_gamlss_journal()` e `theme_gamlss_presentation()`: temas científicos baseados apenas na API pública do `ggplot2`.

## Camada de dados separada

`gamlss_plot_data()` é o contrato entre cálculo e visualização. Os tipos suportados são:

- `parameters`
- `estimand`
- `contrasts`
- `distribution`
- `quantiles`
- `zero_adjusted`
- `trend`
- `derivative`
- `optimum`
- `surface`

Isso permite auditar numericamente qualquer figura, reutilizar os dados com um `ggplot` próprio e testar a estatística sem depender da aparência do gráfico.

## Autoplot e métodos S3

Foram acrescentados métodos `autoplot()`/`plot()` para:

- `gamlss_posthoc`
- `gamlss_trend`
- `gamlss_distribution_summary`
- `gamlss_posthoc_plan`

Além disso:

- `tidy.gamlss_posthoc()`
- `glance.gamlss_posthoc()`
- `augment.gamlss_posthoc()`
- `parameters::model_parameters.gamlss_posthoc()`

## Interoperabilidade e exportação

Foram implementados:

- `as_flextable()`
- `export_to_word()`
- `export_to_latex()`
- `generate_report()`

`generate_report()` inclui informação do modelo, estimando, estimativas, contrastes, diagnósticos básicos disponíveis e figura.

## Infraestrutura de distribuição

A implementação evita codificar gráficos por família. Quando possível, predições são convertidas em distribuições completas via `gamlss.dist::GAMLSS()`/`distributions3`; zero-adjusted é tratado como mistura entre massa em zero e a distribuição positiva.

## Recursos de distribuição do pacote

- 4 vignettes R Markdown.
- 25 scripts em `inst/examples/`, incluindo meta-análise de resultados, longitudinal, zero-adjusted, K-fold, múltipla imputação + Rubin e relatórios.
- `pkgdown` configurado.
- cheatsheet PDF de duas páginas + fonte Markdown.
- `GLOSSARY.md` instalado em `inst/`.
- benchmark em `tools/benchmark.R`.
- 32 páginas `.Rd`.
- 28 funções públicas exportadas, cada uma com **três chamadas de exemplo** em seu help.

## Decisão sobre gamlss.ggplots

`gamlss.ggplots` foi considerado como antecedente, mas não é dependência. O pacote foi arquivado no CRAN em 28/11/2025; portanto, os diagnósticos necessários nesta versão foram implementados nativamente com APIs públicas do `ggplot2`.

## Referências centrais

- Rigby RA, Stasinopoulos DM (2005). doi:10.1111/j.1467-9876.2005.00510.x.
- Stasinopoulos DM, Rigby RA (2007). doi:10.18637/jss.v023.i07.
- Arel-Bundock V, Greifer N, Heiss A (2024). doi:10.18637/jss.v111.i09.
- Dunn PK, Smyth GK (1996). doi:10.1080/10618600.1996.10474708.
- Kay M (2024). doi:10.1109/TVCG.2023.3327195.
- Lenth RV (2016). doi:10.18637/jss.v069.i01.

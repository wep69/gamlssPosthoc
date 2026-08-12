# Matriz de testes – gamlssPosthoc 0.3.0

## Estratégia

A validação é dividida em quatro níveis:

1. **invariantes estáticos** da arquitetura;
2. **validação matemática independente** em Python;
3. **testes unitários/integrados R** escritos em `testthat`;
4. **regressão visual** especificada em `vdiffr`.

## Cobertura dos requisitos

| Requisito | Teste principal |
|---|---|
| parâmetros múltiplos | estrutura long + plots response/link |
| estimando gráfico | equivalência entre `gamlss_plot_data(type='estimand')` e objeto posthoc |
| contrastes | diferença, razão, log-razão, % e direção consistente |
| densidade/CDF/sobrevivência | integral da densidade e identidade CDF+S |
| quantile fan | monotonicidade + quantil marginal comparado a Monte Carlo |
| zero-adjusted | média/variância/quantil e três componentes gráficos |
| tendência | curvas, bandas e draws |
| derivadas | primeira/segunda ordem versus funções conhecidas |
| ótimo | localização/resposta e draws bootstrap de x* |
| superfície | grade bidimensional e objetos ggplot |
| fit observado/predito | eixo numérico e fator |
| diagnósticos | QQ, PIT, residual-fitted, density, worm-style |
| comparação de modelos | AIC/global deviance/df e estimando |
| temas | objetos `theme` e compatibilidade ggplot |
| data-first | todos os tipos retornam data.frame auditável |
| autoplot | quatro classes S3 retornam ggplot |
| tidy/glance/augment | schemas e classes esperadas |
| parameters/modelsummary | extração padronizada opcional |
| Word/LaTeX/flextable | classes/arquivos quando dependências existem |
| relatório | inclui modelo, estimando, figura e diagnóstico |
| visual regression | 6 `expect_doppelganger()` com `theme_test()` |

## Contagens finais

- `test_that()`: **61** blocos.
- `vdiffr::expect_doppelganger()`: **6** especificações.
- invariantes arquiteturais no validador estático: **39/39**.
- validação matemática do núcleo: **19/19**.
- validação matemática gráfica: **12/12**.

## Testes visuais

As seis expectativas cobrem:

1. efeitos multi-parâmetros;
2. distribuição preditiva;
3. diagnóstico QQ;
4. fan de quantis;
5. forest plot de contraste;
6. curva da primeira derivada.

O arquivo `tools/run_vdiffr.R` habilita explicitamente a suíte local de snapshots. A primeira execução em uma máquina com R deve criar os SVGs candidatos e requer revisão humana com `testthat::snapshot_review()` antes de aceitá-los.

## Limitação do ambiente atual

Não há executável R/Rscript no container, e a rede está isolada. Portanto os 61 testes R e os 6 snapshots foram **escritos e auditados**, mas não executados aqui. Não se deve interpretar a validação Python/estática como substituta de `R CMD check --as-cran`.

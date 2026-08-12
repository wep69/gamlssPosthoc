# Validação da versão 0.3.0

## Resultado disponível neste ambiente

### Auditoria estática

`tools/static_validate.py` foi executado sobre a árvore final e retornou:

- 28 funções exportadas;
- 28 correspondências função/usage documentadas;
- 16 arquivos em `R/`;
- 7 arquivos de teste;
- 25 scripts de exemplos avançados;
- **39/39 invariantes arquiteturais satisfeitos**;
- **0 falhas**.

### Validação matemática do núcleo

`tools/mathematical_validation.py`:

- **19/19 testes aprovados**.
- Inclui média, variância e quantis ZA-Gamma e ZA-Lognormal; quatro escalas de contraste; derivadas; turning point e refinamento quadrático.

### Validação matemática da camada gráfica

`tools/graphics_math_validation.py`:

- **12/12 testes aprovados**.
- Quantil de mistura marginal versus simulação de Monte Carlo.
- Fan de quantis ordenado.
- Quantil zero-adjusted versus Monte Carlo.
- Integral da densidade preditiva padronizada ≈ 1.
- CDF + sobrevivência = 1.
- Orientação de razão e mudança percentual.
- Derivada numérica.
- Localização e resposta do ótimo.
- Média e variância do PIT sob modelo Normal calibrado.

## Documentação

- 32 arquivos `.Rd`.
- 28 funções exportadas, todas com três chamadas de exemplo no help.
- 4 vignettes R Markdown.
- 25 exemplos em `inst/examples/`.
- `pkgdown` configurado.
- cheatsheet PDF + Markdown.
- glossário instalado.
- referências verificadas em fontes primárias/oficiais.

## O que ainda deve ser executado em R

O ambiente atual não contém `R`/`Rscript`; tentativas anteriores de instalação foram bloqueadas por DNS/rede. Consequentemente, ainda são obrigatórios em uma máquina R:

```r
roxygen2::roxygenise()
testthat::test_local()
```

seguido por:

```sh
R CMD build .
R CMD check --as-cran gamlssPosthoc_0.3.0.tar.gz
```

Também devem ser executados:

```r
Sys.setenv(GAMLSSPOSTHOC_VDIFFR = 'true')
source('tools/run_vdiffr.R')
testthat::snapshot_review()
pkgdown::build_site()
```

O script `tools/cran_preflight.R` automatiza a maior parte desse fluxo.

## Interpretação correta do status

A versão 0.3.0 é uma **candidata de desenvolvimento amplamente auditada**, mas não pode ser declarada aprovada pelo CRAN ou livre de erros de runtime até que o check real em R seja executado.

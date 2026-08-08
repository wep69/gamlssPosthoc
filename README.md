# gamlssPosthoc 0.2.0

**Inferência marginal e distribucional pós-GAMLSS com estimandos explícitos, zero-adjusted genérico, contrastes científicos e regressão quantitativa.**

`gamlssPosthoc` organiza o pós-processamento de modelos `gamlss`/`gamlssZadj` sem assumir que toda “média ajustada” representa a mesma quantidade. A versão 0.2.0 separa formalmente:

1. **estimando**: parâmetro, média matemática, variância, quantil, massa exata em zero ou função customizada;
2. **população-alvo**: observada, balanceada ou perfil de referência;
3. **geometria da comparação**: pareada, referência, sequencial ou polinomial;
4. **escala científica**: diferença, razão, log-razão ou mudança percentual;
5. **incerteza**: delta, simulação, bootstrap com refit ou apenas estimativa pontual.

## O que mudou em 0.2.0

### 1. Zero-adjusted genérico

Para `gamlssZadj`, o pacote reconstrói a distribuição positiva completa com `gamlss.dist::GAMLSS()` quando possível. Se

\[
P(Y=0)=h
\]

e a parte positiva é \(Y_+\), então

\[
E(Y)=(1-h)E(Y_+)
\]

\[
Var(Y)=(1-h)Var(Y_+)+h(1-h)E(Y_+)^2.
\]

Os quantis são obtidos da distribuição completa:

\[
Q_Y(p)=0,\quad p\le h,
\]

\[
Q_Y(p)=Q_+\left(\frac{p-h}{1-h}\right),\quad p>h.
\]

Isso remove a antiga limitação GA/GAF. Famílias positivas compatíveis, como `GG`, podem ser processadas sem assumir que `mu` seja a média.

Para famílias locais/customizadas que não possam ser convertidas automaticamente, use `positive_dist_fun`.

### 2. Adapter de modelo

O núcleo não percorre mais uma lista fixa `mu/sigma/nu/tau`. Ele descobre `object$parameters`, fórmulas, links e smoothers por uma camada interna de adapter. Isso mantém o pacote utilizável com a infraestrutura atual do `gamlss` e facilita adapters futuros sem expor detalhes internos na API pública.

### 3. Mais responsabilidade para `marginaleffects`

Quando o alvo é um parâmetro distribucional de um `gamlss` ordinário, `marginaleffects` pode assumir:

- `avg_predictions()` para padronização;
- `avg_comparisons()` para diferenças, razões e log-razões; a mudança percentual é transformada a partir da razão das médias marginais, preservando o mesmo estimando do motor distribucional;
- `avg_slopes()` para derivadas de primeira ordem em regressão quantitativa;
- `inferences(method = "simulation")` quando solicitado e suportado.

No roteamento `engine = "auto"`, uma falha de capacidade em um objeto específico retorna ao motor de distribuição em vez de produzir uma resposta parcial silenciosa. Se `engine = "marginaleffects"` for solicitado explicitamente, a falha é reportada como erro para não mascarar uma incompatibilidade.

### 4. O estimando fica registrado

Todo resultado de `gamlss_posthoc()` inclui `estimand_info`, por exemplo:

```text
Target:       marginal response mean including the zero mass
Definition:   E(Y|x) = [1-P(Y=0|x)] E(Y+|Y>0,x)
Population:   observed
Weighting:    proportional
Scale:        response/distribution
```

### 5. Incerteza em camadas

```r
uncertainty = "delta"
uncertainty = "simulation"
uncertainty = "bootstrap"
uncertainty = "none"
```

O bootstrap completo pode ser paramétrico, por casos ou por clusters. `auto` não dispara centenas de refits ocultos; `gamlss_posthoc_plan()` informa quando bootstrap explícito é recomendado para intervalos de estimandos derivados.

### 6. Contrastes em escala científica

A pergunta “quais níveis?” é separada da pergunta “qual efeito?”.

```r
contrast = "reference"
comparison = "difference"
```

ou

```r
contrast = "reference"
comparison = "ratio"
```

ou

```r
comparison = "percent_change"
```

Assim, uma comparação pode ser apresentada diretamente como

\[
100\left(\frac{E(Y_A)}{E(Y_B)}-1\right),
\]

sem depender da interpretação implícita da escala de link.

### 7. Regressão quantitativa ampliada

`gamlss_trend()` agora suporta:

```r
method = "curve"
method = "derivative", derivative_order = 1
method = "derivative", derivative_order = 2
method = "turning_points"
method = "optimum", optimum = "maximum"
```

Com bootstrap, também pode construir bandas simultâneas a partir do máximo desvio padronizado entre curvas bootstrap. Para derivadas de primeira ordem de parâmetros ordinários, `delta` e `simulation` podem ser delegados a `marginaleffects`; para alvos derivados da distribuição, essas camadas recaem em bootstrap com refit.

### 8. Diagnóstico verdadeiro

Antes de executar uma análise:

```r
plan <- gamlss_posthoc_plan(
  fit,
  estimand = "mean",
  contrast = "pairwise",
  comparison = "ratio"
)
print(plan)
```

O diagnóstico mostra:

- classe e família;
- parâmetros descobertos;
- fórmulas e links;
- smoothers por parâmetro;
- dependências opcionais instaladas;
- elegibilidade de `emmeans`, `marginaleffects` e motor distribucional;
- engine recomendado;
- camada de incerteza recomendada;
- avisos estruturais.

`gamlss_engine_info()` continua disponível como wrapper resumido para compatibilidade.

---

## Instalação local

```r
install.packages(c(
  "gamlss", "gamlss.dist", "distributions3",
  "gamlss.inf", "emmeans", "marginaleffects",
  "future", "future.apply", "multcompView"
))

install.packages("gamlssPosthoc_0.2.0.tar.gz", repos = NULL, type = "source")
```

> O tarball para submissão ao CRAN deve ser criado por `R CMD build`, não por compactação manual.

---

## Exemplo 1: contraste científico em Gamma

```r
library(gamlss)
library(gamlss.dist)
library(gamlssPosthoc)

set.seed(1)
D <- data.frame(trt = factor(rep(c("T0", "T1", "T2"), each = 50)))
D$y <- rGA(nrow(D), mu = c(T0=3, T1=3.6, T2=4.4)[D$trt], sigma=.25)
fit <- gamlss(y ~ trt, family = GA, data = D, trace = FALSE)

ans <- gamlss_posthoc(
  fit,
  specs = "trt",
  estimand = "mean",
  population = "observed",
  contrast = "reference",
  comparison = "percent_change",
  uncertainty = "none",
  data = D
)

ans$estimand_info
ans$estimates
ans$contrasts
```

## Exemplo 2: zero-adjusted Generalized Gamma

```r
library(gamlss.inf)
library(distributions3)

set.seed(2)
D <- data.frame(trt = factor(rep(c("Control", "Bio"), each = 80)))
mu <- ifelse(D$trt == "Control", 2, 3)
h  <- ifelse(D$trt == "Control", .40, .18)
ypos <- rGG(nrow(D), mu = mu, sigma=.35, nu=.7)
D$y <- ifelse(rbinom(nrow(D), 1, h) == 1, 0, ypos)

fitz <- gamlssZadj(
  y = y,
  mu.formula = ~ trt,
  xi0.formula = ~ trt,
  family = GG,
  data = D,
  trace = FALSE
)

nd <- data.frame(trt = factor(levels(D$trt), levels = levels(D$trt)))
gamlss_distribution_summary(fitz, nd, data = D)

gamlss_posthoc(
  fitz, specs = "trt",
  estimand = "mean",
  contrast = "pairwise",
  comparison = "ratio",
  uncertainty = "none",
  data = D
)
```

## Exemplo 3: dose quantitativa

```r
set.seed(3)
D <- data.frame(dose = seq(0, 150, length.out = 180))
eta <- log(2) + .012*D$dose - .000045*D$dose^2
D$biomass <- rGA(nrow(D), mu=exp(eta), sigma=.20)
fit <- gamlss(biomass ~ dose + I(dose^2), family=GA, data=D, trace=FALSE)
g <- seq(0, 150, length.out=151)

gamlss_trend(fit, "dose", at_x=g,
             method="derivative", derivative_order=1,
             estimand="mean", uncertainty="none", data=D)

gamlss_trend(fit, "dose", at_x=g,
             method="turning_points",
             estimand="mean", uncertainty="none", data=D)$special_points

gamlss_trend(fit, "dose", at_x=g,
             method="optimum", optimum="maximum",
             estimand="mean", uncertainty="none", data=D)$special_points
```

## `emmeans`: onde continua sendo preferido

O suporte documentado de `emmeans` a `gamlss` permanece focado em `what = "mu"`, `"sigma"`, `"nu"` ou `"tau"` e não cobre a parte selecionada quando ela contém um smoother como `pb()`. Além disso, nesta versão do `gamlssPosthoc`, o roteamento automático para `emmeans` exige `population = "reference"` e `comparison = "difference"`; isso impede que uma solicitação para a população observada seja silenciosamente trocada pelo reference grid do `emmeans`.

Fonte técnica: <https://rvlenth.github.io/emmeans/articles/models.html>

## `marginaleffects`: onde assume mais trabalho

`marginaleffects` oferece padronização de predições, comparações em múltiplas escalas, slopes e inferência por simulação. O pacote utiliza essas capacidades quando o alvo é um parâmetro de um `gamlss` ordinário e mantém teste de capacidade em runtime. Razões são razões de médias marginalizadas; `percent_change` é definido como `100 * (ratio - 1)`, não como a média de mudanças percentuais unidade a unidade.

Fontes técnicas:

- <https://marginaleffects.com/man/r/predictions.html>
- <https://marginaleffects.com/man/r/comparisons.html>
- <https://marginaleffects.com/man/r/slopes.html>
- <https://marginaleffects.com/man/r/inferences.html>

## Arquivos de exemplo

`inst/examples/` contém exemplos independentes de:

1. fatorial Gamma + `emmeans`;
2. smoother `pb()`;
3. GAF e potência média-variância;
4. regressão polinomial quantitativa;
5. contrastes polinomiais com espaçamento desigual;
6. zero-adjusted Gamma;
7. ZA-GAF customizada;
8. `marginaleffects` opcional;
9. compact letter display;
10. **zero-adjusted GG genérico**;
11. **diferença/razão/log-razão/%**;
12. **derivadas, turning points e optimum**;
13. **diagnóstico completo**;
14. **marginaleffects como engine de comparação**.

## Validação

O pacote inclui:

```r
tools/cran_preflight.R
```

que, em uma máquina com R e acesso às dependências, executa Roxygen, testes,
vignette, `R CMD build` e `R CMD check --as-cran` sobre o tarball produzido.

No ambiente em que esta versão foi construída, a instalação temporária de R
continua bloqueada por isolamento de rede. Por isso, os testes de runtime em R
devem ser concluídos em uma máquina com R antes da submissão ao CRAN. Nesta
refatoração foram executadas e aprovadas **20 invariantes arquiteturais** na
auditoria estática e **19/19 verificações matemáticas independentes**. A suíte
R preparada contém **39 blocos `test_that()`**, além dos 14 exemplos completos.

A auditoria de dependências também foi atualizada contra o CRAN em 2026-08-08:
`marginaleffects` 0.32.0, `emmeans` 2.0.4, `gamlss` 5.5-0,
`gamlss.dist` 6.1-1, `gamlss.inf` 1.0-2 e `distributions3` 0.2.3. O requisito
mínimo de `distributions3` foi corrigido para `>= 0.2.1`, versão em que a API
`is_discrete()` já está disponível, evitando exigir uma versão inexistente no
CRAN.

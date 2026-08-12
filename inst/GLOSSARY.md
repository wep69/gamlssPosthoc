# Glossario tecnico - gamlssPosthoc

## Conditional mean
`E(Y | X=x)`: media da distribuicao condicional para um perfil de covariaveis.

## Marginal/standardized mean
Media de predicoes condicionais sobre uma populacao-alvo explicitamente declarada: `sum(w_i E(Y|X_i,g))`.

## Distributional parameter
Parametro da familia GAMLSS (`mu`, `sigma`, `nu`, `tau` ou outro detectado). Nao deve ser chamado de media sem verificar a parametrizacao da familia.

## Estimand
Quantidade cientifica alvo: parametro, media, variancia, quantil, massa em zero ou funcao customizada.

## Zero-adjusted response
Mistura com massa pontual `h=P(Y=0)` e distribuicao positiva `F+`. A media marginal e `(1-h)E(Y+)`.

## Difference / ratio / log ratio / percent change
Para estimandos A e B: `A-B`, `A/B`, `log(A/B)` e `100(A/B-1)`.

## Parametric bootstrap
Simula novas respostas do modelo ajustado, reajusta o modelo e recalcula o estimando. Avalia a incerteza sob o modelo gerador assumido.

## Case bootstrap
Reamostra unidades observacionais. Deve respeitar a unidade amostral independente.

## Cluster bootstrap
Reamostra clusters inteiros (blocos, sujeitos, parcelas principais etc.) para preservar dependencia intracluster.

## Pointwise band
Intervalo com cobertura nominal em cada ponto isolado da curva.

## Simultaneous band
Banda calibrada para cobrir a curva inteira com probabilidade aproximadamente nominal.

## Quantile fan
Conjunto de bandas definidas por quantis preditivos. Mostra localizacao, dispersao e assimetria ao longo de um preditor.

## Worm plot
Representacao detrended de um QQ plot. No ecossistema GAMLSS, worm plots sao ferramentas importantes para avaliar desajustes locais da distribuicao.

## PIT
Probability integral transform. Para previsoes continuas bem calibradas, `F_i(y_i)` deve ser aproximadamente Uniforme(0,1). Para dados discretos, randomizacao apropriada e necessaria.

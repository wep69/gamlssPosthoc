# gamlssPosthoc 0.3.0 - Quick reference

## Decision flow
1. What do you want? parameter / mean / variance / quantile / P(Y=0) / custom.
2. Which population? observed / balanced / reference.
3. Which comparison? difference / ratio / log ratio / percent change.
4. Which engine? inspect with `gamlss_posthoc_plan()`.
5. Which uncertainty? delta / simulation / bootstrap / none.
6. Build data first with `gamlss_plot_data()`, then plot.

## Core
`gamlss_posthoc()` - inference and contrasts
`gamlss_posthoc_plan()` - engine diagnosis
`gamlss_distribution_summary()` - full predictive distribution summaries
`gamlss_trend()` - curves, derivatives, turning points, optima

## Graphics
`plot_gamlss_parameters()`
`plot_gamlss_estimand()`
`plot_gamlss_contrasts()`
`plot_gamlss_distribution()`
`plot_gamlss_quantiles()`
`plot_gamlss_zero_adjusted()`
`plot_gamlss_trend()`
`plot_gamlss_derivative()`
`plot_gamlss_optimum()`
`plot_gamlss_surface()`
`plot_gamlss_fit()`
`plot_gamlss_diagnostics()`
`plot_gamlss_compare()`

## Key formulas
Standardized estimand: T_g = sum(w_i T(F_ig)) / sum(w_i)
Zero-adjusted mean: E(Y)=(1-h)E(Y+)
Zero-adjusted variance: Var(Y)=(1-h)Var(Y+) + h(1-h)E(Y+)^2
Percent change: 100(A/B-1)

## Interoperability
`generics::tidy()` / `glance()` / `augment()`
`modelsummary::modelsummary()` via tidy/glance
`parameters::model_parameters()` delayed S3 method
`as_flextable()`, `export_to_word()`, `export_to_latex()`, `generate_report()`

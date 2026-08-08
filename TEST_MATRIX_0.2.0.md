# Test matrix — gamlssPosthoc 0.2.0

This development document maps each requested 0.2.0 feature to validation artifacts.

| Requirement | Main implementation | Static/math validation | Prepared R integration tests |
|---|---|---|---|
| 1. Generic zero-adjusted | `R/zero_adjusted.R` | ZA Gamma and ZA Lognormal mean/variance/quantiles; generic constructor invariant | zero-adjusted Gamma; generic GG; xi0; full mean/variance/quantiles |
| 2. Remove fixed parameter dependency | `R/adapters.R` | dynamic `object$parameters`, formulas/links fallback, constructor-formals invariants | fake nonconventional `alpha`/`shapeX`; ordinary model parameter discovery |
| 3. More responsibility to marginaleffects | `R/gamlss_posthoc.R`, `R/estimands.R` | avg_predictions/comparisons/slopes invariants; pairwise direction invariant | forced ME predictions; ratio comparisons; percent-change equivalence; orientation agreement |
| 4. Explicit estimand | `R/estimands.R`, `R/utils.R` | formal estimand metadata invariant | target population and plan routing tests |
| 5. Layered uncertainty | `R/estimands.R`, core/trend functions | uncertainty-router invariants | planner delta→bootstrap; bootstrap engine change; refit bootstrap tests |
| 6. Scientific-scale contrasts | `R/utils.R` | difference/ratio/log-ratio/% mathematical checks | distribution and ME ratio/% tests; multiplicity handling |
| 7. Quantitative regression | `R/gamlss_trend.R` | first/second derivative, turning point, local optimum mathematical checks | derivative2, turning point, optimum, pb smoother routing |
| 8. True diagnostic | `R/gamlss_posthoc_plan.R` | diagnostic-plan invariant | smoother capability matrix; uncertainty fallbacks; numeric-weight nonlinear warning |

## Cross-engine conventions

- Pairwise direction: earlier level minus later level for the internal/emmeans-compatible convention; the marginaleffects adapter requests `revpairwise` to match it.
- Reference geometry: non-reference level relative to reference.
- Sequential geometry: current level relative to previous level.
- Ratio null: 1; difference/log-ratio/percent-change null: 0.
- Percent change: `100 * (ratio of standardized marginal means - 1)`.

## Runtime status

The R test suite is prepared but not executed in this container because `R` is unavailable and the container cannot download R/dependencies. Static and independent mathematical validations were executed here.

# Technical references and compatibility notes

Checked on 2026-08-08.

1. **emmeans: Models supported**
   - https://rvlenth.github.io/emmeans/articles/models.html
   - Current documentation lists `gamlss` with `what = c("mu", "sigma", "nu", "tau")` and states that the selected component is not supported when it contains a smoothing method such as `pb()`.

2. **gamlss::prodist()**
   - https://rdrr.io/cran/gamlss/man/prodist.gamlss.html
   - Builds full fitted/predicted distribution objects through `predictAll()` and `distributions3`.
   - Documentation explicitly states that these predicted distributions do not capture uncertainty in estimated parameters.

3. **gamlss.inf::gamlssZadj()**
   - https://rdrr.io/cran/gamlss.inf/man/gamlssZadj.html
   - Fits a positive GAMLSS component plus an additional mass-at-zero parameter `xi0` in a single wrapper object.

4. **predict.gamlssZadj()**
   - https://rdrr.io/cran/gamlss.inf/man/predict.gamlssZadj.html
   - Supports predictions for `mu`, `sigma`, `nu`, `tau`, and `xi0`.

5. **marginaleffects predictions**
   - https://marginaleffects.com/man/r/predictions.html
   - Documents unit-level and average predictions, delta-method standard errors, and control over prediction grids.
   - Package NEWS documents support for `gamlss` and a later fix involving the `what` argument.

## Design choice in gamlssPosthoc

The core package does not depend on undocumented internals of `emmeans`. It routes only the documented simple GAMLSS cases to `emmeans`. Derived estimands and smoothers are handled through the model's prediction methods, with optional refit bootstrap for uncertainty.

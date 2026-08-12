# Graphics mathematical validation

Overall: **True**

| Test | Target | Obtained | Abs. error | Tolerance | Pass |
|---|---:|---:|---:|---:|:---:|
| marginal mixture quantile p=.8 | 3.5738542 | 3.5718776 | 0.00198 | 0.025 | YES |
| quantile fan is ordered | 1 | 1 | 0 | 0 | YES |
| zero-adjusted quantile p=.7 | 2.266692 | 2.2705609 | 0.00387 | 0.025 | YES |
| standardized predictive density integrates to one | 1 | 0.99999999 | 5.3e-09 | 0.0002 | YES |
| cdf plus survival | 1 | 1 | 0 | 1e-12 | YES |
| ratio orientation A over B | 1.5 | 1.5 | 2.22e-16 | 1e-12 | YES |
| percent change orientation | 50 | 50 | 2.13e-14 | 1e-12 | YES |
| first derivative grid max error | 0 | 2.0428104e-14 | 2.04e-14 | 1e-10 | YES |
| local optimum location | 2.5 | 2.5 | 5.33e-15 | 1e-10 | YES |
| local optimum response | 7.5 | 7.5 | 4.44e-15 | 1e-10 | YES |
| PIT mean under calibrated normal residuals | 0.5 | 0.49983183 | 0.000168 | 0.002 | YES |
| PIT variance under calibrated normal residuals | 0.083333333 | 0.083285776 | 4.76e-05 | 0.001 | YES |

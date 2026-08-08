# Mathematical validation of gamlssPosthoc 0.2.0

Independent numerical checks of formulas and algorithms used by the refactored code.

| test                                 |    theory |    empirical |   abs_error |   threshold | pass   |
|:-------------------------------------|----------:|-------------:|------------:|------------:|:-------|
| ZA Gamma mean                        |  2.016    |  2.0189      | 0.00290409  |       0.01  | True   |
| ZA Gamma variance                    |  3.11144  |  3.11602     | 0.00457612  |       0.03  | True   |
| ZA quantile p=0.2                    |  0        |  0           | 0           |       0.03  | True   |
| ZA quantile p=0.5                    |  2.30122  |  2.30334     | 0.00211709  |       0.03  | True   |
| ZA quantile p=0.9                    |  4.25536  |  4.25874     | 0.00337757  |       0.03  | True   |
| ZA Lognormal mean                    |  1.82721  |  1.82701     | 0.000200883 |       0.01  | True   |
| ZA Lognormal variance                |  2.45367  |  2.46362     | 0.00995241  |       0.03  | True   |
| ZA Lognormal quantile p=0.1          |  0        |  0           | 0           |       0.03  | True   |
| ZA Lognormal quantile p=0.5          |  1.64936  |  1.64768     | 0.00167113  |       0.03  | True   |
| ZA Lognormal quantile p=0.95         |  4.64535  |  4.65392     | 0.00857859  |       0.03  | True   |
| contrast difference                  |  1.5      |  1.5         | 0           |       1e-12 | True   |
| contrast ratio                       |  1.5      |  1.5         | 0           |       1e-12 | True   |
| contrast log_ratio                   |  0.405465 |  0.405465    | 0           |       1e-12 | True   |
| contrast percent_change              | 50        | 50           | 0           |       1e-12 | True   |
| first derivative max interior error  |  0        |  3.28626e-14 | 3.28626e-14 |       1e-08 | True   |
| second derivative max interior error |  0        |  1.42109e-12 | 1.42109e-12 |       1e-08 | True   |
| turning point quadratic              |  2.125    |  2.125       | 2.79776e-14 |       0.02  | True   |
| local quadratic optimum x            |  2.13     |  2.13        | 4.44089e-16 |       1e-10 | True   |
| local quadratic optimum y            |  5        |  5           | 4.44089e-15 |       1e-10 | True   |

Overall: **PASS** (19/19 checks).
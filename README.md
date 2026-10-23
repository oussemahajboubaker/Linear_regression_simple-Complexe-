# Linear Regression: Simple & Multiple (Theory + R)

Study notes and hands-on R code on **linear regression**, from the formulas to the implementation.
The goal is to show that `lm()` is not a black box: every result is first computed by hand (sums, matrices) and then checked against R's built-in functions.

## Repository contents

| File | Type | Description |
|---|---|---|
| `Linear_Regression_Part1.docx` | Notes | **Theory.** Simple and multiple linear regression, ordinary least squares (OLS), residuals, error metrics (RSS, MSE, RMSE, MAE), model assumptions, and a worked example. |
| `Linear_Regression_Part2.docx` | Notes | **From formulas to R code.** Each line of R code is linked to its formula. Includes a simulation (10,000 samples) showing that the OLS estimator is unbiased, and a real-data analysis on `mtcars` (variable selection with `stepAIC`, multicollinearity with VIF). |
| `Regression_model_exp.R` | R script | **Worked example on `mtcars`.** OLS by hand for `mpg ~ wt` (means, slope, intercept), fitted values, residuals, RSS/MSE/RMSE/MAE, matrix form `(X'X)^-1 X'y`, multiple regression `mpg ~ wt + hp`, prediction, residual plot, and exploratory analysis. |
| `Boston_Regression_Model.R` | R script | **Full exercise with solution on the `Boston` housing data** (`MASS` package), organised in 9 parts and 31 questions (see below). |

## Topics covered

- Simple linear regression: slope and intercept from the formulas
- Residuals and error metrics: RSS, MSE, RMSE, MAE
- Matrix form of OLS: `beta = (X'X)^-1 X'y`, variance of the estimator, standard errors
- Hat matrix and leverage
- Multiple linear regression: reading `summary(lm)`, R², adjusted R², residual standard error
- Variable selection with AIC (`stepAIC`)
- Multicollinearity: VIF, computed by hand and with `car::vif`
- Model assumptions and diagnostics: linearity, normality, constant variance, influential points (Cook's distance)
- Prediction quality: train/test split, test RMSE and MAE
- Simulation: checking unbiasedness and the variance formula of the OLS estimator
- Improving a model: quadratic and log transformations

## The Boston exercise at a glance

Target: `medv`, the median value of homes (in thousands of dollars), for 506 Boston neighbourhoods.
The `black` variable is removed from the data (ethically problematic).

| Part | Content | Questions |
|---|---|---|
| 1 | Exploration: structure, normality, correlations | Q1 to Q5 |
| 2 | Simple regression by hand (`medv ~ lstat`) | Q6 to Q10 |
| 3 | Matrix computation and hat matrix | Q11 to Q15 |
| 4 | Full multiple regression, TSS, RSS, R² | Q16 to Q18 |
| 5 | Variable selection with `stepAIC` | Q19 to Q21 |
| 6 | Multicollinearity (VIF) | Q22, Q23 |
| 7 | Diagnostics and assumptions | Q24 to Q26 |
| 8 | Out-of-sample prediction (train/test) | Q27 to Q29 |
| 9 | Bonus: simulation and model improvement | Q30, Q31 |

Main findings (each one is reproduced in the script comments):

- A simple model `medv ~ lstat` gives an RMSE of about 6.2 k$, and the residual plot shows the relationship is **not linear**.
- The full model reaches R² = 0.734. `stepAIC` removes `indus` and `age`, giving a simpler model with a slightly better adjusted R² and AIC.
- VIF values stay below 10 (`tax` is the highest, about 9).
- Normality and constant-variance tests are rejected, so classical standard errors should be interpreted with caution.
- On the test set, transforming `lstat` (square or log) lowers the prediction error compared with the plain linear model.

## How to run the code

1. Install [R](https://www.r-project.org/) (and optionally RStudio).
2. Install the required packages:

   ```r
   install.packages(c("MASS", "car"))
   ```

3. Clone the repository and open a script:

   ```bash
   git clone https://github.com/oussemahajboubaker/Linear_regression_simple-Complexe-.git
   ```

4. Run `Regression_model_exp.R` first (it only needs base R), then `Boston_Regression_Model.R`.

Both datasets are built in (`mtcars` in base R, `Boston` in `MASS`), so no external files are needed.
Random splits and simulations use `set.seed()`, so the numbers in the comments can be reproduced.

## Suggested reading order

1. `Linear_Regression_Part1.docx` for the theory.
2. `Linear_Regression_Part2.docx` for the link between formulas and R code.
3. `Regression_model_exp.R` to practise on a small data set.
4. `Boston_Regression_Model.R` for a complete exercise: try the questions yourself before reading the solution.

## Author

Oussema Haj Boubaker, [GitHub profile](https://github.com/oussemahajboubaker)

Feedback and suggestions are welcome: open an issue or send a pull request.

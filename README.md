# HomeValue — Housing Price Statistical Analysis (R Shiny)

## Included files
- `app.R`: Complete Shiny application.
- `Housing.csv`: Housing dataset supplied for this project.

## Requirements
R (recent version) and RStudio are recommended.

## Install packages (run once in the RStudio Console)
```r
install.packages(c("shiny", "bslib", "ggplot2", "dplyr", "DT", "broom", "scales"))
```

## Run the app
1. Extract this ZIP file.
2. Open the extracted `HomeValue_Shiny_App` folder in RStudio.
3. Open `app.R`.
4. Click **Run App**, or run:
```r
shiny::runApp()
```
Make sure `app.R` and `Housing.csv` stay in the same folder.

## Analyses included
- Dataset explorer and CSV download
- Descriptive statistics and histograms/boxplots
- Correlation and covariance matrices
- Simple linear regression with coefficient table and model summary
- Multiple linear regression with numeric and categorical predictors
- Optional log(price) regression
- Actual-vs-predicted plot and residual diagnostics
- Chi-square test of independence for categorical variables
- Downloadable multiple-regression coefficient table

## Statistical cautions
- Chi-square is for categorical variables; inspect expected counts before interpreting the test.
- Regression shows associations and predictions, not proof of causation.
- Review model assumptions and influential observations before drawing conclusions.
- If `log(price)` is selected, coefficients are on the log scale.

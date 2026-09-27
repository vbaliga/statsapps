# Launch the power and precision app

Opens an interactive Shiny app for exploring how sample size,
explanatory variables, interactions, and repeated measurements affect
statistical power and confidence intervals for a known, fictional
caffeine effect.

## Usage

``` r
run_power_precision_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). By
  default, statsapps opens apps in the system default web browser using
  `launch.browser = TRUE`. Use `launch.browser = FALSE` to disable this
  behavior.

## Value

`run_power_precision_app()` opens the Shiny app locally.

## Details

All data are simulated. Sample size ranges from 12 to 180 people in
steps of 6, with 42 people by default. Equal numbers are assigned to
each combination of drink and age group. The true caffeine-minus-decaf
effect is fixed at -25 milliseconds.

Optional repeated measurements provide 1 to 5 observations per person.
When person identity is accounted for, a random-intercept model is
fitted by restricted maximum likelihood (REML). Predictors are constant
within each person. Mixed-model confidence intervals and power are
approximate. This is not a general-purpose mixed-model fitter.

Power is calculated from the known simulation parameters, not from
estimates fitted to the displayed outcomes. Confidence intervals
describe the caffeine-minus-decaf effect estimated from the displayed
sample. When the drink-by-sleep interaction is included, this contrast
is evaluated at 7 hours of sleep.

## Examples

``` r
if (FALSE) { # interactive()
run_power_precision_app()
}
```

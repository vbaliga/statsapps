# Launch the simple linear regression app

Opens an interactive Shiny app for exploring how the slope and intercept
of a linear model affect fitted values and residuals.

## Usage

``` r
run_linear_reg_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). By
  default, statsapps opens apps in the system default web browser using
  `launch.browser = TRUE`. Use `launch.browser = FALSE` to disable this
  behavior.

## Value

`run_linear_reg_app()` opens the Shiny app locally.

## Details

All data were simulated and don't come from a specific study.

## Examples

``` r
if (FALSE) { # interactive()
run_linear_reg_app()
}
```

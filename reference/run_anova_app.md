# Launch the one-way ANOVA simulator

Opens an interactive Shiny app for exploring how group means,
within-group variation, and sample size affect a one-way ANOVA.

## Usage

``` r
run_anova_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), such
  as `launch.browser = TRUE`. Most users can ignore this.

## Value

`run_anova_app()` opens the Shiny app locally.

## Details

All data are simulated in R via
[`rnorm()`](https://rdrr.io/r/stats/Normal.html).

## Examples

``` r
if (FALSE) { # interactive()
run_anova_app()
}
```

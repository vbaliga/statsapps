# Launch the probability distributions app

Opens an interactive Shiny app for exploring common probability
distributions used in BIOL 300.

## Usage

``` r
run_distributions_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), such
  as `launch.browser = TRUE`. Most users can ignore this.

## Value

`run_distributions_app()` opens the Shiny app locally.

## Details

All data were simulated and don't come from a specific study.

## Examples

``` r
if (FALSE) { # interactive()
run_distributions_app()
}
```

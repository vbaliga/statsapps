# Launch the sums of squares app

Opens an interactive Shiny app for exploring how total variation can be
partitioned into group-level and within-group components in one-way
ANOVA.

## Usage

``` r
run_sums_squares_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), such
  as `launch.browser = TRUE`. Most users can ignore this.

## Value

`run_sums_squares_app()` opens the Shiny app locally.

## Examples

``` r
if (FALSE) { # interactive()
run_sums_squares_app()
}
```

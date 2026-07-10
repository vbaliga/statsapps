# Launch the permutation test app

`run_permutation_app()` opens an interactive Shiny app that demonstrates
how repeated random reassignment of observations can be used to build a
null distribution for a two-sample permutation test.

## Usage

``` r
run_permutation_app(...)
```

## Arguments

- ...:

  Optional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html), such
  as `launch.browser = TRUE`. Most users can ignore this.

## Value

`run_permutation_app()` opens the Shiny app locally.

## Details

The app uses data on time to mating in female sagebrush crickets from
Johnson et al. (1999), as presented in *The Analysis of Biological Data*
by Whitlock and Schluter.

## Examples

``` r
if (FALSE) { # interactive()
run_permutation_app()
}
```

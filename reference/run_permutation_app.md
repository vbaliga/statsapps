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
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html). By
  default, statsapps opens apps in the system default web browser using
  `launch.browser = TRUE`. Use `launch.browser = FALSE` to disable this
  behavior.

## Value

`run_permutation_app()` opens the Shiny app locally.

## Details

The app uses data on time to mating in female sagebrush crickets from
Johnson et al. (1999), as presented in *The Analysis of Biological Data*
by Whitlock and Schluter.

## References

Johnson, J. C., T. M. Ivy, and S. K. Sakaluk. 1999. Female remating
propensity contingent on sexual cannibalism in sagebrush crickets,
*Cyphoderris strepitans*: a mechanism of cryptic female choice.
*Behavioral Ecology* 10: 227-233.

Whitlock, M. C., and D. Schluter. 2020. *The Analysis of Biological
Data*. 3rd ed. W. H. Freeman and Company.

## Examples

``` r
if (FALSE) { # interactive()
run_permutation_app()
}
```

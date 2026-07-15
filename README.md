
# statsapps

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!-- badges: end -->

The `statsapps` R package features interactive `Shiny` apps to help you
learn concepts from data science & statistics.

The apps run locally and use visualization, simulation, and repeated
sampling to illustrate topics such as probability distributions,
permutation tests, analysis of variance, linear regression, and sums of
squares.

## Installation

You can install the development version of `statsapps` from [this GitHub
repo](https://github.com/vbaliga/statsapps) with:

``` r
# install.packages("pak")
pak::pak("vbaliga/statsapps")
```

## Example

All apps can be started via functions from `statsapps`. Each app can be
started using a function that begins with `run_`. For example, use
`run_permutation_app()` to start the Shiny app for the Permutation Test.
This will open a new window in your default web browser with the Shiny
app.

``` r
library(statsapps)
run_permutation_app()
```

The example we showcase here is the Permutation Test app, demonstrates
how repeated random reassignment of observations can be used to build a
null distribution for a two-sample permutation test. As with all
`statsapps`, this app is interactive and allows you to see the results
of individual permutations, and how the result of each permutation
incrementally builds the null distribution.

There are several tabs within the main window of the app. The first two
tabs provide an explanation and the original data. The third tab shows
the results of performing permutations. The buttons in the left sidebar
allow you to perform permutations and visualize the outcome of the most
recent permutation.

<p align="center">

<img src="man/figures/perm_app_02.png" width="700"/>

</p>

<hr>

## Contributing and/or raising Issues

Feedback on bugs, improvements, and/or feature requests are all welcome.
Please see the Issues section on GitHub to make a bug fix request or
feature request.

## Citation

TBD

## License

See LICENSE file (MIT)

🐢

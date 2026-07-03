
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statsapps

<!-- badges: start -->

<!-- badges: end -->

The `statsapps` package features interactive `Shiny` apps to help you
learn concepts from data science & statistics.

## Installation

You can install the development version of statsapps from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("vbaliga/statsapps")
```

## Example

Use `run_permutation_app()` to start the Shiny app.

``` r
library(statsapps)
run_permutation_app()
```

This will open a new window with the Shiny app. It is recommended to hit
the “Open in Browser” button at the top to get the best view of the app.

<img src="man/figures/perm_app_01.png" width="600" />

<br>

The first two tabs provide an explanation and the original data. The
third tab shows the results of performing permutations.

![](man/figures/perm_app_02.png)

<hr>

# Contributing and/or raising Issues

Feedback on bugs, improvements, and/or feature requests are all welcome.
Please see the Issues templates on GitHub to make a bug fix request or
feature request.

To contribute code via a pull request, please consult the Contributing
Guide first.

# Citation

TBD

# License

See LICENSE file (MIT)

🐢

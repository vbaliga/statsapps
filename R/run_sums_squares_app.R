#' Launch the sums of squares app
#'
#' Opens an interactive Shiny app for exploring how total variation can be
#' partitioned into group-level and within-group components in one-way ANOVA.
#'
#' @inheritParams run_anova_app
#'
#' @return `run_sums_squares_app()` opens the Shiny app locally.
#'
#' @export
#'
#' @examplesIf interactive()
#' run_sums_squares_app()
run_sums_squares_app <- function(...) {
  run_statsapps_app("sums_squares", ...)
}

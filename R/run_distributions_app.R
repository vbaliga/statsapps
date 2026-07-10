#' Launch the probability distributions app
#'
#' Opens an interactive Shiny app for exploring common probability distributions
#' used in BIOL 300.
#'
#' All data were simulated and don't come from a specific study.
#'
#' @inheritParams run_anova_app
#'
#' @return `run_distributions_app()` opens the Shiny app locally.
#'
#' @export
#'
#' @examplesIf interactive()
#' run_distributions_app()
run_distributions_app <- function(...) {
  run_statsapps_app("distributions", ...)
}

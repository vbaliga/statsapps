#' Launch the power and precision app
#'
#' Opens an interactive Shiny app for exploring how additional explanatory
#' variables, interactions, and repeated observations affect statistical power
#' and confidence intervals for a prespecified drink effect.
#'
#' The number of independent people is fixed. All observations are simulated.
#' The repeated-measures analysis uses exact person-mean inference for a
#' balanced random-intercept design with predictors constant within people.
#' It is not a general-purpose mixed-model fitter.
#'
#' @inheritParams run_anova_app
#'
#' @return `run_power_precision_app()` opens the Shiny app locally.
#'
#' @export
#'
#' @examplesIf interactive()
#' run_power_precision_app()
run_power_precision_app <- function(...) {
  run_statsapps_app("power_precision", ...)
}

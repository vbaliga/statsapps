#' Launch the simple linear regression app
#'
#' Opens an interactive Shiny app for exploring how the slope and intercept of a
#' linear model affect fitted values and residuals.
#'
#' All data were simulated and don't come from a specific study.
#'
#' @inheritParams run_anova_app
#'
#' @return `run_linear_reg_app()` opens the Shiny app locally.
#'
#' @importFrom ggplot2 aes after_stat element_text geom_histogram geom_hline geom_line geom_point geom_segment ggplot labs stat_function theme theme_classic
#'
#' @export
#'
#' @examplesIf interactive()
#' run_linear_reg_app()
run_linear_reg_app <- function(...) {
  run_statsapps_app("linear_reg", ...)
}

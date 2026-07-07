#' Launch the simple linear regression app
#'
#' Opens an interactive Shiny app for exploring how the slope and intercept of a
#' linear model affect fitted values and residuals.
#'
#' All data were simulated and don't come from a specific study.
#'
#' @param ... Optional arguments passed to [shiny::runApp()]. Most users can
#'   ignore this.
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
  app_dir <- system.file("apps", "linear_reg", package = "statsapps")

  if (identical(app_dir, "")) {
    stop(
      "Could not find the linear regression app. Try reinstalling statsapps.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}

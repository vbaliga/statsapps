#' Launch the probability distributions app
#'
#' Opens an interactive Shiny app for exploring common probability distributions
#' used in BIOL 300.
#'
#' All data were simulated and don't come from a specific study.
#'
#' @param ... Optional arguments passed to [shiny::runApp()]. Most users can
#'   ignore this.
#'
#' @return `run_distributions_app()` opens the Shiny app locally.
#'
#' @export
#'
#' @examplesIf interactive()
#' run_distributions_app()
run_distributions_app <- function(...) {
  app_dir <- system.file("apps", "distributions", package = "statsapps")

  if (identical(app_dir, "")) {
    stop(
      "Could not find the distributions app. Try reinstalling statsapps.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}

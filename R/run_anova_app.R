#' Launch the one-way ANOVA simulator
#'
#' Opens an interactive Shiny app for exploring how group means, within-group
#' variation, and sample size affect a one-way ANOVA.
#'
#' All data are simulated in R via `rnorm()`.
#'
#' @param ... Optional arguments passed to [shiny::runApp()]. Most users can
#'   ignore this.
#'
#' @return `run_anova_app()` opens the Shiny app locally.
#'
#' @importFrom ggplot2 aes geom_boxplot geom_jitter ggplot labs scale_color_manual scale_fill_manual theme_classic ylim
#'
#' @export
#'
#' @examplesIf interactive()
#' run_anova_app()
run_anova_app <- function(...) {
  app_dir <- system.file("apps", "ANOVA", package = "statsapps")

  if (identical(app_dir, "")) {
    stop(
      "Could not find the ANOVA app. Try reinstalling statsapps.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}

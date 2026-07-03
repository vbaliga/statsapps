#' Launch the permutation test app
#'
#' `run_permutation_app()` opens an interactive Shiny app that demonstrates how
#' repeated random reassignment of observations can be used to build a null
#' distribution for a two-sample permutation test.
#'
#' The app uses data on time to mating in female sagebrush crickets from
#' Johnson et al. (1999), as presented in *The Analysis of Biological Data* by
#' Whitlock and Schluter.
#'
#' @param ... Optional arguments passed to [shiny::runApp()], such as
#'   `launch.browser = TRUE`.
#'
#' @return `run_permutation_app()` opens the Shiny app locally.
#'
#' @importFrom dplyr arrange bind_rows filter if_else mutate pull row_number summarise
#' @importFrom ggplot2 aes annotate element_blank element_line element_text expansion facet_wrap geom_histogram geom_vline ggplot labs margin scale_x_continuous scale_y_continuous theme theme_classic vars
#' @importFrom purrr map
#' @importFrom tibble tibble
#'
#' @export
#'
#' @examplesIf interactive()
#' run_permutation_app()
run_permutation_app <- function(...) {
  app_dir <- system.file("apps", "permutation", package = "statsapps")

  if (identical(app_dir, "")) {
    stop(
      "Could not find the permutation app. Try reinstalling statsapps.",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, ...)
}

run_statsapps_app <- function(
    app_name,
    launch.browser = getOption("statsapps.launch.browser", TRUE),
    ...
) {
  app_dir <- system.file("apps", app_name, package = "statsapps")

  if (identical(app_dir, "")) {
    stop(
      "Could not find the ",
      app_name,
      " app. Try reinstalling statsapps.",
      call. = FALSE
    )
  }

  shiny::runApp(
    app_dir,
    launch.browser = launch.browser,
    ...
  )
}

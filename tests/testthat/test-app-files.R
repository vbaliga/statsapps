test_that("all app.R files parse", {
  app_names <- c(
    "ANOVA",
    "distributions",
    "linear_reg",
    "permutation"
  )

  app_dirs <- system.file("apps", app_names, package = "statsapps")
  app_files <- file.path(app_dirs, "app.R")

  for (app_file in app_files) {
    expect_no_error(parse(app_file))
  }
})

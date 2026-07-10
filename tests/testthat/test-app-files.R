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

test_that("shared app files are bundled and parse", {
  shared_files <- c(
    system.file("app_shared", "app_settings.R", package = "statsapps"),
    system.file("app_shared", "statsapps.css", package = "statsapps")
  )

  expect_true(all(nzchar(shared_files)))
  expect_true(all(file.exists(shared_files)))

  expect_no_error(parse(shared_files[[1]]))
})

test_that("shared app settings define expected plotting helpers", {
  settings_file <- system.file(
    "app_shared",
    "app_settings.R",
    package = "statsapps"
  )

  settings_env <- new.env(parent = baseenv())
  source(settings_file, local = settings_env)

  expect_equal(settings_env$statsapps_plot_base_size, 16)
  expect_s3_class(settings_env$statsapps_plot_theme(), "theme")
  expect_equal(
    settings_env$statsapps_plot_theme_code(),
    "theme_classic(base_size = 16)"
  )
})

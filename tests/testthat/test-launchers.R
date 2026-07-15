test_that("app directories exist", {
  app_names <- c(
    "ANOVA",
    "distributions",
    "linear_reg",
    "permutation",
    "sums_squares"
  )

  app_dirs <- system.file("apps", app_names, package = "statsapps")

  expect_true(all(nzchar(app_dirs)))
  expect_true(all(file.exists(app_dirs)))
  expect_true(all(file.exists(file.path(app_dirs, "app.R"))))
})

test_that("launcher functions point to the correct apps", {
  captured <- new.env(parent = emptyenv())
  captured$calls <- character(0)

  local_mocked_bindings(
    run_statsapps_app = function(app_name, ...) {
      captured$calls <- c(captured$calls, app_name)
      invisible(app_name)
    }
  )

  expect_invisible(run_anova_app())
  expect_invisible(run_distributions_app())
  expect_invisible(run_linear_reg_app())
  expect_invisible(run_permutation_app())
  expect_invisible(run_sums_squares_app())

  expect_identical(
    captured$calls,
    c("ANOVA", "distributions", "linear_reg", "permutation", "sums_squares")
  )
})

test_that("run_statsapps_app finds an app directory and calls shiny::runApp", {
  captured <- new.env(parent = emptyenv())
  captured$app_dir <- NULL
  captured$launch_browser <- NULL

  local_mocked_bindings(
    runApp = function(appDir, launch.browser = NULL, ...) {
      captured$app_dir <- appDir
      captured$launch_browser <- launch.browser
      invisible(appDir)
    },
    .package = "shiny"
  )

  expect_invisible(run_statsapps_app("ANOVA"))

  expect_true(nzchar(captured$app_dir))
  expect_true(dir.exists(captured$app_dir))
  expect_true(file.exists(file.path(captured$app_dir, "app.R")))
  expect_true(grepl("ANOVA$", captured$app_dir))
  expect_true(captured$launch_browser)
})

test_that("run_statsapps_app allows browser launch to be disabled", {
  captured <- new.env(parent = emptyenv())
  captured$launch_browser <- NULL

  local_mocked_bindings(
    runApp = function(appDir, launch.browser = NULL, ...) {
      captured$launch_browser <- launch.browser
      invisible(appDir)
    },
    .package = "shiny"
  )

  expect_invisible(run_statsapps_app("ANOVA", launch.browser = FALSE))

  expect_false(captured$launch_browser)
})

test_that("run_statsapps_app respects the statsapps launch browser option", {
  captured <- new.env(parent = emptyenv())
  captured$launch_browser <- NULL

  local_mocked_bindings(
    runApp = function(appDir, launch.browser = NULL, ...) {
      captured$launch_browser <- launch.browser
      invisible(appDir)
    },
    .package = "shiny"
  )

  old_option <- getOption("statsapps.launch.browser")
  on.exit(options(statsapps.launch.browser = old_option), add = TRUE)

  options(statsapps.launch.browser = FALSE)

  expect_invisible(run_statsapps_app("ANOVA"))

  expect_false(captured$launch_browser)
})

test_that("run_statsapps_app errors clearly when an app is missing", {
  expect_error(
    run_statsapps_app("not_an_app"),
    "Could not find the not_an_app app"
  )
})

test_that("launcher functions pass launch.browser through to run_statsapps_app", {
  captured <- new.env(parent = emptyenv())
  captured$args <- list()

  local_mocked_bindings(
    run_statsapps_app = function(app_name, ...) {
      captured$args[[app_name]] <- list(...)
      invisible(app_name)
    }
  )

  expect_invisible(run_anova_app(launch.browser = FALSE))
  expect_invisible(run_distributions_app(launch.browser = FALSE))
  expect_invisible(run_linear_reg_app(launch.browser = FALSE))
  expect_invisible(run_permutation_app(launch.browser = FALSE))
  expect_invisible(run_sums_squares_app(launch.browser = FALSE))

  expect_false(captured$args$ANOVA$launch.browser)
  expect_false(captured$args$distributions$launch.browser)
  expect_false(captured$args$linear_reg$launch.browser)
  expect_false(captured$args$permutation$launch.browser)
  expect_false(captured$args$sums_squares$launch.browser)
})

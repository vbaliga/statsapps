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

  local_mocked_bindings(
    runApp = function(appDir, ...) {
      captured$app_dir <- appDir
      invisible(appDir)
    },
    .package = "shiny"
  )

  expect_invisible(run_statsapps_app("ANOVA"))

  expect_true(nzchar(captured$app_dir))
  expect_true(dir.exists(captured$app_dir))
  expect_true(file.exists(file.path(captured$app_dir, "app.R")))
  expect_true(grepl("ANOVA$", captured$app_dir))
})

test_that("run_statsapps_app errors clearly when an app is missing", {
  expect_error(
    run_statsapps_app("not_an_app"),
    "Could not find the not_an_app app"
  )
})

source_app_env <- function(app_name) {
  app_file <- system.file("apps", app_name, "app.R", package = "statsapps")

  expect_true(nzchar(app_file))
  expect_true(file.exists(app_file))

  app_env <- new.env(parent = globalenv())
  expect_no_error(source(app_file, local = app_env))

  app_env
}

test_that("sums_squares simulation has the expected group structure", {
  app_env <- source_app_env("sums_squares")

  data <- app_env$simulate_sums_squares_data(seed = 20260710)

  expect_s3_class(data, "data.frame")
  expect_identical(nrow(data), 10L)
  expect_identical(levels(data$group), c("A", "B", "C"))
  expect_identical(unname(as.integer(table(data$group))), c(4L, 3L, 3L))
  expect_identical(data$id, seq_len(nrow(data)))

  expect_true(all(c("group", "y", "id", "x_group", "x") %in% names(data)))
  expect_true(all(is.finite(data$y)))
  expect_true(all(is.finite(data$x)))
})

test_that("sums_squares simulation is reproducible by seed", {
  app_env <- source_app_env("sums_squares")

  data_1 <- app_env$simulate_sums_squares_data(seed = 20260710)
  data_2 <- app_env$simulate_sums_squares_data(seed = 20260710)
  data_3 <- app_env$simulate_sums_squares_data(seed = 20260711)

  expect_identical(data_1, data_2)
  expect_false(isTRUE(all.equal(data_1$y, data_3$y)))
})

test_that("sums_squares preserves seed state on success and errors", {
  app_env <- source_app_env("sums_squares")

  local({
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      saved_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", saved_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)

    original_positions <- app_env$add_plot_positions
    for (seed_exists in c(TRUE, FALSE)) {
      for (fail in c(FALSE, TRUE)) {
        if (seed_exists) {
          set.seed(12345)
          before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }

        if (fail) {
          # Fail after random draws, not before the generator changes state.
          app_env$add_plot_positions <- function(data) {
            stop("test simulation error", call. = FALSE)
          }
          expect_error(
            app_env$simulate_sums_squares_data(20260710),
            "test simulation error",
            fixed = TRUE
          )
        } else {
          app_env$add_plot_positions <- original_positions
          expect_no_error(app_env$simulate_sums_squares_data(20260710))
        }

        expect_identical(
          exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
          seed_exists
        )
        if (seed_exists) {
          expect_identical(
            get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
            before
          )
        }
      }
    }
  })
})

test_that("sums_squares retains the established simulated values", {
  app_env <- source_app_env("sums_squares")

  local({
    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (had_seed) {
      saved_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
    on.exit({
      if (had_seed) {
        assign(".Random.seed", saved_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)

    for (seed in c(20260710, 20260711, 123)) {
      actual <- app_env$simulate_sums_squares_data(seed)
      set.seed(seed)
      expected <- c(
        stats::rnorm(4, mean = 8, sd = 1.5),
        stats::rnorm(3, mean = 12, sd = 1.8),
        stats::rnorm(3, mean = 18, sd = 1.6)
      )
      expect_identical(actual$y, expected)
    }
  })
})

test_that("sums_squares decomposition is internally consistent", {
  app_env <- source_app_env("sums_squares")

  data <- app_env$simulate_sums_squares_data(seed = 20260710)
  summary_object <- app_env$summarize_sums_squares(data)

  group_means <- ave(data$y, data$group, FUN = mean)
  grand_mean <- mean(data$y)

  expect_equal(
    summary_object$ss_total,
    sum((data$y - grand_mean)^2),
    tolerance = 1e-10
  )

  expect_equal(
    summary_object$ss_groups,
    sum((group_means - grand_mean)^2),
    tolerance = 1e-10
  )

  expect_equal(
    summary_object$ss_error,
    sum((data$y - group_means)^2),
    tolerance = 1e-10
  )

  expect_equal(
    summary_object$ss_total,
    summary_object$ss_groups + summary_object$ss_error,
    tolerance = 1e-10
  )
})

test_that("sums_squares square data are well formed", {
  app_env <- source_app_env("sums_squares")

  data <- app_env$simulate_sums_squares_data(seed = 20260710)
  summary_object <- app_env$summarize_sums_squares(data)
  square_data <- app_env$make_square_data(summary_object)

  expect_s3_class(square_data, "data.frame")
  expect_identical(nrow(square_data), 3L * nrow(data))
  expect_identical(levels(square_data$component), c("Total", "Groups", "Error"))

  expect_true(all(square_data$ymin <= square_data$ymax))
  expect_true(all(square_data$xmin <= square_data$xmax))
  expect_equal(
    square_data$abs_deviation,
    abs(square_data$deviation),
    tolerance = 1e-10
  )
})

test_that("sums_squares step helpers work for all defined steps", {
  app_env <- source_app_env("sums_squares")

  expect_identical(app_env$max_step, 6)

  expect_no_error(
    vapply(0:app_env$max_step, app_env$step_title, character(1))
  )

  expect_no_error(
    vapply(0:app_env$max_step, app_env$step_description, character(1))
  )
})

test_that("linear_reg simulation preserves the user's random seed", {
  app_env <- source_app_env("linear_reg")

  set.seed(12345)
  seed_before <- .Random.seed

  expect_no_error(app_env$simulate_linear_reg_data(seed = 20260713))

  seed_after <- .Random.seed

  expect_identical(seed_after, seed_before)
})

test_that("linear_reg simulated solution is recovered by lm", {
  app_env <- source_app_env("linear_reg")

  simulation <- app_env$simulate_linear_reg_data(seed = 20260713)
  data <- simulation$data

  x_model <- if (isTRUE(simulation$solution_log_x)) {
    log(data$x)
  } else {
    data$x
  }

  y_model <- if (isTRUE(simulation$solution_log_y)) {
    log(data$y)
  } else {
    data$y
  }

  fit <- stats::lm(y_model ~ x_model)

  expect_equal(
    unname(stats::coef(fit)[[1]]),
    simulation$solution_intercept,
    tolerance = 1e-10
  )

  expect_equal(
    unname(stats::coef(fit)[[2]]),
    simulation$solution_slope,
    tolerance = 1e-10
  )
})

test_that("linear_reg displayed simulation code reproduces the data", {
  app_env <- source_app_env("linear_reg")

  simulation <- app_env$simulate_linear_reg_data(seed = 20260713)
  code <- app_env$simulation_code_text(simulation)

  code_env <- new.env(parent = globalenv())

  expect_no_error(
    eval(parse(text = code), envir = code_env)
  )

  expect_equal(
    code_env$linreg_data,
    simulation$data,
    tolerance = 1e-10
  )
})

test_that("linear_reg relationship specs stay on the slider grid", {
  app_env <- source_app_env("linear_reg")

  intercept_values <- app_env$slider_values(
    app_env$intercept_range,
    app_env$intercept_step
  )

  slope_values <- app_env$slider_values(
    app_env$slope_range,
    app_env$slope_step
  )

  for (relationship_id in names(app_env$relationship_specs)) {
    spec <- app_env$relationship_specs[[relationship_id]]

    expect_true(all(spec$intercept_values %in% intercept_values))
    expect_true(all(spec$slope_values %in% slope_values))
  }
})

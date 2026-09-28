# Run against the installed app in R CMD check, or the local source tree.
candidates <- c(file.path("..", "..", "inst", "apps", "power_precision"),
                file.path("inst", "apps", "power_precision"),
                system.file("apps", "power_precision", package = "statsapps"))
candidates <- candidates[nzchar(candidates)]
power_app_path <- candidates[file.exists(file.path(candidates, "helpers.R"))][1]
stopifnot(length(power_app_path) == 1L, !is.na(power_app_path))
power_app_path <- normalizePath(power_app_path)
power_env <- new.env(parent = globalenv())
source(file.path(power_app_path, "helpers.R"), local = power_env)

power_all_specs <- function() {
  flags <- expand.grid(sleep = c(FALSE, TRUE), age = c(FALSE, TRUE),
                        coffee = c(FALSE, TRUE), drink_sleep = c(FALSE, TRUE),
                        age_coffee = c(FALSE, TRUE))
  specs <- lapply(seq_len(nrow(flags)), function(i) {
    do.call(power_env$pp_spec, as.list(flags[i, ]))
  })
  specs[!duplicated(vapply(specs, power_env$pp_spec_key, character(1)))]
}

power_load_app <- function() {
  app <- new.env(parent = globalenv())
  source(file.path(power_app_path, "app.R"), local = app)
  app
}

# Plot-layout checks need a graphics device for measurements, not a PDF file.
# Keep this scoped to each check; do not change the user's default device.
power_with_null_device <- function(code) {
  previous_device <- grDevices::dev.cur()
  grDevices::pdf(file = NULL)
  device <- grDevices::dev.cur()
  on.exit({
    if (device %in% grDevices::dev.list()) {
      grDevices::dev.off(device)
    }
    if (previous_device > 1L && previous_device %in% grDevices::dev.list()) {
      grDevices::dev.set(previous_device)
    }
  }, add = TRUE)
  force(code)
}

test_that("power app code and local CSS exist", {
  expect_true(file.exists(file.path(power_app_path, "power_precision.css")))
  expect_no_error(parse(file.path(power_app_path, "helpers.R")))
  expect_no_error(parse(file.path(power_app_path, "app.R")))
})

test_that("plot-layout checks create no files and restore devices after errors", {
  local({
    scratch <- tempfile("statsapps-graphics-")
    dir.create(scratch)
    old_wd <- setwd(scratch)
    on.exit({
      setwd(old_wd)
      unlink(scratch, recursive = TRUE)
    }, add = TRUE)

    before <- grDevices::dev.list()
    active <- grDevices::dev.cur()
    figure <- ggplot2::ggplot()
    grob <- power_with_null_device(ggplot2::ggplotGrob(figure))
    expect_s3_class(grob, "gtable")
    expect_identical(grDevices::dev.list(), before)
    expect_identical(grDevices::dev.cur(), active)

    expect_error(power_with_null_device({
      ggplot2::ggplotGrob(figure)
      stop("test plotting error")
    }), "test plotting error")
    expect_identical(grDevices::dev.list(), before)
    expect_identical(grDevices::dev.cur(), active)
    expect_length(list.files(scratch, all.files = TRUE, no.. = TRUE), 0L)
  })
})

test_that("simulation preserves the caller's RNG, including on errors", {
  set.seed(144)
  before <- .Random.seed
  a <- power_env$pp_make_study(1800)
  expect_identical(.Random.seed, before)
  expect_identical(a, power_env$pp_make_study(1800))
  expect_false(identical(a, power_env$pp_make_study(1801)))
  expect_error(power_env$pp_with_seed(30, stop("test error")), "test error")
  expect_identical(.Random.seed, before)
  data <- power_env$pp_sample_data(a, TRUE)
  spec <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  fit <- power_env$pp_fit_data(data, spec)
  power_env$pp_power(data, spec)
  power_env$pp_prediction_grid(data, spec, fit)
  expect_identical(.Random.seed, before)
})

test_that("simulation does not leave a seed when none existed", {
  had <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had) assign(".Random.seed", old, envir = .GlobalEnv)
  }, add = TRUE)
  if (had) rm(".Random.seed", envir = .GlobalEnv)
  expect_no_error(power_env$pp_make_study(2000))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

test_that("repeats are unequal and keep the first measurement unchanged", {
  study <- power_env$pp_make_study(1800)
  single <- power_env$pp_sample_data(study)
  repeated <- power_env$pp_sample_data(study, TRUE)
  expect_identical(nrow(single), 42L)
  expect_identical(as.integer(table(single$drink)), c(21L, 21L))
  expect_length(unique(repeated$person), 42L)
  expect_true(all(table(repeated$person) %in% 1:5))
  expect_gt(length(unique(as.integer(table(repeated$person)))), 1L)
  first <- repeated[repeated$measurement == 1L, ]
  expect_identical(first$reaction_time_ms, single$reaction_time_ms)
  expect_identical(first$hours_slept, single$hours_slept)
  expect_true(
    all(single$daily_coffee_cups >= 0 & single$daily_coffee_cups <= 4)
  )
})

test_that("formula names match measurement units and show the random intercept", {
  full <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  expect_identical(power_env$pp_formula_text(full, TRUE),
    "reaction_time_ms ~ drink * hours_slept + age_group * daily_coffee_cups + (1 | person)")
  capped <- power_env$pp_spec(sleep = TRUE, age = TRUE, coffee = TRUE,
    drink_sleep = TRUE, age_coffee = TRUE, max_interactions = 1)
  expect_identical(power_env$pp_formula_text(capped),
    "reaction_time_ms ~ drink * hours_slept + age_group + daily_coffee_cups")
  expect_length(power_env$pp_columns(full), 9L)
  expect_true(full$sleep)
  expect_true(full$age)
  expect_true(full$coffee)
  expect_false(capped$age_coffee)
  expect_false(
    grepl("mean_reaction", power_env$pp_formula_text(full, TRUE), fixed = TRUE)
  )
})

test_that("single-measurement estimates and CIs agree with ordinary lm", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(303))
  for (spec in power_all_specs()) {
    actual <- power_env$pp_fit_data(data, spec)
    reference <- stats::lm(power_env$pp_formula(spec), data = data)
    X <- power_env$pp_design(data, spec)
    contrast <- power_env$pp_contrast(X)
    columns <- colnames(X)
    beta <- stats::coef(reference)[columns]
    V <- stats::vcov(reference)[columns, columns, drop = FALSE]
    estimate <- as.numeric(crossprod(contrast, beta))
    se <- sqrt(as.numeric(crossprod(contrast, V %*% contrast)))
    critical <- stats::qt(0.975, stats::df.residual(reference))
    expect_equal(actual$estimate, estimate, tolerance = 1e-8)
    expect_equal(actual$se, se, tolerance = 1e-8)
    expect_equal(actual$lower, estimate - critical * se, tolerance = 1e-8)
    expect_equal(actual$upper, estimate + critical * se, tolerance = 1e-8)
    expect_false(actual$random)
  }
})

test_that("the focal contrast remains caffeine minus decaf at 7 hours", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(306))
  full <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  first <- data[1, , drop = FALSE]
  first$drink <- 0
  first$hours_slept <- 7
  other <- first
  other$drink <- 1
  X <- power_env$pp_design(data, full)
  difference <- as.vector(
    power_env$pp_design(other, full) - power_env$pp_design(first, full)
  )
  expect_equal(difference, unname(power_env$pp_contrast(X)))
})

test_that("grouped REML equals the full observation-level restricted likelihood", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(304), TRUE)
  spec <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  d <- power_env$pp_prepare_data(data)
  X <- power_env$pp_design(d$people, spec)
  Xlong <- X[d$group, , drop = FALSE]
  ZZ <- outer(d$group, d$group, "==") * 1
  for (theta in c(0, 0.3, 1.2, 3)) {
    V0 <- diag(length(d$y)) + expm1(theta) * ZZ
    V0X <- solve(V0, Xlong)
    information <- crossprod(Xlong, V0X)
    beta <- solve(information, crossprod(V0X, d$y))
    residual <- d$y - as.vector(Xlong %*% beta)
    df <- length(d$y) - ncol(X)
    sigma2 <- as.numeric(crossprod(residual, solve(V0, residual))) / df
    expected <- df * log(sigma2) +
      as.numeric(determinant(V0, logarithm = TRUE)$modulus) +
      as.numeric(determinant(information, logarithm = TRUE)$modulus)
    actual <- power_env$pp_profile_reml(
      theta,
      X,
      d$counts,
      d$means,
      d$ss_within,
      TRUE
    )
    expect_equal(actual$objective, expected, tolerance = 1e-8)
    expect_equal(unname(actual$beta), as.vector(beta), tolerance = 1e-8)
    expect_equal(
      unname(actual$vcov),
      unname(sigma2 * solve(information)),
      tolerance = 1e-8
    )
  }
})

test_that("unequal-repeat estimates and random effects agree with dense GLS", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(305), TRUE)
  spec <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  result <- power_env$pp_fit_data(data, spec)
  d <- power_env$pp_prepare_data(data)
  X <- power_env$pp_design(d$people, spec)
  Xlong <- X[d$group, , drop = FALSE]
  V <- diag(result$sigma2, length(d$y)) +
    result$tau2 * outer(d$group, d$group, "==")
  VinvX <- solve(V, Xlong)
  covariance <- solve(crossprod(Xlong, VinvX))
  beta <- covariance %*% crossprod(VinvX, d$y)
  expect_true(result$random)
  expect_true(result$valid)
  expect_equal(unname(result$beta), as.vector(beta), tolerance = 1e-7)
  expect_equal(unname(result$vcov), unname(covariance), tolerance = 1e-7)
  raw_offsets <- d$means - as.vector(X %*% result$beta)
  expect_true(all(abs(result$person_effects) <= abs(raw_offsets) + 1e-8))
  expect_length(result$residuals, nrow(data))
  expect_gt(result$df, 1)
  expect_lt(result$df, nrow(data) - result$rank)
  shuffled <- data[rev(seq_len(nrow(data))), ]
  again <- power_env$pp_fit_data(shuffled, spec)
  expect_equal(result$estimate, again$estimate, tolerance = 1e-6)
  expect_equal(result$se, again$se, tolerance = 1e-6)
})

test_that("balanced-interior Satterthwaite df recover the person-level df", {
  people <- power_env$pp_make_study(811)$people
  X <- power_env$pp_design(
    people,
    power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  )
  df <- power_env$pp_contrast_df(
    X,
    rep(3L, nrow(people)),
    625,
    400,
    power_env$pp_contrast(X)
  )
  expect_equal(df, nrow(people) - ncol(X), tolerance = 1e-7)
})

test_that("invalid independence is not presented as legitimate power", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(812), TRUE)
  spec <- power_env$pp_spec(sleep = TRUE)
  naive <- power_env$pp_fit_data(data, spec, FALSE)
  reference <- stats::lm(power_env$pp_formula(spec), data = data)
  expect_false(naive$valid)
  expect_equal(
    naive$estimate,
    unname(stats::coef(reference)["drink"]),
    tolerance = 1e-8
  )
  expect_equal(naive$se, unname(summary(reference)$coefficients["drink", "Std. Error"]),
                tolerance = 1e-8)
  result <- power_env$pp_power(data, spec, FALSE)
  expect_true(is.na(result$power))
  expect_false(result$valid)
})

test_that("power uses known simulation parameters, not the displayed outcomes", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(401), TRUE)
  changed <- data
  changed$reaction_time_ms <- data$reaction_time_ms * 10 + 100 * data$drink
  for (spec in power_all_specs()) {
    expect_identical(
      power_env$pp_power(data, spec),
      power_env$pp_power(changed, spec)
    )
    expect_equal(
      power_env$pp_power(data, spec, drink_effect = 0)$power,
      0.05,
      tolerance = 1e-10
    )
  }
  useful <- power_env$pp_power(data, power_env$pp_spec(sleep = TRUE))
  full <- power_env$pp_power(
    data,
    power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  )
  expect_gte(useful$power, 0)
  expect_lte(useful$power, 1)
  expect_lt(full$power, useful$power)
  a <- power_env$pp_fit_data(data, power_env$pp_spec(sleep = TRUE))
  b <- power_env$pp_fit_data(changed, power_env$pp_spec(sleep = TRUE))
  expect_equal(b$estimate, 10 * a$estimate + 100, tolerance = 1e-5)
  expect_equal(b$width, 10 * a$width, tolerance = 1e-5)
})

test_that("plot axes, facets and point sizes follow selected variables", {
  empty <- power_env$pp_plot_view(power_env$pp_spec())
  sleep <- power_env$pp_plot_view(power_env$pp_spec(sleep = TRUE))
  coffee <- power_env$pp_plot_view(power_env$pp_spec(coffee = TRUE))
  full_spec <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  full <- power_env$pp_plot_view(full_spec)
  expect_identical(empty$x, "drink")
  expect_identical(sleep$x, "hours_slept")
  expect_identical(coffee$x, "daily_coffee_cups")
  expect_true(full$facet)
  expect_identical(full$size, "daily_coffee_cups")
  data <- power_env$pp_sample_data(power_env$pp_make_study(901), TRUE)
  fit <- power_env$pp_fit_data(data, full_spec)
  lines <- power_env$pp_prediction_grid(data, full_spec, fit)
  expected <- as.vector(power_env$pp_design(lines, full_spec) %*% fit$beta)
  expect_equal(lines$fitted, expected)
  expect_length(unique(lines$age_group), 3L)
  expect_length(unique(lines$assigned_drink), 2L)
  points <- power_env$pp_plot_points(data, full_spec)
  expect_identical(points$plot_x, data$hours_slept)
})

test_that("invalid inputs fail explicitly while unbalanced counts are accepted", {
  data <- power_env$pp_sample_data(power_env$pp_make_study(308), TRUE)
  expect_no_error(power_env$pp_fit_data(data, power_env$pp_spec()))
  duplicate <- which(duplicated(data$person))[1]
  changed <- data
  changed$hours_slept[duplicate] <- changed$hours_slept[duplicate] + 1
  expect_error(power_env$pp_prepare_data(changed), "constant within")
  changed <- data
  changed$reaction_time_ms[1] <- NA_real_
  expect_error(power_env$pp_prepare_data(changed), "Missing")
  changed$reaction_time_ms[1] <- Inf
  expect_error(power_env$pp_prepare_data(changed), "finite")
})

test_that("the sidebar holds controls while the main area stays data-first", {
  app <- power_load_app()
  html <- as.character(app$ui)
  locate <- function(text) as.integer(regexpr(text, html, fixed = TRUE))
  sidebar_start <- locate('class="pp-sidebar"')
  main_start <- locate('class="pp-main"')
  footer_start <- locate('class="footer-note"')
  expect_gt(sidebar_start, 0L)
  expect_gt(main_start, sidebar_start)
  expect_gt(footer_start, main_start)
  sidebar <- substr(html, sidebar_start, main_start - 1L)
  main <- substr(html, main_start, footer_start - 1L)
  for (id in c("include_age", "include_coffee", "include_sleep", "interaction_one",
               "interaction_two", "repeated", "account_person", "n_people")) {
    marker <- paste0('id="', id, '"')
    expect_match(sidebar, marker, fixed = TRUE)
    expect_false(grepl(marker, main, fixed = TRUE))
  }
  for (id in c("study_note", "data_plot", "data_note", "new_sample", "model_formula",
               "model_note", "validity_warning", "power_plot", "precision_plot",
               "power_feedback", "precision_feedback")) {
    marker <- paste0('id="', id, '"')
    expect_match(main, marker, fixed = TRUE)
    expect_false(grepl(marker, sidebar, fixed = TRUE))
  }
  position <- function(id) locate(paste0('id="', id, '"'))
  expect_lt(position("data_plot"), position("model_formula"))
  expect_lt(position("model_formula"), position("power_plot"))
  expect_lt(position("model_formula"), position("precision_plot"))
  for (id in c("simulation_note", "sample_estimate", "nsim", "effect", "detail_tab")) {
    expect_identical(position(id), -1L)
  }
  expect_false(
    grepl("attempt|Usual coffee consumption|tagline|Simulation settings", html)
  )
  expect_match(sidebar, "Cups of coffee per day", fixed = TRUE)
})

test_that("Shiny preserves the sample, fixes baseline colors, and exposes actual CIs", {
  app <- power_load_app()
  shiny::testServer(app$server, {
    session$setInputs(include_sleep = FALSE, include_age = FALSE, include_coffee = FALSE,
      interaction_one = FALSE, interaction_two = FALSE,
      repeated = FALSE, account_person = TRUE, new_sample = 0)
    original <- long_data()
    expect_identical(nrow(comparison_data()), 1L)
    expect_identical(comparison_data()$role, "baseline")
    expect_identical(comparison_data()$lower, baseline_fit()$lower)
    expect_identical(data_view()$x, "drink")

    session$setInputs(
      include_sleep = TRUE,
      include_age = TRUE,
      include_coffee = TRUE
    )
    expect_identical(long_data(), original)
    expect_identical(comparison_data()$role, c("baseline", "current"))
    expect_identical(data_view()$x, "hours_slept")
    expect_true(data_view()$facet)
    expect_identical(data_view()$size, "daily_coffee_cups")
    expect_identical(comparison_data()$lower[2], current_fit()$lower)
    expect_identical(comparison_data()$upper[2], current_fit()$upper)
    expect_no_error(output$data_plot)
    expect_no_error(output$power_plot)
    expect_no_error(output$precision_plot)

    session$setInputs(
      repeated = TRUE,
      interaction_one = TRUE,
      interaction_two = TRUE
    )
    expect_true(all(table(long_data()$person) %in% 1:5))
    expect_true(current_fit()$random)
    expect_identical(current_fit()$n_people, 42L)
    expect_match(output$model_formula, "(1 | person)", fixed = TRUE)
    expect_false(grepl("mean_reaction|attempt", output$model_formula))
    expect_no_error(output$data_plot)
    expect_no_error(output$precision_plot)

    session$setInputs(account_person = FALSE)
    expect_true(is.na(comparison_data()$power[2]))
    expect_false(current_fit()$valid)
    expect_match(
      output$validity_warning$html,
      "invalid confidence interval",
      fixed = TRUE
    )
    expect_no_error(output$power_plot)
    session$setInputs(account_person = TRUE)

    before <- study()
    session$setInputs(new_sample = 1)
    expect_false(identical(study(), before))
    session$setInputs(include_sleep = FALSE)
    expect_false(selected_spec()$drink_sleep)
    expect_identical(data_view()$x, "daily_coffee_cups")
    session$setInputs(include_coffee = FALSE)
    expect_false(selected_spec()$age_coffee)
    expect_identical(data_view()$x, "drink")
  })
})

test_that("mixed-model intervals have reasonable seeded calibration", {
  # Slow check: run in local development and GitHub CI, not on CRAN.
  skip_on_cran()
  spec <- power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE)
  covered <- detected <- planned <- numeric(600)
  for (i in seq_along(covered)) {
    data <- power_env$pp_sample_data(
      power_env$pp_make_study(20262000L + i),
      TRUE
    )
    result <- power_env$pp_fit_data(data, spec)
    effect <- power_env$pp_defaults$drink_effect
    covered[i] <- result$lower <= effect && result$upper >= effect
    detected[i] <- result$lower > 0 || result$upper < 0
    planned[i] <- power_env$pp_power(data, spec)$power
  }
  expect_lt(abs(mean(covered) - 0.95), 0.04)
  expect_lt(abs(mean(detected) - mean(planned)), 0.08)
})

test_that("sample sizes are balanced and nested across the entire slider", {
  expect_identical(power_env$pp_defaults$min_people, 12L)
  expect_identical(power_env$pp_defaults$n_people, 42L)
  expect_identical(power_env$pp_defaults$max_people, 180L)
  expect_identical(power_env$pp_defaults$people_step, 6L)
  allowed <- seq(12L, 180L, by = 6L)
  expect_length(allowed, 29L)
  expect_true(all(allowed %% 6L == 0L))
  for (seed in c(20261016L, 811L, 1800L)) {
    pool <- power_env$pp_make_study(seed, 180L)
    for (n in allowed) {
      small <- power_env$pp_make_study(seed, n)
      expect_identical(small$people, pool$people[seq_len(n), , drop = FALSE])
      expect_identical(small$person_z, pool$person_z[seq_len(n)])
      expect_identical(
        small$measurement_z,
        pool$measurement_z[seq_len(n), , drop = FALSE]
      )
      expect_equal(as.integer(table(small$people$drink)), rep(n / 2, 2))
      expect_equal(as.integer(table(small$people$age_group)), rep(n / 3, 3))
      expect_equal(as.integer(table(small$people$age_group, small$people$drink)),
                   rep(n / 6, 6))
    }
  }
  for (n in c(13, 20, 40, 41, 43, 178, 42.5)) {
    expect_error(power_env$pp_make_study(123, n), "divisible by 6")
  }
  for (n in c(6, 10, 186, 200, NA_real_, Inf)) {
    expect_error(power_env$pp_make_study(123, n), "supported range")
  }
})

test_that("all permitted models work at both slider limits", {
  for (n in c(12L, 180L)) {
    study <- power_env$pp_make_study(20261016, n)
    for (repeated in c(FALSE, TRUE)) {
      data <- power_env$pp_sample_data(study, repeated)
      for (spec in power_all_specs()) {
        fit <- power_env$pp_fit_data(data, spec)
        planned <- power_env$pp_power(data, spec)
        expect_identical(fit$n_people, n)
        expect_true(
          all(is.finite(c(fit$estimate, fit$lower, fit$upper, fit$df)))
        )
        expect_gt(fit$width, 0)
        expect_gt(fit$df, 0)
        expect_gte(planned$power, 0)
        expect_lte(planned$power, 1)
        expect_no_error(power_env$pp_prediction_grid(data, spec, fit))
      }
    }
  }
})

test_that("status colours obey the power threshold and interval-width rules", {
  p <- power_env$pp_power_status
  expect_identical(
    p(0.81, 0.95),
    "strong"
  ) # Absolute 80% rule takes precedence.
  expect_identical(p(0.80, 0.5), "better")
  expect_identical(p(0.7, 0.5), "better")
  expect_identical(p(0.4, 0.5), "worse")
  expect_identical(p(0.5, 0.5), "same")
  expect_identical(p(NA_real_, 0.5), "invalid")
  expect_identical(p(0.95, 0.5, valid = FALSE), "invalid")
  w <- power_env$pp_precision_status
  expect_identical(w(30, 40), "strong")
  expect_identical(w(32, 40), "strong")
  expect_identical(w(38, 40), "better")
  expect_identical(w(50, 40), "worse")
  expect_identical(w(40, 40), "same")
  expect_identical(w(30, 40, valid = FALSE), "invalid")
  app <- power_load_app()
  expect_identical(unname(app$pp_drink_colors["Caffeinated coffee"]), "#8C510A")
  expect_false(any(app$pp_drink_colors == app$pp_plot_colors["baseline"]))
  expect_identical(unname(app$pp_plot_colors["baseline"]), "#9A9FA6")
  expect_false(
    identical(app$pp_plot_colors["strong"], app$pp_plot_colors["better"])
  )
})

test_that("feedback distinguishes low power, uncertainty and evidence of an effect", {
  p <- power_env$pp_power_feedback
  expect_match(p(0.7, 0.5), "higher.*below 80%")
  expect_match(p(0.7, 0.5), "30.0%", fixed = TRUE)
  expect_match(p(0.4, 0.5), "lower.*below 80%")
  expect_match(p(0.9, 0.5), "above 80%")
  expect_match(p(0.9, 0.95), "lower.*above 80%")
  expect_match(p(0.8, 0.5), "at 80%")
  expect_match(p(1, 0.5), "less than 0.1%", fixed = TRUE)
  expect_match(p(NA_real_, 0.5, valid = FALSE), "not independent", fixed = TRUE)
  w <- power_env$pp_precision_feedback
  expect_match(w(30, 40, -20, 10), "narrower.*more precise.*includes 0")
  expect_match(w(50, 40, -60, -10), "wider.*less precise.*excludes 0")
  expect_match(w(40, 40, -40, 0), "unchanged.*includes 0")
  expect_match(w(40, 40, 0, 40), "includes 0")
  expect_match(w(40, 40, 1, 41), "excludes 0")
  expect_match(w(10, 40, -30, -20, valid = FALSE), "Do not use", fixed = TRUE)
  # Do not recreate the long instructional panels removed in v02.
  for (text in c(p(0.7, 0.5), p(0.4, 0.5), p(0.9, 0.95),
                 w(30, 40, -20, 10), w(50, 40, -60, -10))) {
    expect_lte(length(strsplit(text, "\\s+")[[1]]), 34L)
  }
})

test_that("the stronger scenario changes generating parameters, not the power formula", {
  study <- power_env$pp_make_study(777, 42)
  data <- power_env$pp_sample_data(study)
  # Set the same sleep values within each drink group, eliminating imbalance.
  for (drink in 0:1) {
    data$hours_slept[data$drink == drink] <- seq(5.5, 8.5, length.out = 21)
  }
  baseline <- power_env$pp_power(data, power_env$pp_spec())
  sleep <- power_env$pp_power(data, power_env$pp_spec(sleep = TRUE))
  residual_sd <- sqrt(power_env$pp_defaults$person_sd^2 +
                      power_env$pp_defaults$measurement_sd^2)
  reference <- stats::power.t.test(n = 21, delta = 25,
    sd = sqrt(residual_sd^2 + (power_env$pp_defaults$sleep_effect *
                               power_env$pp_defaults$sleep_sd)^2),
    sig.level = 0.05, type = "two.sample", strict = TRUE)
  expect_equal(baseline$power, reference$power, tolerance = 1e-8)
  expect_lt(baseline$power, 0.6)
  expect_gt(sleep$power, 0.8)
  expect_gt(sleep$power - baseline$power, 0.3)
  expect_equal(sleep$se, residual_sd * sqrt(1 / 21 + 1 / 21), tolerance = 1e-8)
})

test_that("the slider and short feedback update without replacing existing people", {
  app <- power_load_app()
  html <- as.character(app$ui)
  expect_match(html, 'id="n_people"', fixed = TRUE)
  expect_match(html, 'data-min="12"', fixed = TRUE)
  expect_match(html, 'data-max="180"', fixed = TRUE)
  expect_match(html, 'data-from="42"', fixed = TRUE)
  expect_match(html, 'data-step="6"', fixed = TRUE)
  expect_match(html, 'class="pp-sample-size"', fixed = TRUE)
  expect_match(html, 'id="power_feedback"', fixed = TRUE)
  expect_match(html, 'id="precision_feedback"', fixed = TRUE)
  shiny::testServer(app$server, {
    session$setInputs(n_people = 42, include_sleep = FALSE, include_age = FALSE,
      include_coffee = FALSE, interaction_one = FALSE, interaction_two = FALSE,
      repeated = FALSE, account_person = TRUE, new_sample = 0)
    original <- long_data()
    expect_identical(comparison_data()$power_status, "baseline")
    expect_identical(comparison_data()$precision_status, "baseline")
    expect_match(output$allocation_note, "21 people per drink.", fixed = TRUE)
    expect_no_error(output$power_feedback)
    expect_no_error(output$precision_feedback)
    session$setInputs(n_people = 12)
    expect_identical(nrow(long_data()), 12L)
    expect_identical(as.integer(table(long_data()$drink)), c(6L, 6L))
    expect_identical(as.integer(table(long_data()$age_group)), c(4L, 4L, 4L))
    expect_identical(long_data(), original[seq_len(12), , drop = FALSE])
    expect_match(output$study_note, "12 people: 6 receive", fixed = TRUE)
    session$setInputs(n_people = 180)
    expect_identical(nrow(long_data()), 180L)
    expect_identical(long_data()[seq_len(42), , drop = FALSE], original)
    session$setInputs(n_people = 42)
    expect_identical(long_data(), original)
    session$setInputs(include_sleep = TRUE, include_age = TRUE, include_coffee = TRUE,
                      interaction_one = TRUE, interaction_two = TRUE)
    expect_identical(long_data(), original)
    comparison <- comparison_data()
    expect_identical(comparison$power_status[1], "baseline")
    expect_identical(comparison$precision_status[1], "baseline")
    # Keep app-level lookups explicit inside testServer.
    expect_identical(comparison$power_status[2],
      app$pp_power_status(current_power()$power, baseline_power()$power))
    expect_identical(comparison$precision_status[2],
      app$pp_precision_status(current_fit()$width, baseline_fit()$width))
    expect_no_error(output$data_plot)
    expect_no_error(output$power_plot)
    expect_no_error(output$precision_plot)
    for (n in c(12L, 180L)) {
      session$setInputs(n_people = n, repeated = TRUE)
      expect_length(unique(long_data()$person), n)
      expect_match(
        output$data_note,
        paste0("from ", n, " people"),
        fixed = TRUE
      )
      expect_true(current_fit()$valid)
      expect_no_error(output$data_plot)
      expect_no_error(output$power_feedback)
      expect_no_error(output$precision_feedback)
    }
    session$setInputs(account_person = FALSE)
    expect_identical(comparison_data()$power_status[2], "invalid")
    expect_identical(comparison_data()$precision_status[2], "invalid")
    expect_match(output$power_feedback, "not independent", fixed = TRUE)
    expect_match(output$precision_feedback, "Do not use", fixed = TRUE)
  })
})

test_that("the sidebar title, description and caution retain the requested wording", {
  app <- power_load_app()
  html <- as.character(app$ui)
  locate <- function(text) as.integer(regexpr(text, html, fixed = TRUE))
  title <- "Sample size, effect size and power"
  description <- paste("This demonstration shows how sample size and model choices affect",
                        "statistical power and precision for a known effect size.")
  question <- "Fictional example: Does caffeine affect reaction time?"
  expect_gt(locate(title), 0)
  expect_lt(locate('class="pp-sidebar"'), locate(title))
  expect_lt(locate(title), locate(description))
  expect_lt(locate(description), locate('class="pp-controls"'))
  expect_lt(locate('class="pp-controls"'), locate('class="pp-caution"'))
  expect_lt(locate('class="pp-caution"'), locate('class="pp-main"'))
  expect_lt(locate('class="pp-main"'), locate(question))
  expect_lt(locate(question), locate('id="data_plot"'))
  expect_match(html, paste0("<strong>",
    "A sample size that gives good power here does not guarantee good power for your data.",
    "</strong>"), fixed = TRUE)
  expect_match(
    html,
    "Do not use these sample sizes as recommendations for your study.",
    fixed = TRUE
  )
  expect_lt(locate('id="include_age"'), locate('id="include_coffee"'))
  expect_lt(locate('id="include_coffee"'), locate('id="include_sleep"'))
  expect_match(html, "Adjust sample size", fixed = TRUE)
})

test_that("the precision axis has equal positive and negative extents and ticks", {
  axis <- power_env$pp_precision_axis(-51, -12)
  expect_equal(axis$limits, c(-100, 100))
  expect_equal(axis$breaks, c(-100, -50, 0, 50, 100))
  expect_identical(axis$labels, c("-100", "-50", "0", "+50", "+100"))
  for (endpoints in list(c(-160, -20), c(-30, 260), c(-400, 300))) {
    axis <- power_env$pp_precision_axis(endpoints[1], endpoints[2])
    expect_equal(axis$limits[1], -axis$limits[2])
    expect_equal(axis$breaks, -rev(axis$breaks))
    expect_lte(axis$limits[1], min(endpoints))
    expect_gte(axis$limits[2], max(endpoints))
  }
  axis <- power_env$pp_precision_axis(-5, 5, true_effect = -350)
  expect_lte(axis$limits[1], -350)
  expect_gte(axis$limits[2], 350)
})

test_that("the CI reference line is labelled directly and cannot obscure its values", {
  app <- power_load_app()
  expect_false(
    app$pp_reference_color %in% c(app$pp_drink_colors, app$pp_plot_colors)
  )
  reference_rgb <- as.integer(grDevices::col2rgb(app$pp_reference_color))
  expect_gt(length(unique(reference_rgb)), 1L) # The reference is not grey.

  shiny::testServer(app$server, {
    session$setInputs(n_people = 42, include_sleep = FALSE, include_age = FALSE,
      include_coffee = FALSE, interaction_one = FALSE, interaction_two = FALSE,
      repeated = FALSE, account_person = TRUE, new_sample = 0)
    # Check the one-row baseline, selected models, both slider limits, and the
    # explicitly invalid analysis with person identity ignored.
    states <- list(
      list(n_people = 42, repeated = FALSE, selected = FALSE, account = TRUE),
      list(n_people = 42, repeated = FALSE, selected = TRUE, account = TRUE),
      list(n_people = 42, repeated = TRUE, selected = TRUE, account = TRUE),
      list(n_people = 12, repeated = TRUE, selected = TRUE, account = TRUE),
      list(n_people = 180, repeated = TRUE, selected = TRUE, account = TRUE),
      list(n_people = 42, repeated = TRUE, selected = TRUE, account = FALSE)
    )
    for (state in states) {
      session$setInputs(n_people = state$n_people, repeated = state$repeated,
        account_person = state$account, include_sleep = state$selected,
        include_age = state$selected, include_coffee = state$selected,
        interaction_one = state$selected, interaction_two = state$selected)
      figure <- precision_figure()
      built <- power_with_null_device(ggplot2::ggplot_build(figure))
      layer_index <- function(geom) which(vapply(figure$layers, function(layer) {
        inherits(layer$geom, geom)
      }, logical(1)))

      reference_index <- layer_index("GeomVline")
      expect_length(reference_index, 1L)
      reference <- built$data[[reference_index]]
      expect_equal(reference$xintercept, app$pp_defaults$drink_effect)
      expect_false(any(reference$xintercept == 0))
      expect_true(all(reference$colour == app$pp_reference_color))
      expect_true(all(reference$linetype == "dashed"))

      xscale <- figure$scales$get_scales("x")
      expect_equal(xscale$limits[1], -xscale$limits[2])
      expect_equal(xscale$breaks, -rev(xscale$breaks))
      expect_identical(figure$labels$subtitle,
        if (state$repeated) "95% CI (approx.)" else "95% CI")
      expect_identical(figure$theme$legend.position, "none")

      direct_index <- layer_index("GeomText")
      expect_length(direct_index, 1L)
      direct <- built$data[[direct_index]]
      expect_identical(direct$label,
        sprintf("True effect (%g ms)", app$pp_defaults$drink_effect))
      expect_identical(direct$colour, app$pp_reference_color)
      expect_gt(direct$x, app$pp_defaults$drink_effect)
      expect_lt(direct$x, xscale$limits[2])

      value_index <- layer_index("GeomLabel")
      expect_length(value_index, 1L)
      values <- built$data[[value_index]]
      comparison <- comparison_data()
      expect_identical(values$label, sprintf("%+.1f  [%+.1f, %+.1f]",
        comparison$estimate, comparison$lower, comparison$upper))
      expect_equal(values$x, (comparison$lower + comparison$upper) / 2)
      expect_equal(values$y, comparison$position + 0.25)
      expect_true(all(values$fill == "white"))
      expect_true(all(values$colour == "black"))
      expect_true(all(is.na(values$alpha) | values$alpha == 1))
      expect_gt(value_index, reference_index) # Opaque labels are drawn on top.
      expect_gt(
        direct$y,
        max(values$y)
      ) # The true-effect label has its own row.
      expect_lt(direct$y, figure$scales$get_scales("y")$limits[2])
      value_layer <- figure$layers[[value_index]]
      if ("border.colour" %in% names(formals(ggplot2::geom_label))) {
        expect_equal(value_layer$aes_params$linewidth, 0)
      } else {
        expect_equal(value_layer$geom_params$label.size, 0)
      }
      expect_no_error(power_with_null_device(ggplot2::ggplotGrob(figure)))
      expect_no_error(output$precision_plot)
    }
  })
})

test_that("the default design exposes a substantial cost of unnecessary interactions", {
  data <- power_env$pp_sample_data(power_env$pp_make_study())
  baseline <- power_env$pp_power(data, power_env$pp_spec())$power
  split <- power_env$pp_power(data, power_env$pp_spec(age_coffee = TRUE))$power
  useful <- power_env$pp_power(data, power_env$pp_spec(sleep = TRUE))$power
  full <- power_env$pp_power(data,
    power_env$pp_spec(drink_sleep = TRUE, age_coffee = TRUE))$power
  expect_lt(split, baseline)
  expect_gt(useful - baseline, 0.3)
  expect_gt(useful, 0.75)
  expect_lt(useful, 0.85)
  expect_gt(useful - full, 0.3)
  expect_lt(full, baseline)
  # The data and their RNG stream are not regenerated when a model changes.
  expect_identical(data, power_env$pp_sample_data(power_env$pp_make_study()))
})


test_that("every slider setting plots every point in the correct age and drink group", {
  app <- power_load_app()
  shiny::testServer(app$server, {
    session$setInputs(n_people = 42, include_sleep = TRUE, include_age = TRUE,
      include_coffee = TRUE, interaction_one = FALSE, interaction_two = FALSE,
      repeated = FALSE, account_person = TRUE, new_sample = 0)
    sample_sizes <- if (identical(Sys.getenv("NOT_CRAN"), "true")) {
      seq(12L, 180L, by = 6L)
    } else {
      c(12L, 42L, 180L)
    }

    for (n in sample_sizes) {
      for (with_repeats in c(FALSE, TRUE)) {
        session$setInputs(n_people = n, repeated = with_repeats)
        plotted <- plot_points()
        raw <- long_data()
        expect_identical(nrow(plotted), nrow(raw))
        expect_identical(plotted$person, raw$person)
        expect_identical(plotted$reaction_time_ms, raw$reaction_time_ms)
        people <- plotted[!duplicated(plotted$person), , drop = FALSE]
        expect_equal(
          as.integer(table(people$age_group, people$drink)),
          rep(n / 6, 6)
        )

        figure <- data_figure()
        point_index <- which(vapply(figure$layers, function(layer) {
          inherits(layer$geom, "GeomPoint")
        }, logical(1)))
        expect_length(point_index, 1L)
        built <- power_with_null_device(ggplot2::ggplot_build(figure))
        drawn <- built$data[[point_index]]
        expect_identical(nrow(drawn), nrow(raw))
        expect_false(anyNA(drawn[, c("x", "y", "colour", "PANEL")]))
        panels <- built$layout$layout
        for (i in seq_len(nrow(panels))) {
          age <- as.character(panels$age_group[i])
          panel <- panels$PANEL[i]
          for (drink_label in names(app$pp_drink_colors)) {
            expect_equal(sum(drawn$PANEL == panel &
                              drawn$colour == app$pp_drink_colors[[drink_label]]),
                         sum(raw$age_group == age & raw$assigned_drink == drink_label))
          }
        }
      }
    }
    # Also check the age-faceted categorical x-axis at the two limits.
    session$setInputs(
      repeated = FALSE,
      include_sleep = FALSE,
      include_coffee = FALSE
    )
    for (n in c(12L, 180L)) {
      session$setInputs(n_people = n)
      plotted <- plot_points()
      expect_true(all(plotted$plot_x > 0.5 & plotted$plot_x < 2.5))
      expect_identical(nrow(plotted), n)
      expect_no_error(
        power_with_null_device(ggplot2::ggplotGrob(data_figure()))
      )
    }
  })
})

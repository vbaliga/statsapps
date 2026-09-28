# Numerical helpers for the power / precision app. Base R and stats only.
# The mixed-model engine is deliberately restricted to one random intercept
# and predictors that are constant within a person. Counts may differ.
#
# REML: y = X beta + Z b + e, b ~ N(0, tau^2 I), e ~ N(0, sigma^2 I).
# We profile sigma^2 and optimize log(1 + tau^2 / sigma^2). Group sufficient
# statistics evaluate the FULL observation-level likelihood, including every
# within-person residual. This is not an unweighted regression of person means.
# Reference: Bates et al. (2015), doi:10.18637/jss.v067.i01.
#
# Mixed-model intervals use an expected-information Satterthwaite approximation.
# Power uses the known simulation effect/variances and the selected design, NOT
# the effect or residual variance fitted to the displayed outcomes. Noncentral
# t calculations are approximate for the mixed model. No simulation bank is run.

pp_defaults <- list(
  n_people = 42L, min_people = 12L, max_people = 180L, people_step = 6L,
  max_measurements = 5L, sample_seed = 20261016L,
  # These are fictional population parameters, not empirical caffeine results.
  # Sleep is useful; age, coffee consumption and both interactions have zero
  # true effects. Most people sleep around 8.2 hours, while the same focal drink
  # contrast is evaluated at 7 hours. Estimating separate drink-specific slopes
  # therefore costs precision at that reference value. Residual variation also
  # keeps the useful simple model close to 80% power at the default n.
  # No checkbox-dependent penalty or outcome-based seed selection is used.
  drink_effect = -25, sleep_effect = -30, sleep_sd = 1.1, sleep_mean = 8.2,
  person_sd = 19, measurement_sd = 19, reference_sleep = 7,
  alpha = 0.05, target_power = 0.8, max_interactions = 2L
)

pp_scalar <- function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < lower || x > upper) {
    stop(name, " is outside its supported range.", call. = FALSE)
  }
  invisible(x)
}

# Apply the same range and divisibility rule to the slider and simulation.
pp_validate_n_people <- function(n_people) {
  pp_scalar(
    n_people,
    "n_people",
    pp_defaults$min_people,
    pp_defaults$max_people
  )
  if (n_people != as.integer(n_people) || n_people %% pp_defaults$people_step != 0) {
    stop("Use a whole number of people divisible by 6.", call. = FALSE)
  }
  as.integer(n_people)
}

pp_with_seed <- function(seed, code) {
  pp_scalar(seed, "seed", 0, .Machine$integer.max)
  if (seed != as.integer(seed)) stop("seed must be an integer.", call. = FALSE)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(as.integer(seed))
  force(code)
}

pp_spec <- function(sleep = FALSE, age = FALSE, coffee = FALSE,
                    drink_sleep = FALSE, age_coffee = FALSE,
                    max_interactions = pp_defaults$max_interactions) {
  pp_scalar(max_interactions, "max_interactions", 0, 2)
  if (max_interactions != as.integer(max_interactions)) {
    stop("max_interactions must be 0, 1, or 2.", call. = FALSE)
  }
  first <- isTRUE(drink_sleep) && max_interactions >= 1L
  second <- isTRUE(age_coffee) && max_interactions >= 2L
  list(sleep = isTRUE(sleep) || first, age = isTRUE(age) || second,
       coffee = isTRUE(coffee) || second,
       drink_sleep = first, age_coffee = second)
}

pp_spec_key <- function(spec) paste(as.integer(unlist(spec)), collapse = "")

pp_formula_text <- function(spec, repeated = FALSE, account_person = TRUE) {
  terms <- if (spec$drink_sleep) "drink * hours_slept" else "drink"
  if (spec$sleep && !spec$drink_sleep) terms <- c(terms, "hours_slept")
  if (spec$age_coffee) {
    terms <- c(terms, "age_group * daily_coffee_cups")
  } else {
    if (spec$age) terms <- c(terms, "age_group")
    if (spec$coffee) terms <- c(terms, "daily_coffee_cups")
  }
  text <- paste("reaction_time_ms ~", paste(terms, collapse = " + "))
  if (isTRUE(repeated) && isTRUE(account_person)) {
    text <- paste(text, "+ (1 | person)")
  }
  text
}

pp_formula <- function(spec) stats::as.formula(pp_formula_text(spec))

pp_columns <- function(spec) {
  columns <- c("(Intercept)", "drink")
  if (spec$sleep) columns <- c(columns, "hours_slept")
  if (spec$age) columns <- c(columns, "age_group30-44", "age_group45-65")
  if (spec$coffee) columns <- c(columns, "daily_coffee_cups")
  if (spec$drink_sleep) columns <- c(columns, "drink:hours_slept")
  if (spec$age_coffee) {
    columns <- c(columns, "age_group30-44:daily_coffee_cups",
                 "age_group45-65:daily_coffee_cups")
  }
  columns
}

pp_design <- function(people, spec) {
  # Actual measurement units agree with the displayed formula.
  full <- cbind(
    "(Intercept)" = rep(1, nrow(people)), drink = people$drink,
    hours_slept = people$hours_slept,
    "age_group30-44" = as.numeric(people$age_group == "30-44"),
    "age_group45-65" = as.numeric(people$age_group == "45-65"),
    daily_coffee_cups = people$daily_coffee_cups,
    "drink:hours_slept" = people$drink * people$hours_slept,
    "age_group30-44:daily_coffee_cups" =
      (people$age_group == "30-44") * people$daily_coffee_cups,
    "age_group45-65:daily_coffee_cups" =
      (people$age_group == "45-65") * people$daily_coffee_cups
  )
  full[, pp_columns(spec), drop = FALSE]
}

pp_contrast <- function(X) {
  contrast <- setNames(rep(0, ncol(X)), colnames(X))
  if (!"drink" %in% names(contrast)) stop("The drink effect is missing.", call. = FALSE)
  contrast["drink"] <- 1
  if ("drink:hours_slept" %in% names(contrast)) {
    contrast["drink:hours_slept"] <- pp_defaults$reference_sleep
  }
  contrast
}

pp_make_study <- function(seed = pp_defaults$sample_seed,
                          n_people = pp_defaults$n_people) {
  n_people <- pp_validate_n_people(n_people)
  pp_with_seed(seed, {
    # Every six-person block includes one person per drink in each age group.
    # Slider increases add complete blocks without replacing existing people.
    # These are independent people, not matched pairs or shared random effects.
    pool_size <- pp_defaults$max_people
    first_drink <- sample(c(0, 1), pool_size / 2L, replace = TRUE)
    drinks <- as.vector(rbind(first_drink, 1 - first_drink))
    ages <- c("18-29", "30-44", "45-65")
    age_pool <- unlist(lapply(seq_len(pool_size / 6L), function(i) {
      rep(sample(ages), each = 2L)
    }), use.names = FALSE)
    people <- data.frame(
      person = seq_len(pool_size), drink = drinks,
      hours_slept = pp_defaults$sleep_mean +
        stats::rnorm(pool_size, 0, pp_defaults$sleep_sd),
      age_group = factor(age_pool, levels = ages),
      daily_coffee_cups = stats::runif(pool_size, 0, 4)
    )
    person_z <- stats::rnorm(pool_size)
    measurement_z <- matrix(
      stats::rnorm(pool_size * pp_defaults$max_measurements), nrow = pool_size
    )
    people$n_measurements <- sample.int(pp_defaults$max_measurements,
                                        pool_size, replace = TRUE)
    keep <- seq_len(n_people)
    list(people = people[keep, , drop = FALSE], person_z = person_z[keep],
         measurement_z = measurement_z[keep, , drop = FALSE])
  })
}

pp_sample_data <- function(study, repeated = FALSE,
                           drink_effect = pp_defaults$drink_effect,
                           sleep_effect = pp_defaults$sleep_effect,
                           person_sd = pp_defaults$person_sd,
                           measurement_sd = pp_defaults$measurement_sd) {
  pp_scalar(drink_effect, "drink_effect")
  pp_scalar(sleep_effect, "sleep_effect")
  pp_scalar(person_sd, "person_sd", 0)
  pp_scalar(measurement_sd, "measurement_sd", .Machine$double.eps)
  people <- study$people
  counts <- if (isTRUE(repeated)) people$n_measurements else rep(1L, nrow(people))
  index <- rep(seq_len(nrow(people)), counts)
  data <- people[index, , drop = FALSE]
  rownames(data) <- NULL
  data$measurement <- sequence(counts)
  mu <- 350 + drink_effect * (people$drink - 0.5) +
    sleep_effect * (people$hours_slept - pp_defaults$reference_sleep) +
    person_sd * study$person_z
  data$reaction_time_ms <- mu[index] + measurement_sd *
    study$measurement_z[cbind(index, data$measurement)]
  data$assigned_drink <- factor(
    ifelse(data$drink == 1, "Caffeinated coffee", "Decaf"),
    levels = c("Decaf", "Caffeinated coffee")
  )
  # Each person's first measurement stays identical when repeats are switched on.
  data
}

pp_prepare_data <- function(data) {
  predictors <- c("drink", "hours_slept", "age_group", "daily_coffee_cups")
  required <- c("person", "reaction_time_ms", predictors)
  if (!is.data.frame(data) || !all(required %in% names(data))) {
    stop("The data are missing required columns.", call. = FALSE)
  }
  if (anyNA(data[, required, drop = FALSE])) {
    stop("Missing values are not supported.", call. = FALSE)
  }
  for (name in c("reaction_time_ms", "drink", "hours_slept", "daily_coffee_cups")) {
    if (!is.numeric(data[[name]]) || !all(is.finite(data[[name]]))) {
      stop("All numeric observations must be finite.", call. = FALSE)
    }
  }
  ids <- unique(data$person)
  group <- match(data$person, ids)
  rows <- lapply(seq_along(ids), function(i) which(group == i))
  for (r in rows) {
    if (any(vapply(data[r, predictors, drop = FALSE],
                   function(x) length(unique(x)) != 1L, logical(1)))) {
      stop("Predictors must be constant within each person.", call. = FALSE)
    }
  }
  first <- vapply(rows, function(r) r[[1]], integer(1))
  people <- data[first, c("person", predictors), drop = FALSE]
  rownames(people) <- NULL
  if (!all(people$drink %in% c(0, 1)) || length(unique(people$drink)) != 2L) {
    stop("Both drink groups, coded 0 and 1, are required.", call. = FALSE)
  }
  if (!all(as.character(people$age_group) %in% c("18-29", "30-44", "45-65"))) {
    stop("Unrecognized age group.", call. = FALSE)
  }
  people$age_group <- factor(
    people$age_group,
    levels = c("18-29", "30-44", "45-65")
  )
  means <- vapply(rows, function(r) mean(data$reaction_time_ms[r]), numeric(1))
  list(people = people, group = group, counts = lengths(rows), means = means,
       y = data$reaction_time_ms,
       ss_within = sum((data$reaction_time_ms - means[group])^2))
}

pp_check_design <- function(X, counts) {
  if (!all(is.finite(X)) || qr(X)$rank != ncol(X)) {
    stop("The selected model is not full rank.", call. = FALSE)
  }
  if (nrow(X) <= ncol(X) || length(counts) != nrow(X) || any(counts < 1)) {
    stop(
      "There are insufficient independent people for this model.",
      call. = FALSE
    )
  }
}

# Full-data profiled restricted likelihood, written using grouped sufficient
# statistics. log|I + lambda ZZ'| = sum(log(1 + lambda * m_i)).
pp_profile_reml <- function(
    theta,
    X,
    counts,
    means,
    ss_within,
    details = FALSE
) {
  ratio <- expm1(theta)
  weights <- counts / (1 + ratio * counts)
  information <- crossprod(X, X * weights)
  R <- chol(information)
  inverse <- chol2inv(R)
  beta <- as.vector(inverse %*% crossprod(X, weights * means))
  residual <- means - as.vector(X %*% beta)
  q <- ss_within + sum(weights * residual^2)
  df <- sum(counts) - ncol(X)
  sigma2 <- q / df
  if (!is.finite(sigma2) || sigma2 <= .Machine$double.eps) {
    stop(
      "Residual variation is too small to estimate uncertainty.",
      call. = FALSE
    )
  }
  objective <- df * log(sigma2) + sum(log1p(ratio * counts)) +
    2 * sum(log(diag(R)))
  if (!details) return(objective)
  list(objective = objective, beta = setNames(beta, colnames(X)),
       vcov = sigma2 * inverse, sigma2 = sigma2, tau2 = ratio * sigma2,
       ratio = ratio, theta = theta)
}

# Satterthwaite denominator df for this contrast. The expected REML information
# combines the independent within-person residual contrasts with the covariance
# of the group means. This is an approximation, not an exact mixed-model t law.
pp_contrast_df <- function(X, counts, tau2, sigma2, contrast) {
  g <- nrow(X)
  p <- ncol(X)
  if (all(counts == 1L) || tau2 <= 1e-8) return(as.numeric(g - p))
  w <- 1 / (tau2 + sigma2 / counts)
  WX <- X * w
  C <- chol2inv(chol(crossprod(X, WX)))
  P <- diag(w) - WX %*% C %*% t(WX)
  P2 <- P^2
  inv_m <- 1 / counts
  I11 <- 0.5 * sum(P2)
  I12 <- 0.5 * sum(sweep(P2, 2, inv_m, "*"))
  I22 <- 0.5 * (sum(P2 * outer(inv_m, inv_m)) +
                  (sum(counts) - g) / sigma2^2)
  info <- matrix(c(I11, I12, I12, I22), nrow = 2)
  h <- as.vector(WX %*% C %*% contrast)
  gradient <- c(sum(h^2), sum(h^2 / counts))
  variance <- as.numeric(crossprod(contrast, C %*% contrast))
  variance_of_variance <- tryCatch(
    as.numeric(crossprod(gradient, solve(info, gradient))),
    error = function(e) NA_real_
  )
  if (!is.finite(variance_of_variance) || variance_of_variance <= 0) {
    return(as.numeric(g - p))
  }
  max(1, min(sum(counts) - p, 2 * variance^2 / variance_of_variance))
}

pp_fit_data <- function(
    data,
    spec,
    account_person = TRUE,
    alpha = pp_defaults$alpha
) {
  pp_scalar(alpha, "alpha", .Machine$double.eps, 1 - .Machine$double.eps)
  d <- pp_prepare_data(data)
  X <- pp_design(d$people, spec)
  pp_check_design(X, d$counts)
  contrast <- pp_contrast(X)
  repeated <- any(d$counts > 1L)
  random <- repeated && isTRUE(account_person)
  if (random) {
    objective <- function(theta) {
      pp_profile_reml(theta, X, d$counts, d$means, d$ss_within)
    }
    # Search a grid before local one-dimensional minimization; do not assume
    # a restricted-likelihood profile is globally unimodal.
    grid <- seq(0, 16, length.out = 17L)
    values <- vapply(grid, objective, numeric(1))
    best <- which.min(values)
    lo <- grid[max(1L, best - 1L)]
    hi <- grid[min(length(grid), best + 1L)]
    opt <- stats::optimize(objective, c(lo, hi), tol = 1e-8)
    candidates <- c(0, grid[best], opt$minimum)
    theta <- candidates[which.min(vapply(candidates, objective, numeric(1)))]
    if (theta >= max(grid) - 0.01) {
      stop(
        "The random-intercept variance reached the numerical search limit.",
        call. = FALSE
      )
    }
    model <- pp_profile_reml(theta, X, d$counts, d$means, d$ss_within, TRUE)
    beta <- model$beta
    covariance <- model$vcov
    tau2 <- model$tau2
    sigma2 <- model$sigma2
    df <- pp_contrast_df(X, d$counts, tau2, sigma2, contrast)
    person_effects <- tau2 / (tau2 + sigma2 / d$counts) *
      (d$means - as.vector(X %*% beta))
  } else {
    Xlong <- X[d$group, , drop = FALSE]
    model <- stats::lm.fit(Xlong, d$y)
    beta <- model$coefficients
    df <- nrow(Xlong) - model$rank
    sigma2 <- sum(model$residuals^2) / df
    if (!is.finite(sigma2) || sigma2 <= .Machine$double.eps) {
      stop(
        "Residual variation is too small to estimate uncertainty.",
        call. = FALSE
      )
    }
    inverse <- chol2inv(qr.R(model$qr))
    unpivot <- order(model$qr$pivot)
    covariance <- sigma2 * inverse[unpivot, unpivot, drop = FALSE]
    tau2 <- 0
    person_effects <- rep(0, nrow(X))
  }
  dimnames(covariance) <- list(colnames(X), colnames(X))
  estimate <- as.numeric(crossprod(contrast, beta))
  se <- sqrt(as.numeric(crossprod(contrast, covariance %*% contrast)))
  critical <- stats::qt(1 - alpha / 2, df)
  lower <- estimate - critical * se
  upper <- estimate + critical * se
  fitted <- as.vector(
    X[d$group, , drop = FALSE] %*% beta
  ) + person_effects[d$group]
  list(beta = beta, vcov = covariance, estimate = estimate, se = se,
       lower = lower, upper = upper, width = upper - lower, df = df,
       p_value = 2 * stats::pt(-abs(estimate / se), df), critical = critical,
       rank = ncol(X), n_people = nrow(X), n_observations = length(d$y),
       tau2 = tau2, sigma2 = sigma2, random = random,
       valid = !repeated || isTRUE(account_person),
       person_effects = setNames(person_effects, d$people$person),
       fitted = fitted, residuals = d$y - fitted, spec = spec)
}

pp_power <- function(data, spec, account_person = TRUE,
                      drink_effect = pp_defaults$drink_effect,
                      sleep_effect = pp_defaults$sleep_effect,
                      person_sd = pp_defaults$person_sd,
                      measurement_sd = pp_defaults$measurement_sd,
                      alpha = pp_defaults$alpha) {
  pp_scalar(drink_effect, "drink_effect")
  pp_scalar(sleep_effect, "sleep_effect")
  pp_scalar(person_sd, "person_sd", 0)
  pp_scalar(measurement_sd, "measurement_sd", .Machine$double.eps)
  pp_scalar(alpha, "alpha", .Machine$double.eps, 1 - .Machine$double.eps)
  # Only design columns and counts enter below. No fitted effect/variance enters.
  d <- pp_prepare_data(data)
  X <- pp_design(d$people, spec)
  pp_check_design(X, d$counts)
  repeated <- any(d$counts > 1L)
  if (repeated && !isTRUE(account_person)) {
    return(list(power = NA_real_, valid = FALSE, approximate = TRUE))
  }
  # If sleep is not included, marginalize its known, independent Gaussian
  # variation. It is shared by a person's repeated measurements. If sleep IS
  # included, condition on its displayed values, as for a fixed-design model.
  tau2 <- person_sd^2 + if (spec$sleep) 0 else (sleep_effect * pp_defaults$sleep_sd)^2
  sigma2 <- measurement_sd^2
  contrast <- pp_contrast(X)
  weights <- 1 / (tau2 + sigma2 / d$counts)
  covariance <- chol2inv(chol(crossprod(X, X * weights)))
  se <- sqrt(as.numeric(crossprod(contrast, covariance %*% contrast)))
  df <- if (repeated) {
    pp_contrast_df(X, d$counts, tau2, sigma2, contrast)
  } else nrow(X) - ncol(X)
  critical <- stats::qt(1 - alpha / 2, df)
  ncp <- abs(drink_effect) / se
  power <- stats::pt(-critical, df, ncp = ncp) +
    stats::pt(critical, df, ncp = ncp, lower.tail = FALSE)
  list(power = max(0, min(1, power)), valid = TRUE, approximate = repeated,
       se = se, df = df, effect = drink_effect)
}

pp_plot_view <- function(spec) {
  if (spec$sleep) {
    list(x = "hours_slept", x_label = "Hours slept", continuous = TRUE,
         size = if (spec$coffee) "daily_coffee_cups" else NULL, facet = spec$age)
  } else if (spec$coffee) {
    list(x = "daily_coffee_cups", x_label = "Cups of coffee per day", continuous = TRUE,
         size = NULL, facet = spec$age)
  } else {
    list(
      x = "drink",
      x_label = NULL,
      continuous = FALSE,
      size = NULL,
      facet = spec$age
    )
  }
}

pp_plot_points <- function(data, spec) {
  view <- pp_plot_view(spec)
  data$plot_x <- data[[view$x]]
  if (!view$continuous) {
    ids <- match(data$person, unique(data$person))
    offsets <- ((ids * 0.61803398875) %% 1 - 0.5) * 0.46
    data$plot_x <- data$drink + 1 + offsets
  }
  data
}

pp_prediction_grid <- function(data, spec, fit) {
  view <- pp_plot_view(spec)
  people <- pp_prepare_data(data)$people
  strata <- if (spec$age) levels(people$age_group) else "all"
  grids <- lapply(strata, function(stratum) {
    pool <- if (spec$age) people[people$age_group == stratum, , drop = FALSE] else people
    x_values <- if (view$continuous) {
      seq(min(pool[[view$x]]), max(pool[[view$x]]), length.out = 60L)
    } else 0
    grid <- expand.grid(x_value = x_values, drink_value = c(0, 1))
    new <- pool[rep(1L, nrow(grid)), , drop = FALSE]
    new$hours_slept <- mean(pool$hours_slept)
    new$daily_coffee_cups <- mean(pool$daily_coffee_cups)
    new$drink <- grid$drink_value
    if (view$continuous) new[[view$x]] <- grid$x_value
    new$plot_x <- if (view$continuous) grid$x_value else new$drink + 1
    new$assigned_drink <- factor(
      ifelse(new$drink == 1, "Caffeinated coffee", "Decaf"),
      levels = c("Decaf", "Caffeinated coffee")
    )
    # Population-level fitted lines from the SELECTED model, never an unrelated
    # geom_smooth() refit. Non-axis covariates are at person-level stratum means.
    new$fitted <- as.vector(pp_design(new, spec) %*% fit$beta)
    new
  })
  result <- do.call(rbind, grids)
  rownames(result) <- NULL
  result
}

# Keep the precision plot symmetric about zero, with matching signed ticks.
# A fixed minimum extent makes changing CI widths visible on a common scale.
pp_precision_axis <- function(lower, upper,
                              true_effect = pp_defaults$drink_effect) {
  pp_scalar(true_effect, "true_effect")
  endpoints <- c(lower, upper)
  endpoints <- endpoints[is.finite(endpoints)]
  extent <- max(100, abs(true_effect), abs(endpoints))
  ticks <- pretty(c(-extent, extent), n = 4)
  ticks <- sort(unique(c(-abs(ticks), 0, abs(ticks))))
  bound <- max(abs(ticks))
  list(limits = c(-bound, bound), breaks = ticks,
       labels = ifelse(ticks == 0, "0", sprintf("%+g", ticks)))
}

# Status colours and feedback depend on the reported quantities, not on which
# checkboxes were selected. Baselines are assigned separately and remain grey.
pp_power_status <- function(power, baseline, valid = TRUE,
                             target = pp_defaults$target_power) {
  if (!isTRUE(valid) || !is.finite(power)) return("invalid")
  if (power > target) return("strong")
  if (power > baseline + 1e-10) return("better")
  if (power < baseline - 1e-10) return("worse")
  "same"
}

pp_precision_status <- function(width, baseline_width, valid = TRUE) {
  if (!isTRUE(valid) || !is.finite(width) || !is.finite(baseline_width) ||
      width <= 0 || baseline_width <= 0) return("invalid")
  ratio <- width / baseline_width
  # Precision is judged ONLY by interval width, not by exclusion of zero.
  # A reduction of at least 20% uses the stronger green.
  if (ratio <= 0.8 + 1e-10) return("strong")
  if (ratio < 1 - 1e-10) return("better")
  if (ratio > 1 + 1e-10) return("worse")
  "same"
}

pp_power_feedback <- function(power, baseline, compare = TRUE, valid = TRUE,
                               target = pp_defaults$target_power) {
  if (!isTRUE(valid) || !is.finite(power)) {
    return(paste("Repeated measurements from one person are not independent.",
                 "Include a random intercept for person before interpreting power."))
  }
  level <- if (power < target) "below" else if (power > target) "above" else "at"
  if (!isTRUE(compare)) {
    first <- sprintf("Power is %s %.0f%%.", level, 100 * target)
  } else {
    change <- if (power > baseline + 1e-10) {
      "higher than"
    } else if (power < baseline - 1e-10) {
      "lower than"
    } else "the same as"
    joining <- if (change == "higher than" && power < target) {
      ", but still"
    } else if (change == "lower than" && power >= target) {
      ", but still"
    } else " and"
    first <- sprintf("Power is %s in Drink only%s %s %.0f%%.",
                     change, joining, level, 100 * target)
  }
  # One decimal avoids describing near-100% power as a guarantee.
  missed <- 100 * (1 - power)
  chance <- if (missed < 0.1) "less than 0.1%" else sprintf("%.1f%%", missed)
  paste(
    first,
    "The chance of missing the true caffeine effect is",
    paste0(chance, ".")
  )
}

pp_precision_feedback <- function(width, baseline_width, lower, upper,
                                   compare = TRUE, valid = TRUE) {
  if (!isTRUE(valid)) {
    return(paste("This interval ignores repeated measurements from the same person.",
                 "Do not use it to judge the caffeine effect."))
  }
  if (!isTRUE(compare)) {
    first <- "The interval shows uncertainty in the caffeine effect."
  } else {
    ratio <- width / baseline_width
    amount <- 100 * abs(ratio - 1)
    percent <- if (amount < 0.1) "less than 0.1%" else sprintf("%.1f%%", amount)
    if (ratio < 1 - 1e-10) {
      first <- paste("The interval is", percent,
                     "narrower than Drink only: the estimate is more precise.")
    } else if (ratio > 1 + 1e-10) {
      first <- paste("The interval is", percent,
                     "wider than Drink only: the estimate is less precise.")
    } else {
      first <- "The interval is as wide as Drink only: precision is unchanged."
    }
  }
  second <- if (lower <= 0 && upper >= 0) {
    "It includes 0: these data do not rule out no effect."
  } else "It excludes 0: these data support a caffeine effect."
  paste(first, second)
}

intercept_range <- c(-5, 12)
slope_range <- c(-2, 4)

intercept_step <- 0.25
slope_step <- 0.25

default_intercept <- intercept_range[1]
default_slope <- slope_range[1]

slider_values <- function(range, step) {
  round(seq(range[1], range[2], by = step), 10)
}

format_number <- function(x, digits = 2) {
  formatC(x, format = "f", digits = digits)
}

format_signed_slope <- function(slope, variable_name) {
  sign_text <- if (slope >= 0) {
    " + "
  } else {
    " - "
  }

  paste0(
    sign_text,
    format_number(abs(slope), digits = 2),
    " ",
    variable_name
  )
}

make_initial_seed <- function() {
  seed <- as.integer((as.numeric(Sys.time()) * 1000) %% 1000000)

  if (!is.finite(seed) || seed < 1) {
    seed <- 202607
  }

  seed
}

relationship_specs <- list(
  raw_linear = list(
    solution_log_x = FALSE,
    solution_log_y = FALSE,
    relationship_label = "Y and X",
    x_label = "X",
    y_label = "Y",
    intercept_values = seq(8, 12, by = intercept_step),
    slope_values = c(-1.25, -1, -0.75, -0.5, 0.5, 0.75, 1, 1.25),
    residual_sd = 0.6
  ),
  log_x = list(
    solution_log_x = TRUE,
    solution_log_y = FALSE,
    relationship_label = "Y and log(X)",
    x_label = "log(X)",
    y_label = "Y",
    intercept_values = seq(8, 12, by = intercept_step),
    slope_values = c(-2, -1.5, -1, -0.75, 0.75, 1, 1.5, 2),
    residual_sd = 0.6
  ),
  log_y = list(
    solution_log_x = FALSE,
    solution_log_y = TRUE,
    relationship_label = "log(Y) and X",
    x_label = "X",
    y_label = "log(Y)",
    intercept_values = seq(1.25, 2.5, by = intercept_step),
    slope_values = c(-0.5, -0.25, 0.25, 0.5),
    residual_sd = 0.25
  ),
  log_x_log_y = list(
    solution_log_x = TRUE,
    solution_log_y = TRUE,
    relationship_label = "log(Y) and log(X)",
    x_label = "log(X)",
    y_label = "log(Y)",
    intercept_values = seq(1.25, 2.5, by = intercept_step),
    slope_values = c(-1, -0.75, -0.5, 0.5, 0.75, 1),
    residual_sd = 0.25
  )
)

preserve_random_seed <- function() {
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    list(
      exists = TRUE,
      value = get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    )
  } else {
    list(
      exists = FALSE,
      value = NULL
    )
  }
}

restore_random_seed <- function(saved_seed) {
  if (isTRUE(saved_seed$exists)) {
    assign(".Random.seed", saved_seed$value, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }

  invisible(NULL)
}

generate_data_from_model <- function(
    data_seed,
    intercept,
    slope,
    residual_sd,
    log_x_model,
    log_y_model
) {
  saved_seed <- preserve_random_seed()
  on.exit(restore_random_seed(saved_seed), add = TRUE)

  set.seed(data_seed)

  n <- 100

  x_log_true <- stats::runif(
    n,
    min = log(0.4),
    max = log(4)
  )

  x <- exp(x_log_true)

  x_model <- if (isTRUE(log_x_model)) {
    log(x)
  } else {
    x
  }

  raw_error <- stats::rnorm(
    n,
    mean = 0,
    sd = residual_sd
  )

  random_error <- stats::residuals(
    stats::lm(raw_error ~ x_model)
  )

  random_error <- random_error / stats::sd(random_error) * residual_sd

  y_model <- intercept + slope * x_model + random_error

  y <- if (isTRUE(log_y_model)) {
    exp(y_model)
  } else {
    y_model
  }

  if (!all(is.finite(y)) || !all(y > 0)) {
    stop(
      "Could not simulate a valid positive Y dataset.",
      call. = FALSE
    )
  }

  data.frame(
    x = x,
    y = y
  )
}

simulate_linear_reg_data <- function(seed) {
  saved_seed <- preserve_random_seed()
  on.exit(restore_random_seed(saved_seed), add = TRUE)

  set.seed(seed)

  relationship_id <- sample(names(relationship_specs), size = 1)
  spec <- relationship_specs[[relationship_id]]

  intercept <- sample(spec$intercept_values, size = 1)
  slope <- sample(spec$slope_values, size = 1)

  max_attempts <- 100

  for (attempt in seq_len(max_attempts)) {
    candidate_data_seed <- sample.int(1000000, size = 1)

    data <- tryCatch(
      generate_data_from_model(
        data_seed = candidate_data_seed,
        intercept = intercept,
        slope = slope,
        residual_sd = spec$residual_sd,
        log_x_model = spec$solution_log_x,
        log_y_model = spec$solution_log_y
      ),
      error = function(e) NULL
    )

    if (!is.null(data)) {
      data_seed <- candidate_data_seed
      break
    }

    if (attempt == max_attempts) {
      stop(
        "Could not simulate a valid positive Y dataset.",
        call. = FALSE
      )
    }
  }

  list(
    data = data,
    seed = seed,
    data_seed = data_seed,
    relationship_id = relationship_id,
    relationship_label = spec$relationship_label,
    solution_log_x = spec$solution_log_x,
    solution_log_y = spec$solution_log_y,
    solution_intercept = intercept,
    solution_slope = slope,
    residual_sd = spec$residual_sd,
    x_label = spec$x_label,
    y_label = spec$y_label
  )
}

calculate_ssr <- function(data, intercept, slope, log_x, log_y) {
  x_display <- if (isTRUE(log_x)) {
    log(data$x)
  } else {
    data$x
  }

  y_display <- if (isTRUE(log_y)) {
    log(data$y)
  } else {
    data$y
  }

  fitted_display <- intercept + slope * x_display
  residual <- y_display - fitted_display

  sum(residual^2)
}

make_feedback_box <- function(type, label, text) {
  shiny::div(
    class = paste("feedback-box", paste0("feedback-", type)),
    shiny::span(class = "feedback-label", label),
    " ",
    text
  )
}

solution_formula_text <- function(simulation, include_error = FALSE) {
  lhs <- if (isTRUE(simulation$solution_log_y)) {
    "log(Y)"
  } else {
    "Y"
  }

  rhs <- if (isTRUE(simulation$solution_log_x)) {
    "log(X)"
  } else {
    "X"
  }

  paste0(
    lhs,
    " = ",
    format_number(simulation$solution_intercept, digits = 2),
    format_signed_slope(simulation$solution_slope, rhs),
    if (isTRUE(include_error)) {
      " + random error"
    } else {
      ""
    }
  )
}

simulation_code_text <- function(simulation) {
  x_model_line <- if (isTRUE(simulation$solution_log_x)) {
    "x_model <- log(x)"
  } else {
    "x_model <- x"
  }

  y_line <- if (isTRUE(simulation$solution_log_y)) {
    "y <- exp(y_model)"
  } else {
    "y <- y_model"
  }

  response_text <- if (isTRUE(simulation$solution_log_y)) {
    "log(y)"
  } else {
    "y"
  }

  predictor_text <- if (isTRUE(simulation$solution_log_x)) {
    "log(x)"
  } else {
    "x"
  }

  paste0(
    "set.seed(", simulation$data_seed, ")\n\n",
    "n <- 100\n",
    "intercept <- ",
    format_number(simulation$solution_intercept, digits = 2),
    "\n",
    "slope <- ",
    format_number(simulation$solution_slope, digits = 2),
    "\n",
    "residual_sd <- ",
    format_number(simulation$residual_sd, digits = 2),
    "\n\n",
    "x_log_true <- runif(n, min = log(0.4), max = log(4))\n",
    "x <- exp(x_log_true)\n\n",
    x_model_line, "\n\n",
    "raw_error <- rnorm(n, mean = 0, sd = residual_sd)\n\n",
    "random_error <- residuals(lm(raw_error ~ x_model))\n",
    "random_error <- random_error / sd(random_error) * residual_sd\n\n",
    "y_model <- intercept + slope * x_model + random_error\n",
    y_line, "\n\n",
    "linreg_data <- data.frame(x = x, y = y)\n\n",
    "summary(lm(",
    response_text,
    " ~ ",
    predictor_text,
    ", data = linreg_data))"
  )
}

statsapps_shared_file <- function(...) {
  installed_file <- system.file("app_shared", ..., package = "statsapps")

  if (nzchar(installed_file)) {
    return(installed_file)
  }

  file.path("..", "..", "app_shared", ...)
}

source(statsapps_shared_file("app_settings.R"), local = TRUE)

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::includeCSS(statsapps_shared_file("statsapps.css")),
    shiny::tags$style(shiny::HTML("
      .plot-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        column-gap: 34px;
        row-gap: 28px;
        align-items: start;
      }

      .plot-cell {
        min-width: 0;
      }

      .plot-cell .app-subtitle {
        margin-top: 18px;
      }

      @media (max-width: 1100px) {
        .plot-grid {
          grid-template-columns: 1fr;
        }
      }

      .ssr-cell {
        display: flex;
        flex-direction: column;
        align-self: end;
      }

      .plot-grid > .ssr-cell {
        align-self: end;
      }

      .ssr-plot-wrap {
        margin-top: 8px;
        margin-bottom: 8px;
      }

      .button-row {
        display: grid;
        grid-template-columns: 1fr 1fr 1.35fr;
        gap: 6px;
        margin-top: 18px;
        margin-bottom: 12px;
      }

      .button-row .btn {
        width: 100%;
        white-space: normal;
      }

      .residual-mean-note {
        margin-bottom: 8px;
      }

      .feedback-box {
        border-left: 4px solid #cccccc;
        border-radius: 4px;
        padding: 8px 10px;
        margin-top: 8px;
        margin-bottom: 12px;
        font-size: 15px;
        line-height: 1.35;
      }

      .feedback-good {
        border-left-color: #2E7D32;
        background-color: #F1F8F3;
      }

      .feedback-neutral {
        border-left-color: #A36A00;
        background-color: #FFF8E8;
      }

      .feedback-warn {
        border-left-color: #B3261E;
        background-color: #FFF2F0;
      }

      .feedback-label {
        font-weight: 700;
      }

      .ssr-title sub {
        font-size: 60%;
        vertical-align: sub;
      }

      .ssr-formula {
        font-family: Georgia, 'Times New Roman', serif;
        font-size: 21px;
        line-height: 1.4;
        margin-top: 6px;
        margin-bottom: 12px;
      }

      .ssr-formula sub {
        font-size: 65%;
      }

      .ssr-formula sup {
        font-size: 70%;
      }

      .solution-box {
        background-color: #ffffff;
        border: 1px solid #dddddd;
        border-radius: 4px;
        padding: 12px;
        margin-top: 18px;
      }

      .solution-box h4 {
        margin-top: 0;
        color: #315f96;
        font-weight: 400;
      }

      .solution-box pre {
        font-size: 12px;
        white-space: pre-wrap;
        word-break: normal;
      }

      .simulation-box {
        background-color: #f7f7f7;
      }

      .simulation-box p {
        margin-bottom: 8px;
      }
    "))
  ),

  shiny::titlePanel(
    shiny::div(
      class = "app-title",
      "Fitting a linear regression"
    )
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

      shiny::tags$p(
        class = "control-note",
        "The scatterplot shows data and a linear model that
        has parameters based on the sliders below. Find values for
        the intercept and slope that minimize the residual error. Consider
        whether it would help to log transform either variable (or both).
        Hit 'Show solution' when you think you have found the best values for
        parameters."
      ),

      shiny::sliderInput(
        "intercept",
        "Intercept",
        min = intercept_range[1],
        max = intercept_range[2],
        value = default_intercept,
        step = intercept_step
      ),

      shiny::sliderInput(
        "slope",
        "Slope",
        min = slope_range[1],
        max = slope_range[2],
        value = default_slope,
        step = slope_step
      ),

      shiny::checkboxInput(
        "log_x",
        "Use log(X)",
        value = FALSE
      ),

      shiny::checkboxInput(
        "log_y",
        "Use log(Y)",
        value = FALSE
      ),

      shiny::div(
        class = "button-row",
        shiny::actionButton("reset", "Reset"),
        shiny::actionButton("solution", "Show solution"),
        shiny::actionButton("simulate_data", "Simulate new data")
      ),

      shiny::uiOutput("solution_box")
    ),

    shiny::mainPanel(
      width = 8,

      shiny::div(
        class = "plot-grid",

        shiny::div(
          class = "plot-cell",
          shiny::div(
            class = "app-subtitle",
            "Data and fitted linear model"
          ),
          shiny::plotOutput("regression_plot", height = "390px"),
          shiny::tags$p(
            class = "plot-note",
            "The black line is your current linear model.
            Red vertical lines show residuals. The other 3 plots show further
            info on how well the linear model fits the data."
          )
        ),

        shiny::div(
          class = "plot-cell ssr-cell",
          shiny::div(
            class = "app-subtitle ssr-title",
            "Sum of squared residuals"
          ),
          shiny::div(
            class = "ssr-formula",
            shiny::HTML(
              "SS<sub>residual</sub> = &sum;<sub>i</sub> (Y<sub>i</sub> &minus; &#374;<sub>i</sub>)<sup>2</sup>"
            )
          ),
          shiny::div(
            class = "ssr-plot-wrap",
            shiny::plotOutput("ssr_plot", height = "230px")
          ),
          shiny::uiOutput("ssr_feedback")
        ),

        shiny::div(
          class = "plot-cell",
          shiny::div(
            class = "app-subtitle",
            "Residual plot"
          ),
          shiny::plotOutput("residual_plot", height = "300px"),
          shiny::uiOutput("residual_pattern_feedback")
        ),

        shiny::div(
          class = "plot-cell",
          shiny::div(
            class = "app-subtitle",
            "Distribution of residuals"
          ),
          shiny::plotOutput("residual_distribution", height = "300px"),
          shiny::uiOutput("residual_mean_feedback")
        )
      )
    )
  ),

  shiny::div(
    class = "footer-note",
    "Developed by ",
    shiny::tags$a(
      href = "https://vbaliga.github.io/",
      target = "_blank",
      rel = "noopener noreferrer",
      "Vikram Baliga"
    ),
    " at the University of British Columbia."
  )
)

server <- function(input, output, session) {
  solution_visible <- shiny::reactiveVal(FALSE)
  data_seed <- shiny::reactiveVal(make_initial_seed())

  current_simulation <- shiny::reactive({
    simulate_linear_reg_data(seed = data_seed())
  })

  current_data <- shiny::reactive({
    current_simulation()$data
  })

  reset_controls <- function() {
    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_x",
      value = FALSE
    )

    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_y",
      value = FALSE
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "intercept",
      min = intercept_range[1],
      max = intercept_range[2],
      value = default_intercept,
      step = intercept_step
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "slope",
      min = slope_range[1],
      max = slope_range[2],
      value = default_slope,
      step = slope_step
    )
  }

  displayed_data <- shiny::reactive({
    data <- current_data()

    data$x_display <- if (isTRUE(input$log_x)) {
      log(data$x)
    } else {
      data$x
    }

    data$y_display <- if (isTRUE(input$log_y)) {
      log(data$y)
    } else {
      data$y
    }

    data$fitted_display <- input$intercept + input$slope * data$x_display
    data$residual <- data$y_display - data$fitted_display

    data
  })

  output$solution_summary <- shiny::renderPrint({
    simulation <- current_simulation()
    data <- simulation$data

    model_data <- data.frame(
      Y = data$y,
      log_Y = log(data$y),
      X = data$x,
      log_X = log(data$x)
    )

    response_name <- if (isTRUE(simulation$solution_log_y)) {
      "log_Y"
    } else {
      "Y"
    }

    predictor_name <- if (isTRUE(simulation$solution_log_x)) {
      "log_X"
    } else {
      "X"
    }

    model_formula <- stats::as.formula(
      paste(response_name, "~", predictor_name)
    )

    summary(stats::lm(model_formula, data = model_data))
  })

  output$solution_box <- shiny::renderUI({
    if (!solution_visible()) {
      return(NULL)
    }

    simulation <- current_simulation()

    shiny::tagList(
      shiny::div(
        class = "solution-box",
        shiny::tags$h4("Solution"),
        shiny::tags$p(
          paste0(
            "These data show a linear relationship between ",
            simulation$relationship_label,
            "."
          )
        ),
        shiny::tags$p(
          shiny::HTML(
            paste0(
              "The best-fit line is: <strong>",
              solution_formula_text(simulation),
              "</strong>."
            )
          )
        ),
        shiny::verbatimTextOutput("solution_summary")
      ),

      shiny::div(
        class = "solution-box simulation-box",
        shiny::tags$h4("R code used to simulate these data"),
        shiny::tags$pre(
          simulation_code_text(simulation)
        )
      )
    )
  })

  shiny::observeEvent(input$solution, {
    simulation <- current_simulation()

    solution_visible(TRUE)

    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_x",
      value = simulation$solution_log_x
    )

    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_y",
      value = simulation$solution_log_y
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "intercept",
      value = simulation$solution_intercept
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "slope",
      value = simulation$solution_slope
    )
  })

  shiny::observeEvent(input$reset, {
    solution_visible(FALSE)
    reset_controls()
  })

  shiny::observeEvent(input$simulate_data, {
    solution_visible(FALSE)
    data_seed(data_seed() + 1L)
    reset_controls()
  })

  current_ssr <- shiny::reactive({
    sum(displayed_data()$residual^2)
  })

  ssr_grid <- shiny::reactive({
    intercept_values <- slider_values(intercept_range, intercept_step)
    slope_values <- slider_values(slope_range, slope_step)

    combinations <- expand.grid(
      intercept = intercept_values,
      slope = slope_values,
      KEEP.OUT.ATTRS = FALSE
    )

    combinations$ssr <- mapply(
      FUN = calculate_ssr,
      intercept = combinations$intercept,
      slope = combinations$slope,
      MoreArgs = list(
        data = current_data(),
        log_x = input$log_x,
        log_y = input$log_y
      )
    )

    combinations
  })

  target_ssr <- shiny::reactive({
    min(ssr_grid()$ssr)
  })

  ssr_relative_gap <- shiny::reactive({
    gap <- max(current_ssr() - target_ssr(), 0)

    if (target_ssr() <= .Machine$double.eps) {
      return(gap)
    }

    gap / target_ssr()
  })

  residual_mean_value <- shiny::reactive({
    mean(displayed_data()$residual)
  })

  residual_mean_scaled <- shiny::reactive({
    data <- displayed_data()
    denominator <- stats::sd(data$y_display)

    if (!is.finite(denominator) || denominator <= .Machine$double.eps) {
      return(abs(residual_mean_value()))
    }

    abs(residual_mean_value()) / denominator
  })

  residual_x_correlation <- shiny::reactive({
    data <- displayed_data()

    if (
      stats::sd(data$x_display) <= .Machine$double.eps ||
      stats::sd(data$residual) <= .Machine$double.eps
    ) {
      return(0)
    }

    stats::cor(data$x_display, data$residual)
  })

  ssr_axis_max <- shiny::reactive({
    max_ssr <- max(
      current_ssr(),
      target_ssr()
    )

    if (!is.finite(max_ssr) || max_ssr <= 0) {
      return(1)
    }

    max(pretty(c(0, max_ssr * 1.15), n = 5))
  })

  axis_labels <- shiny::reactive({
    list(
      x = if (isTRUE(input$log_x)) {
        "log(X)"
      } else {
        "X"
      },
      y = if (isTRUE(input$log_y)) {
        "log(Y)"
      } else {
        "Y"
      }
    )
  })

  line_data <- shiny::reactive({
    data <- displayed_data()

    x_values <- range(data$x_display)

    data.frame(
      x_display = x_values,
      fitted_display = input$intercept + input$slope * x_values
    )
  })

  output$regression_plot <- shiny::renderPlot({
    labels <- axis_labels()

    ggplot2::ggplot(
      displayed_data(),
      ggplot2::aes(x = x_display, y = y_display)
    ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          xend = x_display,
          yend = fitted_display
        ),
        color = "#c92514",
        linewidth = 0.3,
        alpha = 0.75
      ) +
      ggplot2::geom_point(
        color = "#222222",
        size = 2.3,
        alpha = 0.85
      ) +
      ggplot2::geom_line(
        data = line_data(),
        ggplot2::aes(x = x_display, y = fitted_display),
        color = "#222222",
        linewidth = 1.1
      ) +
      ggplot2::labs(
        x = labels$x,
        y = labels$y
      ) +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
      )
  })

  output$ssr_plot <- shiny::renderPlot({
    current_val <- current_ssr()
    target_val <- target_ssr()
    axis_max <- ssr_axis_max()

    plot_data <- data.frame(
      x = c(0.08, 0.08),
      ssr = c(current_val, target_val),
      marker = c("Current", "Target"),
      label = c("Current attempt", "Minimum")
    )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = ssr)) +
      ggplot2::geom_point(
        data = subset(plot_data, marker == "Target"),
        shape = 1,
        size = 6,
        stroke = 2,
        color = "#222222"
      ) +
      ggplot2::geom_point(
        data = subset(plot_data, marker == "Current"),
        shape = 4,
        size = 6,
        stroke = 2,
        color = "#c92514"
      ) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = 0.18,
          label = label,
          color = marker
        ),
        hjust = 0,
        size = 4.5,
        show.legend = FALSE
      ) +
      ggplot2::scale_color_manual(
        values = c(
          "Current" = "#c92514",
          "Target" = "#222222"
        )
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1),
        breaks = NULL,
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(-0.05 * axis_max, axis_max),
        breaks = pretty(c(0, axis_max), n = 5),
        expand = ggplot2::expansion(mult = c(0, 0.06))
      ) +
      ggplot2::labs(
        x = NULL,
        y = expression(SS[residual])
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.line.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_text(
          angle = 90,
          face = "bold",
          margin = ggplot2::margin(r = 10)
        ),
        axis.text.y = ggplot2::element_text(color = "#222222"),
        plot.margin = ggplot2::margin(12, 70, 24, 20)
      )
  })

  output$ssr_feedback <- shiny::renderUI({
    gap <- ssr_relative_gap()

    if (gap <= 0.001) {
      make_feedback_box(
        type = "good",
        label = "Excellent:",
        text = "SS residual is minimized for the variables as currently
        plotted."
      )
    } else if (gap <= 0.05) {
      make_feedback_box(
        type = "good",
        label = "Very close:",
        text = "The SS residual is very close to the minimum possible value."
      )
    } else if (gap <= 0.15) {
      make_feedback_box(
        type = "neutral",
        label = "Getting closer:",
        text = "The SS residual is approaching its minimum possible value.
        Try reducing it further."
      )
    } else {
      make_feedback_box(
        type = "warn",
        label = "Keep adjusting:",
        text = "The SS residual is far from its minimum possible value.
        Try to get the X close as possible to the open circle (its minimal value
        for these data)."
      )
    }
  })

  output$residual_plot <- shiny::renderPlot({
    labels <- axis_labels()

    ggplot2::ggplot(
      displayed_data(),
      ggplot2::aes(x = x_display, y = residual)
    ) +
      ggplot2::geom_hline(
        yintercept = 0,
        color = "#555555",
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        color = "#c92514",
        size = 2.3,
        alpha = 0.85
      ) +
      ggplot2::labs(
        x = labels$x,
        y = "Residual"
      ) +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
      )
  })

  output$residual_pattern_feedback <- shiny::renderUI({
    residual_correlation <- residual_x_correlation()
    abs_correlation <- abs(residual_correlation)

    if (abs_correlation <= 0.05) {
      make_feedback_box(
        type = "good",
        label = "Good sign:",
        text = "The residuals show little correlation with the explanatory
        variable. Ensure that the spread of points above and below the line is
        similar across the range of the x-axis."
      )
    } else if (abs_correlation <= 0.20) {
      make_feedback_box(
        type = "neutral",
        label = "Possible pattern:",
        text = "The residuals show a small remaining trend with X."
      )
    } else if (residual_correlation > 0) {
      make_feedback_box(
        type = "warn",
        label = "Pattern detected:",
        text = "Residuals tend to increase with X."
      )
    } else {
      make_feedback_box(
        type = "warn",
        label = "Pattern detected:",
        text = "Residuals tend to decrease with X."
      )
    }
  })

  output$residual_mean_feedback <- shiny::renderUI({
    residual_mean <- residual_mean_value()
    scaled_mean <- residual_mean_scaled()

    if (scaled_mean <= 0.01) {
      make_feedback_box(
        type = "good",
        label = "Excellent:",
        text = "The mean residual is essentially 0."
      )
    } else if (scaled_mean <= 0.05) {
      make_feedback_box(
        type = "neutral",
        label = "Close:",
        text = "The mean residual is fairly close to 0."
      )
    } else if (residual_mean > 0) {
      make_feedback_box(
        type = "warn",
        label = "Check the fit:",
        text = "The mean residual is positive, so the line is underpredicting
        on average."
      )
    } else {
      make_feedback_box(
        type = "warn",
        label = "Check the fit:",
        text = "The mean residual is negative, so the line is overpredicting
        on average."
      )
    }
  })

  output$residual_distribution <- shiny::renderPlot({
    data <- displayed_data()
    residual_mean <- mean(data$residual)

    hist_info <- hist(
      data$residual,
      breaks = 18,
      plot = FALSE
    )

    y_top <- max(hist_info$density, na.rm = TRUE)
    x_range <- range(data$residual, na.rm = TRUE)
    x_nudge <- 0.03 * diff(x_range)

    if (residual_mean <= mean(x_range)) {
      label_x <- residual_mean + x_nudge
      hjust_value <- 0
    } else {
      label_x <- residual_mean - x_nudge
      hjust_value <- 1
    }

    ggplot2::ggplot(
      data,
      ggplot2::aes(x = residual)
    ) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
        bins = 18,
        fill = "#c92514",
        color = "#222222",
        alpha = 0.85
      ) +
      ggplot2::geom_vline(
        xintercept = residual_mean,
        color = "#222222",
        linewidth = 1.1,
        linetype = "dashed"
      ) +
      ggplot2::annotate(
        "text",
        x = label_x,
        y = y_top * 0.96,
        label = paste0(
          "Mean: ",
          formatC(residual_mean, format = "f", digits = 3)
        ),
        hjust = hjust_value,
        vjust = 1,
        size = 5
      ) +
      ggplot2::labs(
        x = "Residual",
        y = "Density"
      ) +
      ggplot2::coord_cartesian(clip = "off") +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222"),
        plot.margin = ggplot2::margin(10, 20, 10, 10)
      )
  })
}

shiny::shinyApp(ui = ui, server = server)

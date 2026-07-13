true_intercept <- 8
true_slope <- 1.25
residual_sd <- 0.6

intercept_range <- c(-5, 12)
slope_range <- c(-2, 4)

set.seed(202607)

linear_reg_data <- data.frame(
  x_log_true = stats::runif(100, min = -2, max = 2)
)

linear_reg_data$x <- exp(linear_reg_data$x_log_true)

raw_error <- stats::rnorm(
  n = nrow(linear_reg_data),
  mean = 0,
  sd = residual_sd
)

random_error <- stats::residuals(
  stats::lm(raw_error ~ linear_reg_data$x_log_true)
)

random_error <- random_error / stats::sd(random_error) * residual_sd

linear_reg_data$y <- true_intercept +
  true_slope * linear_reg_data$x_log_true +
  random_error

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
      .lower-plot-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        column-gap: 32px;
        row-gap: 24px;
        align-items: start;
      }

      .plot-cell {
        min-width: 0;
      }

      .plot-cell .app-subtitle {
        margin-top: 18px;
      }

      @media (max-width: 1100px) {
        .lower-plot-grid {
          grid-template-columns: 1fr;
        }
      }

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

      .ssr-plot-wrap {
        margin-top: 8px;
        margin-bottom: 8px;
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
        value = -5,
        step = 0.25
      ),

      shiny::sliderInput(
        "slope",
        "Slope",
        min = slope_range[1],
        max = slope_range[2],
        value = -2,
        step = 0.25
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

      shiny::br(),

      shiny::actionButton(
        "reset",
        "Reset",
        width = "100%"
      ),

      shiny::br(),

      shiny::actionButton(
        "solution",
        "Show solution",
        width = "100%"
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
            Red vertical lines show residuals. The other 3 panels on this
            app show further info on how well the linear model fits the data."
          )
        ),

        shiny::div(
          class = "plot-cell ssr-cell",
          shiny::div(
            class = "app-subtitle ssr-title",
            # shiny::HTML(
            #   "SS<sub>residual</sub> = &sum;<sub>i</sub> (Y<sub>i</sub> &minus; &#374;<sub>i</sub>)<sup>2</sup>"
            # )
            #"SS",shiny::tags$sub("residual")," plot"
            "Sum of squared residuals"
          ),
          # shiny::tags$p(
          #   class = "plot-note",
          #   "The X marks the sum of squares of the residuals (",
          #   "SS",
          #   shiny::tags$sub("residual"),
          #   ") for your current attempt. The open circle marks the smallest",
          #   "SS",
          #   shiny::tags$sub("residual"),
          #   "for these data. Try to get the X as
          #   close as possible to the circle."
          # ),
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
          # shiny::tags$p(
          #   class = "plot-note",
          #   "Try to get the X close as possible to the open circle (its minimal value for these data)."
          # ),
          shiny::uiOutput("ssr_feedback")

          # shiny::tags$p(
          #   class = "plot-note",
          #   " Even if you minimize ",
          #   "SS",
          #   shiny::tags$sub("residual"),
          #   ", check the residual plots below. Minimizing the value of ",
          #   "SS",
          #   shiny::tags$sub("residual"),
          #   "does not guarantee that the regression model is
          #   appropriate!"
          # )
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
  displayed_data <- shiny::reactive({
    data <- linear_reg_data

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
    linreg_data <- data.frame(
      Y = linear_reg_data$y,
      log_X = log(linear_reg_data$x)
    )

    summary(lm(Y ~ log_X, data = linreg_data))
  })

  output$solution_box <- shiny::renderUI({
    if (!solution_visible()) {
      return(NULL)
    }

    shiny::div(
      class = "solution-box",
      shiny::tags$h4("Solution"),
      shiny::tags$p(
        "These data show a linear relationship between Y and log(X)."
      ),
      shiny::tags$p(
        shiny::HTML(
          "The best-fit line is: <strong>Y = 8 + 1.25 log(X)</strong>."
        )
      ),
      shiny::verbatimTextOutput("solution_summary")
    )
  })

  shiny::observeEvent(input$solution, {
    solution_visible(TRUE)

    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_x",
      value = TRUE
    )

    shiny::updateCheckboxInput(
      session = session,
      inputId = "log_y",
      value = FALSE
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "intercept",
      value = true_intercept
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "slope",
      value = true_slope
    )
  })

  shiny::observeEvent(input$reset, {
    solution_visible(FALSE)

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
      value = -5
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "slope",
      value = -2
    )
  })

  current_ssr <- shiny::reactive({
    sum(displayed_data()$residual^2)
  })

  target_ssr <- shiny::reactive({
    fit <- stats::lm(y_display ~ x_display, data = displayed_data())
    sum(stats::residuals(fit)^2)
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
    ssr_combinations <- expand.grid(
      intercept = intercept_range,
      slope = slope_range,
      KEEP.OUT.ATTRS = FALSE
    )

    max_ssr <- max(
      mapply(
        FUN = calculate_ssr,
        intercept = ssr_combinations$intercept,
        slope = ssr_combinations$slope,
        MoreArgs = list(
          data = linear_reg_data,
          log_x = input$log_x,
          log_y = input$log_y
        )
      )
    )

    max(pretty(c(0, max_ssr * 1.08), n = 5))
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
        limits = c(0, ssr_axis_max()),
        breaks = pretty(c(0, ssr_axis_max()), n = 5),
        expand = ggplot2::expansion(mult = c(0.08, 0.10))
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
        text = "SS residual is minimized for the variables as currently plotted."
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
        Try to get the X close as possible to the open circle (its minimal value for these data)."
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
        text = "The residuals show little correlation with X. Ensure that the
        spread of points above and below the line is similar across the range
        of X."
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
        text = "Residuals tend to increase with X, suggesting the fitted line is still missing structure."
      )
    } else {
      make_feedback_box(
        type = "warn",
        label = "Pattern detected:",
        text = "Residuals tend to decrease with X, suggesting the fitted line is still missing structure."
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
        text = "The mean residual is positive, so the line is underpredicting on average."
      )
    } else {
      make_feedback_box(
        type = "warn",
        label = "Check the fit:",
        text = "The mean residual is negative, so the line is overpredicting on average."
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

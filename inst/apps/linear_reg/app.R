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
        height: 390px;
      }

      .ssr-plot-wrap {
        margin-top: auto;
      }

      .residual-mean-note {
        margin-bottom: 8px;
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
        "Try to find values for the intercept and slope that minimize the
        residual error from the linear model. Consider whether it would help
        to log transform either variable (or both). Hit 'Show solution' when
        you think you have found the best values for parameters."
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
          shiny::tags$p(
            class = "plot-note",
            "The line is your current attempt, based on the values of the
            intercept and slope sliders. Red vertical lines show residuals: the
            differences between observed Y-values (black dots) and values
            predicted by the line."
          ),
          shiny::plotOutput("regression_plot", height = "390px")
        ),

        shiny::div(
          class = "plot-cell ssr-cell",
          shiny::div(
            class = "app-subtitle ssr-title",
            shiny::HTML(
              "SS<sub>residual</sub> = &sum;<sub>i</sub> (Y<sub>i</sub> &minus; &#374;<sub>i</sub>)<sup>2</sup>"
            )
            # "SS",
            # shiny::tags$sub("residual")
          ),

          shiny::tags$p(
            class = "plot-note",
            "In the plot below, the X marks the sum of squares of the residuals (",
            "SS",
            shiny::tags$sub("residual"),
            ") for your current attempt. The open green circle marks the smallest",
            "SS",
            shiny::tags$sub("residual"),
            "for the variables as they are currently used (i.e., after any log
            transformations you selected)."
          ),
          shiny::div(
            class = "ssr-plot-wrap",
            shiny::plotOutput("ssr_plot", height = "125px")
          ),
          # shiny::div(
          #   class = "ssr-formula",
          #   shiny::HTML(
          #     "SS<sub>residual</sub> = &sum;<sub>i</sub> (Y<sub>i</sub> &minus; &#374;<sub>i</sub>)<sup>2</sup>"
          #   )
          # ),
          shiny::tags$p(
            class = "plot-note",
            "As you adjust intercept and slope values, try to get the X as
            close as possible to the circle. That said, even if you minimize ",
            "SS",
            shiny::tags$sub("residual"),
            ", check the residual plots below. Minimizing the value of ",
            "SS",
            shiny::tags$sub("residual"),
            "does not guarantee that the regression model is
            appropriate!"
          )
        ),

        shiny::div(
          class = "plot-cell",
          shiny::div(
            class = "app-subtitle",
            "Residual plot"
          ),
          shiny::plotOutput("residual_plot", height = "300px")
        ),

        shiny::div(
          class = "plot-cell",
          shiny::div(
            class = "app-subtitle",
            "Distribution of residuals"
          ),
          shiny::uiOutput("residual_mean_text"),
          shiny::plotOutput("residual_distribution", height = "300px")
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
        "The data show a linear relationship between Y and log(X)."
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
    plot_data <- data.frame(
      ssr = c(current_ssr(), target_ssr()),
      y = c(0.28, 0.28),
      marker = c("Current", "Target")
    )

    ggplot2::ggplot(plot_data, ggplot2::aes(x = ssr, y = y)) +
      ggplot2::geom_point(
        data = subset(plot_data, marker == "Target"),
        shape = 1,
        size = 6,
        stroke = 2,
        color = "forestgreen"
      ) +
      ggplot2::geom_point(
        data = subset(plot_data, marker == "Current"),
        shape = 4,
        size = 6,
        stroke = 2,
        color = "#222222"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, ssr_axis_max()),
        breaks = pretty(c(0, ssr_axis_max()), n = 5),
        expand = ggplot2::expansion(mult = c(0.03, 0.03))
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        breaks = NULL,
        expand = c(0, 0)
      ) +
      ggplot2::labs(
        x = "Sum of squares of residuals",
        y = NULL
      ) +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.line.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_text(
          face = "bold",
          margin = ggplot2::margin(t = 10)
        ),
        axis.text.x = ggplot2::element_text(color = "#222222"),
        plot.margin = ggplot2::margin(8, 16, 20, 16)
      )
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

  output$residual_mean_text <- shiny::renderUI({
    residual_mean <- mean(displayed_data()$residual)

    shiny::tags$p(
      class = "plot-note residual-mean-note",
      shiny::HTML(
        paste0(
          "Mean value of these residuals: <strong>",
          round(residual_mean, 3),
          "</strong>"
        )
      )
    )
  })

  output$residual_distribution <- shiny::renderPlot({
    data <- displayed_data()
    residual_mean <- mean(data$residual)
    residual_sd <- stats::sd(data$residual)

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
      # ggplot2::stat_function(
      #   fun = stats::dnorm,
      #   args = list(
      #     mean = residual_mean,
      #     sd = residual_sd
      #   ),
      #   color = "#666666",
      #   linewidth = 1.1
      # ) +
      ggplot2::labs(
        x = "Residual",
        y = "Density"
      ) +
      statsapps_plot_theme() +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
      )
  })
}

shiny::shinyApp(ui = ui, server = server)

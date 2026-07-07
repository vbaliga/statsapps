true_intercept <- 8
true_slope <- 1.25
residual_sd <- 0.6

set.seed(20260707)

linear_reg_data <- data.frame(
  x_log_true = stats::runif(60, min = -2, max = 2)
)

linear_reg_data$x <- exp(linear_reg_data$x_log_true)

linear_reg_data$y <- true_intercept +
  true_slope * linear_reg_data$x_log_true +
  stats::rnorm(
    n = nrow(linear_reg_data),
    mean = 0,
    sd = residual_sd
  )

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .app-title {
        color: #315f96;
        font-size: 38px;
        font-weight: 400;
        margin-bottom: 8px;
      }

      .regression-subtitle {
        font-size: 30px;
        color: #315f96;
        margin-top: 24px;
        margin-bottom: 12px;
      }

      .control-note {
        font-size: 16px;
        line-height: 1.35;
        margin-bottom: 18px;
      }

      .footer-note {
        color: #555555;
        font-size: 14px;
        line-height: 1.35;
        margin-top: 18px;
        padding: 12px 8px 18px 8px;
        border-top: 1px solid #e5e5e5;
        width: 100%;
      }
    "))
  ),

  shiny::titlePanel(
    shiny::div(
      class = "app-title",
      "Simple Linear Regression"
    )
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

      shiny::tags$p(
        class = "control-note",
        "Try to find values for the intercept and slope that minimize the residual error from the linear model."
      ),

      shiny::sliderInput(
        "intercept",
        "Intercept",
        min = -5,
        max = 12,
        value = 5,
        step = 0.25
      ),

      shiny::sliderInput(
        "slope",
        "Slope",
        min = -2,
        max = 4,
        value = 0.75,
        step = 0.25
      ),

      shiny::checkboxInput(
        "log_x",
        "Use log(x)",
        value = FALSE
      ),

      shiny::checkboxInput(
        "log_y",
        "Use log(y)",
        value = FALSE
      )
    ),

    shiny::mainPanel(
      width = 8,

      shiny::div(
        class = "regression-subtitle",
        "Fitting a line"
      ),

      shiny::plotOutput("regression_plot", height = "420px"),

      shiny::div(
        class = "regression-subtitle",
        "Residual plot"
      ),

      shiny::plotOutput("residual_plot", height = "300px"),

      shiny::div(
        class = "regression-subtitle",
        "Distribution of residuals"
      ),

      shiny::plotOutput("residual_distribution", height = "300px")
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

  axis_labels <- shiny::reactive({
    list(
      x = if (isTRUE(input$log_x)) {
        "log(x)"
      } else {
        "x"
      },
      y = if (isTRUE(input$log_y)) {
        "log(y)"
      } else {
        "y"
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
        linewidth = 0.6,
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
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
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
        color = "#222222",
        size = 2.3,
        alpha = 0.85
      ) +
      ggplot2::labs(
        x = labels$x,
        y = "Residual"
      ) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
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
        fill = "grey75",
        color = "#222222"
      ) +
      ggplot2::stat_function(
        fun = stats::dnorm,
        args = list(
          mean = residual_mean,
          sd = residual_sd
        ),
        color = "#c92514",
        linewidth = 1.1
      ) +
      ggplot2::labs(
        x = "Residual",
        y = "Density"
      ) +
      ggplot2::theme_classic(base_size = 15) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        axis.text = ggplot2::element_text(color = "#222222")
      )
  })
}

shiny::shinyApp(ui = ui, server = server)

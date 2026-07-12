statsapps_shared_file <- function(...) {
  installed_file <- system.file("app_shared", ..., package = "statsapps")

  if (nzchar(installed_file)) {
    return(installed_file)
  }

  file.path("..", "..", "app_shared", ...)
}

source(statsapps_shared_file("app_settings.R"), local = TRUE)

distribution_choices <- c(
  "Binomial" = "binomial",
  "Chi-squared" = "chisq",
  "Poisson" = "poisson",
  "Uniform" = "uniform",
  "Normal" = "normal",
  "t" = "t",
  "F (ANOVA)" = "f"
)

distribution_label <- function(distribution) {
  switch(
    distribution,
    normal = "Normal distribution",
    t = "t distribution",
    uniform = "Uniform distribution",
    poisson = "Poisson distribution",
    binomial = "Binomial distribution",
    chisq = "Chi-squared distribution",
    f = "F distribution (ANOVA)"
  )
}

distribution_note <- function(distribution, params) {
  switch(
    distribution,
    normal = paste0(
      "This continuous probability distribution is shown with &mu; = ",
      format_number(params$mean), " and &sigma; = ",
      format_number(params$sd),
      ". Possible values range from &minus;&infin; to &infin;."
    ),
    t = paste0(
      "This continuous probability distribution is shown with df = ",
      format_number(params$df),
      ". Possible values range from &minus;&infin; to &infin;."
    ),
    uniform = paste0(
      "This continuous probability distribution is shown with minimum a = ",
      format_number(params$min), " and maximum b = ",
      format_number(params$max),
      ". Possible values range from a to b."
    ),
    poisson = paste0(
      "This discrete probability distribution models counts and is shown with &mu; = ",
      format_number(params$lambda),
      ". Possible values are whole numbers from 0 to &infin;; the final bar ",
      "combines the remaining upper tail."
    ),
    binomial = paste0(
      "This discrete probability distribution is shown with n = ",
      round(params$size), " trials and p = ",
      format_number(params$prob),
      ". Possible values are whole numbers from 0 to n."
    ),
    chisq = paste0(
      "This continuous probability distribution is shown with df = ",
      format_number(params$df),
      ". Possible values range from 0 to &infin;."
    ),
    f = paste0(
      "This continuous probability distribution is shown for an ANOVA F statistic ",
      "with df<sub>group</sub> = ", format_number(params$df_group),
      " and df<sub>error</sub> = ", format_number(params$df_error),
      ". Possible values range from 0 to &infin;."
    )
  )
}

distribution_formula <- function(distribution) {
  switch(
    distribution,
    normal = paste0(
      "\\[",
      "f(x) = \\frac{1}{\\sqrt{2\\pi\\sigma^2}} \\cdot ",
      "e^{-\\frac{(x - \\mu)^2}{2\\sigma^2}}",
      "\\]"
    ),
    t = paste0(
      "\\[",
      "f(t) = ",
      "\\frac{\\Gamma\\left(\\frac{\\mathrm{df} + 1}{2}\\right)}",
      "{\\sqrt{\\mathrm{df}\\pi}\\,\\Gamma\\left(\\frac{\\mathrm{df}}{2}\\right)}",
      "\\left(1 + \\frac{t^2}{\\mathrm{df}}\\right)^{-\\frac{\\mathrm{df} + 1}{2}}",
      "\\]"
    ),
    uniform = paste0(
      "\\[",
      "f(x) = \\frac{1}{b - a}, \\quad a \\le x \\le b",
      "\\]"
    ),
    poisson = paste0(
      "\\[",
      "\\Pr[X = x] = \\frac{\\mu^x \\cdot e^{-\\mu}}{x!}",
      "\\]"
    ),
    binomial = paste0(
      "\\[",
      "\\Pr[X = x] = {n \\choose x}p^x(1 - p)^{n - x}",
      "\\]"
    ),
    chisq = paste0(
      "\\[",
      "\\chi^2 = \\sum_i ",
      "\\frac{(\\mathrm{Observed}_i - \\mathrm{Expected}_i)^2}",
      "{\\mathrm{Expected}_i}",
      "\\]",
      "\\[",
      "\\Pr(\\chi^2 \\ge x) \\text{ is the right-tail area under the } ",
      "\\chi^2_{\\mathrm{df}} \\text{ distribution.}",
      "\\]"
    ),
    f = paste0(
      "\\[",
      "F = \\frac{MS_{\\mathrm{groups}}}{MS_{\\mathrm{error}}}",
      "\\]",
      "\\[",
      "df_{\\mathrm{group}} = k - 1, \\quad ",
      "df_{\\mathrm{error}} = N - k",
      "\\]",
      "\\[",
      "\\Pr(F \\ge x) \\text{ is the right-tail area under the } ",
      "F_{df_{\\mathrm{group}}, df_{\\mathrm{error}}} \\text{ distribution.}",
      "\\]"
    )
  )
}

distribution_formula_symbols <- function(distribution) {
  switch(
    distribution,
    normal = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>x</em> is a possible value, &mu; is the mean, and &sigma; is the standard deviation."))
    ),
    t = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>t</em> is a possible t statistic, and df is the degrees of freedom."))
    ),
    uniform = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>x</em> is a possible value. Values between <em>a</em> and <em>b</em> have equal density."))
    ),
    poisson = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>x</em> is a count. &mu; is the mean count, but in R this parameter is called lambda."))
    ),
    binomial = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>x</em> is the number of successes, <em>n</em> is the number of trials, and <em>p</em> is the probability of success."))
    ),
    chisq = shiny::tagList(
      shiny::tags$p(
        shiny::HTML(
          "<strong>Symbols:</strong> <em>Observed</em><sub>i</sub> is an observed count, <em>Expected</em><sub>i</sub> is the expected count under the null hypothesis, <em>x</em> is a possible chi-squared value, and df is the degrees of freedom."
        )
      )
    ),
    f = shiny::tagList(
      shiny::tags$p(shiny::HTML("<strong>Symbols:</strong> <em>F</em> is the ANOVA test statistic, <em>x</em> is a possible F value, df<sub>group</sub> is the numerator degrees of freedom, and df<sub>error</sub> is the denominator degrees of freedom."))
    )
  )
}

x_axis_label <- function(distribution, params) {
  switch(
    distribution,
    normal = as.expression(bquote(x)),
    t = as.expression(bquote(t[.(params$df)])),
    uniform = as.expression(bquote(x)),
    poisson = as.expression(bquote(x~"(count)")),
    binomial = as.expression(bquote(x~"(number of successes)")),
    chisq = as.expression(
      bquote(chi^2~.(paste0("(df = ", format_number(params$df), ")")))
    ),
    f = as.expression(
      bquote(F[list(.(params$df_group), .(params$df_error))])
    )
  )
}

x_axis_label_code <- function(distribution, params) {
  switch(
    distribution,
    normal = "expression(x)",
    t = paste0("expression(t[", format_number(params$df), "])"),
    uniform = "expression(x)",
    poisson = "expression(x~\"(count)\")",
    binomial = "expression(x~\"(number of successes)\")",
    chisq = paste0(
      "expression(chi^2~\"(df = ",
      format_number(params$df),
      ")\")"
    ),
    f = paste0(
      "expression(F[list(",
      format_number(params$df_group), ", ",
      format_number(params$df_error),
      ")])"
    )
  )
}

format_number <- function(x) {
  format(signif(x, 6), trim = TRUE, scientific = FALSE)
}

is_valid_number <- function(x) {
  length(x) == 1 && !is.na(x) && is.finite(x)
}

parameter_values <- function(distribution, input) {
  switch(
    distribution,
    normal = list(
      mean = input$normal_mean,
      sd = input$normal_sd
    ),
    t = list(
      df = input$t_df
    ),
    uniform = list(
      min = input$uniform_min,
      max = input$uniform_max
    ),
    poisson = list(
      lambda = input$poisson_lambda
    ),
    binomial = list(
      size = input$binomial_size,
      prob = input$binomial_prob
    ),
    chisq = list(
      df = input$chisq_df
    ),
    f = list(
      df_group = input$f_df_group,
      df_error = input$f_df_error
    )
  )
}

validate_parameters <- function(distribution, params) {
  switch(
    distribution,
    normal = {
      if (!is_valid_number(params$mean)) {
        "Mean must be a finite number."
      } else if (!is_valid_number(params$sd) || params$sd <= 0) {
        "Standard deviation must be greater than 0."
      } else {
        NULL
      }
    },
    t = {
      if (!is_valid_number(params$df) || params$df <= 0) {
        "Degrees of freedom must be greater than 0."
      } else {
        NULL
      }
    },
    uniform = {
      if (!is_valid_number(params$min) || !is_valid_number(params$max)) {
        "Minimum and maximum must be finite numbers."
      } else if (params$max <= params$min) {
        "Maximum must be greater than minimum."
      } else {
        NULL
      }
    },
    poisson = {
      if (!is_valid_number(params$lambda) || params$lambda <= 0) {
        "Lambda must be greater than 0."
      } else {
        NULL
      }
    },
    binomial = {
      if (!is_valid_number(params$size) || params$size < 1) {
        "Number of trials must be at least 1."
      } else if (params$size != round(params$size)) {
        "Number of trials must be a whole number."
      } else if (!is_valid_number(params$prob) || params$prob < 0 || params$prob > 1) {
        "Probability must be between 0 and 1."
      } else {
        NULL
      }
    },
    chisq = {
      if (!is_valid_number(params$df) || params$df <= 0) {
        "Degrees of freedom must be greater than 0."
      } else {
        NULL
      }
    },
    f = {
      if (!is_valid_number(params$df_group) || params$df_group <= 0) {
        "Group degrees of freedom must be greater than 0."
      } else if (!is_valid_number(params$df_error) || params$df_error <= 0) {
        "Error degrees of freedom must be greater than 0."
      } else {
        NULL
      }
    }
  )
}

make_distribution_data <- function(distribution, params) {
  switch(
    distribution,
    normal = {
      x <- seq(
        stats::qnorm(0.001, mean = params$mean, sd = params$sd),
        stats::qnorm(0.999, mean = params$mean, sd = params$sd),
        length.out = 1000
      )

      list(
        type = "continuous",
        data = data.frame(
          x = x,
          density = stats::dnorm(x, mean = params$mean, sd = params$sd)
        )
      )
    },
    t = {
      x <- seq(
        stats::qt(0.001, df = params$df),
        stats::qt(0.999, df = params$df),
        length.out = 1000
      )

      list(
        type = "continuous",
        data = data.frame(
          x = x,
          density = stats::dt(x, df = params$df)
        )
      )
    },
    uniform = {
      x <- seq(
        params$min,
        params$max,
        length.out = 1000
      )

      list(
        type = "continuous",
        data = data.frame(
          x = x,
          density = stats::dunif(x, min = params$min, max = params$max)
        )
      )
    },
    poisson = {
      max_x <- stats::qpois(0.999, lambda = params$lambda)
      x <- 0:max_x
      tail_x <- max_x + 1

      list(
        type = "discrete",
        data = data.frame(
          x = c(x, tail_x),
          x_label = c(as.character(x), paste0("\u2265", tail_x)),
          probability = c(
            stats::dpois(x, lambda = params$lambda),
            1 - stats::ppois(max_x, lambda = params$lambda)
          )
        )
      )
    },
    binomial = {
      x <- 0:round(params$size)

      list(
        type = "discrete",
        data = data.frame(
          x = x,
          x_label = as.character(x),
          probability = stats::dbinom(
            x,
            size = round(params$size),
            prob = params$prob
          )
        )
      )
    },
    chisq = {
      x <- seq(
        0,
        stats::qchisq(0.999, df = params$df),
        length.out = 1000
      )

      list(
        type = "continuous",
        data = data.frame(
          x = x,
          density = stats::dchisq(x, df = params$df)
        )
      )
    },
    f = {
      x <- seq(
        0,
        stats::qf(
          0.999,
          df1 = params$df_group,
          df2 = params$df_error
        ),
        length.out = 1000
      )

      list(
        type = "continuous",
        data = data.frame(
          x = x,
          density = stats::df(
            x,
            df1 = params$df_group,
            df2 = params$df_error
          )
        )
      )
    }
  )
}

make_simulation_code <- function(distribution, params) {
  switch(
    distribution,
    normal = paste0(
      "set.seed(1)\n",
      "simulated_values <- rnorm(\n",
      "  n = 1000,\n",
      "  mean = ", format_number(params$mean), ",\n",
      "  sd = ", format_number(params$sd), "\n",
      ")"
    ),
    t = paste0(
      "set.seed(1)\n",
      "simulated_values <- rt(\n",
      "  n = 1000,\n",
      "  df = ", format_number(params$df), "\n",
      ")"
    ),
    uniform = paste0(
      "set.seed(1)\n",
      "simulated_values <- runif(\n",
      "  n = 1000,\n",
      "  min = ", format_number(params$min), ",\n",
      "  max = ", format_number(params$max), "\n",
      ")"
    ),
    poisson = paste0(
      "set.seed(1)\n",
      "simulated_values <- rpois(\n",
      "  n = 1000,\n",
      "  lambda = ", format_number(params$lambda), "\n",
      ")"
    ),
    binomial = paste0(
      "set.seed(1)\n",
      "simulated_values <- rbinom(\n",
      "  n = 1000,\n",
      "  size = ", round(params$size), ",\n",
      "  prob = ", format_number(params$prob), "\n",
      ")"
    ),
    chisq = paste0(
      "set.seed(1)\n",
      "simulated_values <- rchisq(\n",
      "  n = 1000,\n",
      "  df = ", format_number(params$df), "\n",
      ")"
    ),
    f = paste0(
      "set.seed(1)\n",
      "df_group <- ", format_number(params$df_group), "\n",
      "df_error <- ", format_number(params$df_error), "\n\n",
      "simulated_values <- rf(\n",
      "  n = 1000,\n",
      "  df1 = df_group,\n",
      "  df2 = df_error\n",
      ")"
    )
  )
}

make_plot_code <- function(distribution, params) {
  switch(
    distribution,
    normal = paste0(
      "library(ggplot2)\n\n",
      "x <- seq(\n",
      "  qnorm(0.001, mean = ", format_number(params$mean),
      ", sd = ", format_number(params$sd), "),\n",
      "  qnorm(0.999, mean = ", format_number(params$mean),
      ", sd = ", format_number(params$sd), "),\n",
      "  length.out = 1000\n",
      ")\n\n",
      "density <- dnorm(x, mean = ", format_number(params$mean),
      ", sd = ", format_number(params$sd), ")\n\n",
      "plot_data <- data.frame(x = x, density = density)\n\n",
      "ggplot(plot_data, aes(x = x, y = density)) +\n",
      "  geom_area(alpha = 0.2) +\n",
      "  geom_line(linewidth = 1.2) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Density\") +\n",
      "  theme_classic()"
    ),
    t = paste0(
      "library(ggplot2)\n\n",
      "x <- seq(\n",
      "  qt(0.001, df = ", format_number(params$df), "),\n",
      "  qt(0.999, df = ", format_number(params$df), "),\n",
      "  length.out = 1000\n",
      ")\n\n",
      "density <- dt(x, df = ", format_number(params$df), ")\n\n",
      "plot_data <- data.frame(x = x, density = density)\n\n",
      "ggplot(plot_data, aes(x = x, y = density)) +\n",
      "  geom_area(alpha = 0.2) +\n",
      "  geom_line(linewidth = 1.2) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Density\") +\n",
      "  theme_classic()"
    ),
    uniform = paste0(
      "library(ggplot2)\n\n",
      "x <- seq(\n",
      "  ", format_number(params$min), ",\n",
      "  ", format_number(params$max), ",\n",
      "  length.out = 1000\n",
      ")\n\n",
      "density <- dunif(x, min = ", format_number(params$min),
      ", max = ", format_number(params$max), ")\n\n",
      "plot_data <- data.frame(x = x, density = density)\n\n",
      "ggplot(plot_data, aes(x = x, y = density)) +\n",
      "  geom_area(alpha = 0.2) +\n",
      "  geom_line(linewidth = 1.2) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Density\") +\n",
      "  theme_classic()"
    ),
    poisson = paste0(
      "library(ggplot2)\n\n",
      "max_x <- qpois(0.999, lambda = ", format_number(params$lambda), ")\n",
      "x <- 0:max_x\n",
      "tail_x <- max_x + 1\n\n",
      "probability <- c(\n",
      "  dpois(x, lambda = ", format_number(params$lambda), "),\n",
      "  1 - ppois(max_x, lambda = ", format_number(params$lambda), ")\n",
      ")\n\n",
      "plot_data <- data.frame(\n",
      "  x = c(x, tail_x),\n",
      "  x_label = c(as.character(x), paste0(\"\u2265\", tail_x)),\n",
      "  probability = probability\n",
      ")\n\n",
      "ggplot(plot_data, aes(x = x, y = probability)) +\n",
      "  geom_col() +\n",
      "  scale_x_continuous(\n",
      "    breaks = plot_data$x,\n",
      "    labels = plot_data$x_label\n",
      "  ) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Probability\") +\n",
      "  theme_classic()"
    ),
    binomial = paste0(
      "library(ggplot2)\n\n",
      "x <- 0:", round(params$size), "\n\n",
      "probability <- dbinom(\n",
      "  x,\n",
      "  size = ", round(params$size), ",\n",
      "  prob = ", format_number(params$prob), "\n",
      ")\n\n",
      "plot_data <- data.frame(x = x, probability = probability)\n\n",
      "ggplot(plot_data, aes(x = x, y = probability)) +\n",
      "  geom_col() +\n",
      "  scale_x_continuous(breaks = x) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Probability\") +\n",
      "  theme_classic()"
    ),
    chisq = paste0(
      "library(ggplot2)\n\n",
      "x <- seq(\n",
      "  0,\n",
      "  qchisq(0.999, df = ", format_number(params$df), "),\n",
      "  length.out = 1000\n",
      ")\n\n",
      "density <- dchisq(x, df = ", format_number(params$df), ")\n\n",
      "plot_data <- data.frame(x = x, density = density)\n\n",
      "ggplot(plot_data, aes(x = x, y = density)) +\n",
      "  geom_area(alpha = 0.2) +\n",
      "  geom_line(linewidth = 1.2) +\n",
      " labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Density\") +\n",
      "  theme_classic()"
    ),
    f = paste0(
      "library(ggplot2)\n\n",
      "df_group <- ", format_number(params$df_group), "\n",
      "df_error <- ", format_number(params$df_error), "\n\n",
      "x <- seq(\n",
      "  0,\n",
      "  qf(0.999, df1 = df_group, df2 = df_error),\n",
      "  length.out = 1000\n",
      ")\n\n",
      "density <- df(x, df1 = df_group, df2 = df_error)\n\n",
      "plot_data <- data.frame(x = x, density = density)\n\n",
      "ggplot(plot_data, aes(x = x, y = density)) +\n",
      "  geom_area(alpha = 0.2) +\n",
      "  geom_line(linewidth = 1.2) +\n",
      "  labs(x = ", x_axis_label_code(distribution, params),
      ", y = \"Density\") +\n",
      "  theme_classic()"
    )
  )
}

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::includeCSS(statsapps_shared_file("statsapps.css")),
    shiny::tags$style(shiny::HTML("

      .distribution-subtitle {
        font-size: 30px;
        color: #315f96;
        margin-top: 24px;
        margin-bottom: 12px;
      }

      .code-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        column-gap: 24px;
        row-gap: 20px;
        margin-top: 12px;
      }

      .code-cell {
        min-width: 0;
      }

      .code-cell h4 {
        color: #315f96;
        font-weight: 400;
        margin-top: 0;
      }

      .code-cell pre {
        white-space: pre-wrap;
        word-break: normal;
      }

      @media (max-width: 1100px) {
        .code-grid {
          grid-template-columns: 1fr;
        }
      }

      .equation-box {
        background-color: #ffffff;
        border: 1px solid #dddddd;
        border-radius: 4px;
        padding: 12px;
        margin-top: 18px;
        overflow-x: auto;
      }

      .equation-box h4 {
        color: #315f96;
        font-weight: 400;
        margin-top: 0;
        margin-bottom: 8px;
      }

      .formula-symbols {
        font-size: 14px;
        line-height: 1.35;
        margin-top: 10px;
      }

      .formula-symbols p {
        margin-bottom: 6px;
      }
    "))
  ),

  shiny::titlePanel(
    shiny::div(
      class = "app-title",
      "Exploring probability distributions"
    )
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,

      shiny::tags$p(
        class = "control-note",
        "Choose a probability distribution, then adjust its parameter values.
        The plot and example R code will update automatically."
      ),

      shiny::selectInput(
        inputId = "distribution",
        label = "Distribution",
        choices = distribution_choices,
        selected = "binomial"
      ),

      shiny::uiOutput("parameter_inputs"),

      shiny::uiOutput("distribution_equation")
    ),

    shiny::mainPanel(
      width = 8,

      shiny::div(
        class = "distribution-subtitle",
        shiny::textOutput("distribution_title", inline = TRUE)
      ),

      shiny::uiOutput("distribution_note"),

      shiny::plotOutput("distribution_plot", height = "430px"),

      shiny::div(
        class = "distribution-subtitle",
        "R code"
      ),

      shiny::tags$p(
        class = "code-note",
        "The first code block simulates random observations from the chosen
        probability distribution and the parameter values you have specified.
        The second block provides code to plot the distribution with ggplot2."
      ),

      shiny::div(
        class = "code-grid",

        shiny::div(
          class = "code-cell",
          shiny::tags$h4("Simulate values"),
          shiny::verbatimTextOutput("simulation_code")
        ),

        shiny::div(
          class = "code-cell",
          shiny::tags$h4("Plot the distribution"),
          shiny::verbatimTextOutput("plot_code")
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
  output$parameter_inputs <- shiny::renderUI({
    shiny::req(input$distribution)

    switch(
      input$distribution,
      normal = shiny::tagList(
        shiny::numericInput(
          "normal_mean",
          shiny::HTML("Mean (&mu;)"),
          value = 0
        ),
        shiny::numericInput(
          "normal_sd",
          shiny::HTML("Standard deviation (&sigma;)"),
          value = 1,
          min = 0.0001
        )
      ),
      t = shiny::tagList(
        shiny::numericInput(
          "t_df",
          "Degrees of freedom (df)",
          value = 10,
          min = 0.0001
        )
      ),
      uniform = shiny::tagList(
        shiny::numericInput(
          "uniform_min",
          "Minimum (a)",
          value = 0
        ),
        shiny::numericInput(
          "uniform_max",
          "Maximum (b)",
          value = 1
        )
      ),
      poisson = shiny::tagList(
        shiny::numericInput(
          "poisson_lambda",
          shiny::HTML("Mean count (&mu;)"),
          value = 4.20,
          min = 0.0001
        )
      ),
      binomial = shiny::tagList(
        shiny::numericInput(
          "binomial_size",
          "Number of trials (n)",
          value = 10,
          min = 1,
          step = 1
        ),
        shiny::numericInput(
          "binomial_prob",
          "Probability of success (p)",
          value = 0.5,
          min = 0,
          max = 1,
          step = 0.05
        )
      ),
      chisq = shiny::tagList(
        shiny::numericInput(
          "chisq_df",
          "Degrees of freedom (df)",
          value = 5,
          min = 0.0001
        )
      ),
      f = shiny::tagList(
        shiny::numericInput(
          "f_df_group",
          shiny::HTML("Group degrees of freedom (df<sub>group</sub>)"),
          value = 9,
          min = 0.0001
        ),
        shiny::numericInput(
          "f_df_error",
          shiny::HTML("Error degrees of freedom (df<sub>error</sub>)"),
          value = 12,
          min = 0.0001
        )
      )
    )
  })

  current_distribution <- shiny::reactive({
    shiny::req(input$distribution)
    input$distribution
  })

  current_parameters <- shiny::reactive({
    parameter_values(current_distribution(), input)
  })

  current_parameter_message <- shiny::reactive({
    validate_parameters(current_distribution(), current_parameters())
  })

  output$distribution_title <- shiny::renderText({
    distribution_label(current_distribution())
  })

  output$distribution_note <- shiny::renderUI({
    message <- current_parameter_message()

    if (!is.null(message)) {
      return(
        shiny::tags$p(
          class = "plot-note",
          "Enter valid parameter values to show the distribution."
        )
      )
    }

    shiny::tags$p(
      class = "plot-note",
      shiny::HTML(
        distribution_note(
          distribution = current_distribution(),
          params = current_parameters()
        )
      )
    )
  })

  output$distribution_equation <- shiny::renderUI({
    shiny::div(
      class = "equation-box",
      shiny::tags$h4("Formula"),
      shiny::withMathJax(
        shiny::tagList(
          shiny::HTML(
            distribution_formula(current_distribution())
          ),
          shiny::div(
            class = "formula-symbols",
            distribution_formula_symbols(current_distribution())
          )
        )
      )
    )
  })

  output$distribution_plot <- shiny::renderPlot({
    message <- current_parameter_message()

    shiny::validate(
      shiny::need(is.null(message), message)
    )

    distribution_data <- make_distribution_data(
      distribution = current_distribution(),
      params = current_parameters()
    )

    x_label <- x_axis_label(
      distribution = current_distribution(),
      params = current_parameters()
    )

    if (identical(distribution_data$type, "continuous")) {
      ggplot2::ggplot(
        distribution_data$data,
        ggplot2::aes(x = x, y = density)
      ) +
        ggplot2::geom_area(
          fill = "#315f96",
          alpha = 0.18
        ) +
        ggplot2::geom_line(
          color = "#315f96",
          linewidth = 1.2
        ) +
        ggplot2::labs(
          x = x_label,
          y = "Density"
        ) +
        statsapps_plot_theme() +
        ggplot2::theme(
          axis.title = ggplot2::element_text(face = "bold"),
          axis.text = ggplot2::element_text(color = "#222222")
        )
    } else {
      ggplot2::ggplot(
        distribution_data$data,
        ggplot2::aes(x = x, y = probability)
      ) +
        ggplot2::geom_col(
          fill = "#315f96",
          color = "#222222",
          alpha = 0.85,
          width = 0.85
        ) +
        ggplot2::scale_x_continuous(
          breaks = distribution_data$data$x,
          labels = distribution_data$data$x_label
        ) +
        ggplot2::labs(
          x = x_label,
          y = "Probability"
        ) +
        statsapps_plot_theme() +
        ggplot2::theme(
          axis.title = ggplot2::element_text(face = "bold"),
          axis.text = ggplot2::element_text(color = "#222222")
        )
    }
  })

  output$simulation_code <- shiny::renderText({
    message <- current_parameter_message()

    if (!is.null(message)) {
      return(paste0("# ", message))
    }

    make_simulation_code(
      distribution = current_distribution(),
      params = current_parameters()
    )
  })

  output$plot_code <- shiny::renderText({
    message <- current_parameter_message()

    if (!is.null(message)) {
      return(paste0("# ", message))
    }

    make_plot_code(
      distribution = current_distribution(),
      params = current_parameters()
    )
  })
}

shiny::shinyApp(ui = ui, server = server)

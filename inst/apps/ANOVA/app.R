group_colors <- c("firebrick", "goldenrod1", "#265AAE")

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      .app-title {
        color: #315f96;
        font-size: 38px;
        font-weight: 400;
        margin-bottom: 8px;
      }

      .anova-subtitle {
        font-size: 30px;
        color: #315f96;
        margin-top: 24px;
        margin-bottom: 12px;
      }

      .boxplot-wrapper {
        margin-bottom: 14px;
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
      "One-Way ANOVA Simulator"
    )
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tags$p(
        "Use the sliders below to adjust population parameters or sample sizes.
        An ANOVA will be run automatically whenever a slider is moved.
        The button at the bottom can also simulate new data using the same
        parameters and run an ANOVA."
      ),

      shiny::sliderInput(
        "mean1",
        shiny::HTML("&mu;<sub>1</sub>"),
        min = 100,
        max = 200,
        value = 150
      ),

      shiny::sliderInput(
        "mean2",
        shiny::HTML("&mu;<sub>2</sub>"),
        min = 100,
        max = 200,
        value = 160
      ),

      shiny::sliderInput(
        "mean3",
        shiny::HTML("&mu;<sub>3</sub>"),
        min = 100,
        max = 200,
        value = 150
      ),

      shiny::sliderInput(
        "sd",
        shiny::HTML("&sigma; (each group)"),
        min = 1,
        max = 35,
        value = 25
      ),

      shiny::sliderInput(
        "n",
        "n (per group)",
        min = 5,
        max = 100,
        value = 30
      ),

      shiny::br(),

      shiny::actionButton(
        "resimulate",
        "Simulate new data",
        width = "100%"
      )
    ),

    shiny::mainPanel(
      shiny::div(
        class = "boxplot-wrapper",
        shiny::plotOutput("boxplot")
      ),

      shiny::div(
        class = "anova-subtitle",
        "Analysis of Variance"
      ),

      shiny::uiOutput("anova_decomposition"),
      shiny::uiOutput("p_value_section")
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
  p_value_visible <- shiny::reactiveVal(FALSE)

  data_reactive <- shiny::reactive({

    input$resimulate

    data.frame(
      value = c(
        stats::rnorm(input$n, mean = input$mean1, sd = input$sd),
        stats::rnorm(input$n, mean = input$mean2, sd = input$sd),
        stats::rnorm(input$n, mean = input$mean3, sd = input$sd)
      ),
      group = factor(
        rep(c("Group 1", "Group 2", "Group 3"), each = input$n),
        levels = c("Group 1", "Group 2", "Group 3")
      )
    )
  })

  shiny::observeEvent(input$show_p_value, {
    p_value_visible(TRUE)
  })

  shiny::observeEvent(
    base::list(input$mean1, input$mean2, input$mean3, input$sd, input$n),
    {
      p_value_visible(FALSE)
    },
    ignoreInit = TRUE
  )

  output$boxplot <- shiny::renderPlot({
    ggplot2::ggplot(
      data_reactive(),
      ggplot2::aes(x = group, y = value, color = group, fill = group)
    ) +
      ggplot2::geom_boxplot(alpha = 0.5) +
      ggplot2::geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
      ggplot2::scale_color_manual(values = c("black", "black", "black")) +
      ggplot2::ylim(0, 300) +
      ggplot2::scale_fill_manual(values = group_colors) +
      ggplot2::labs(
        title = "Boxplot with individual data",
        x = NULL,
        y = "Value"
      ) +
      ggplot2::theme_classic(base_size = 16)
  })

  output$anova_decomposition <- shiny::renderUI({
    anova_result <- stats::aov(value ~ group, data = data_reactive())
    anova_table <- summary(anova_result)[[1]]

    SS_between <- anova_table["group", "Sum Sq"]
    df_between <- anova_table["group", "Df"]
    MS_between <- SS_between / df_between

    SS_within <- anova_table["Residuals", "Sum Sq"]
    df_within <- anova_table["Residuals", "Df"]
    MS_within <- SS_within / df_within

    SS_total <- SS_between + SS_within
    df_total <- df_between + df_within

    F_stat <- MS_between / MS_within

    shiny::HTML(
      paste0(
        "<table border='0' style='width:100%; text-align:left;'>",
        "<tr><th> </th><th>SS</th><th>df</th><th>MS</th></tr>",
        "<tr><td>Group</td><td>", round(SS_between, 3),
        "</td><td>", df_between,
        "</td><td>", round(MS_between, 3), "</td></tr>",
        "<tr><td>Error</td><td>", round(SS_within, 3),
        "</td><td>", df_within,
        "</td><td>", round(MS_within, 3), "</td></tr>",
        "</table><br>",
        "<b>SS<sub>total</sub> :</b> ", round(SS_total, 3),
        ", df<sub>total</sub>: ", df_total, "<br>",
        "<b>F :</b> ", round(F_stat, 3)
      )
    )
  })

  output$p_value_section <- shiny::renderUI({
    anova_result <- stats::aov(value ~ group, data = data_reactive())
    anova_table <- summary(anova_result)[[1]]

    p_value <- anova_table["group", "Pr(>F)"]

    p_value_text <- if (p_value < 0.0001) {
      "< 0.0001"
    } else {
      as.character(round(p_value, 4))
    }

    shiny::HTML(
      paste0(
        "<br>",
        "<b>P-value:</b> ",
        p_value_text
      )
    )
  })

}

shiny::shinyApp(ui = ui, server = server)

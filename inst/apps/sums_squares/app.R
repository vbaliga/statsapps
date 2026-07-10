statsapps_shared_file <- function(...) {
  installed_file <- system.file("app_shared", ..., package = "statsapps")

  if (nzchar(installed_file)) {
    return(installed_file)
  }

  file.path("..", "..", "app_shared", ...)
}

source(statsapps_shared_file("app_settings.R"), local = TRUE)

group_colors <- c(
  "A" = "#F4A582",
  "B" = "#315f96",
  "C" = "#E7298A"
)

component_colors <- c(
  "Total" = "#4D4D4D",
  "Groups" = "#6A3D9A",
  "Error" = "#008C8C"
)

max_step <- 6

step_title <- function(step) {
  c(
    "Data only",
    "Overall mean",
    "Total deviations",
    "Group means",
    "Groups component",
    "Error component",
    "Sums of squares"
  )[step + 1]
}

step_description <- function(step) {
  c(
    "Start with the data points only. The groups have unequal sample sizes: A has 4 observations, while B and C have 3 each.",
    "The overall mean is computed from all observations together. Because the group sizes are unequal, this is not necessarily the same as the simple average of the three group means.",
    "Total variation compares each observation to the overall mean.",
    "Each group also has its own mean. These group means summarize where each group is centered.",
    "The groups component compares each group mean to the overall mean. Each observation contributes its group mean's deviation from the overall mean.",
    "The error component compares each observation to its own group mean. This is the within-group variation left over after accounting for group membership.",
    "The total sums of squares can be partitioned into a groups component and an error component."
  )[step + 1]
}

add_plot_positions <- function(data) {
  data$x_group <- as.integer(data$group)
  data$x <- NA_real_

  for (current_group in levels(data$group)) {
    rows <- which(data$group == current_group)
    center <- unique(data$x_group[rows])
    data$x[rows] <- center + seq(-0.16, 0.16, length.out = length(rows))
  }

  data
}

simulate_sums_squares_data <- function(seed = 20260710) {
  set.seed(seed)

  group_info <- data.frame(
    group = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
    n = c(4, 3, 3),
    mean = c(8, 12, 18),
    sd = c(1.5, 1.8, 1.6)
  )

  data <- do.call(
    rbind,
    lapply(seq_len(nrow(group_info)), function(i) {
      data.frame(
        group = group_info$group[i],
        y = stats::rnorm(
          n = group_info$n[i],
          mean = group_info$mean[i],
          sd = group_info$sd[i]
        )
      )
    })
  )

  data$group <- factor(data$group, levels = c("A", "B", "C"))
  data$id <- seq_len(nrow(data))
  data <- add_plot_positions(data)

  data
}

summarize_sums_squares <- function(data) {
  grand_mean <- mean(data$y)

  group_summary <- stats::aggregate(
    y ~ group,
    data = data,
    FUN = function(x) c(n = length(x), mean = mean(x))
  )

  group_summary <- data.frame(
    group = group_summary$group,
    n = group_summary$y[, "n"],
    group_mean = group_summary$y[, "mean"]
  )

  group_summary$group <- factor(group_summary$group, levels = levels(data$group))
  group_summary$x_group <- as.integer(group_summary$group)

  data$group_mean <- group_summary$group_mean[
    match(data$group, group_summary$group)
  ]

  data$total_deviation <- data$y - grand_mean
  data$group_deviation <- data$group_mean - grand_mean
  data$error_deviation <- data$y - data$group_mean

  data$total_ss_component <- data$total_deviation^2
  data$group_ss_component <- data$group_deviation^2
  data$error_ss_component <- data$error_deviation^2

  list(
    data = data,
    group_summary = group_summary,
    grand_mean = grand_mean,
    mean_of_group_means = mean(group_summary$group_mean),
    ss_total = sum(data$total_ss_component),
    ss_groups = sum(data$group_ss_component),
    ss_error = sum(data$error_ss_component)
  )
}

make_square_data <- function(summary_object) {
  data <- summary_object$data
  grand_mean <- summary_object$grand_mean

  total_data <- data
  total_data$component <- "Total"
  total_data$start_y <- total_data$y
  total_data$end_y <- grand_mean

  groups_data <- data
  groups_data$component <- "Groups"
  groups_data$start_y <- groups_data$group_mean
  groups_data$end_y <- grand_mean

  error_data <- data
  error_data$component <- "Error"
  error_data$start_y <- error_data$y
  error_data$end_y <- error_data$group_mean

  square_data <- rbind(total_data, groups_data, error_data)

  square_data$component <- factor(
    square_data$component,
    levels = c("Total", "Groups", "Error")
  )

  square_data$deviation <- square_data$start_y - square_data$end_y
  square_data$abs_deviation <- abs(square_data$deviation)

  width_scale <- 0.055

  square_data$xmin <- square_data$x
  square_data$xmax <- square_data$x + width_scale * square_data$abs_deviation
  square_data$ymin <- pmin(square_data$start_y, square_data$end_y)
  square_data$ymax <- pmax(square_data$start_y, square_data$end_y)

  square_data
}

format_number <- function(x) {
  format(round(x, 2), nsmall = 2, trim = TRUE)
}

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::includeCSS(statsapps_shared_file("statsapps.css")),
    shiny::tags$style(shiny::HTML("
      .ss-equation {
        font-size: 18px;
        line-height: 1.35;
        margin-top: 14px;
        margin-bottom: 12px;
      }

      .sidebar-equation {
        font-size: 16px;
        line-height: 1.35;
        margin-top: 8px;
        margin-bottom: 8px;
        text-align: left;
        overflow-x: auto;
      }

      .ss-total {
        color: #4D4D4D;
        font-weight: 600;
      }

      .ss-groups {
        color: #6A3D9A;
        font-weight: 600;
      }

      .ss-error {
        color: #008C8C;
        font-weight: 600;
      }

      .ss-value-line {
        font-size: 17px;
        line-height: 1.35;
        margin-top: 4px;
        margin-bottom: 8px;
      }

      .value-box {
        background-color: #f7f7f7;
        border: 1px solid #dddddd;
        border-radius: 4px;
        padding: 12px;
        margin-bottom: 12px;
      }

      .value-box h4 {
        color: #315f96;
        margin-top: 0;
      }

      .step-label {
        color: #315f96;
        font-size: 22px;
        margin-top: 8px;
        margin-bottom: 8px;
      }

      .button-row {
        margin-top: 14px;
        margin-bottom: 14px;
      }
    "))
  ),

  shiny::div(
    class = "app-title",
    "Partitioning sums of squares"
  ),

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tags$p(
        class = "control-note",
        "Build the ANOVA sums-of-squares decomposition one layer at a time.
        All data are simulated. Hit the \"Next\" button to proceed to the next
        step."
      ),

      shiny::div(
        class = "button-row",
        shiny::actionButton("previous_step", "Previous"),
        shiny::actionButton("next_step", "Next")
      ),

      shiny::hr(),

      shiny::uiOutput("step_panel"),

      shiny::hr(),

      shiny::actionButton("reset_step", "Reset"),
      shiny::actionButton("simulate_data", "Simulate new data"),

      shiny::hr(),

      shiny::uiOutput("values_panel")
    ),

    shiny::mainPanel(
      shiny::div(
        class = "app-subtitle",
        shiny::textOutput("step_title", inline = TRUE)
      ),

      shiny::tags$p(
        class = "plot-note",
        shiny::textOutput("step_description", inline = TRUE)
      ),

      shiny::plotOutput("main_plot", height = "470px"),

      shiny::uiOutput("breakdown_panel")
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
  current_step <- shiny::reactiveVal(0)
  data_seed <- shiny::reactiveVal(20260710)

  shiny::observeEvent(input$next_step, {
    current_step(min(current_step() + 1, max_step))
  })

  shiny::observeEvent(input$previous_step, {
    current_step(max(current_step() - 1, 0))
  })

  shiny::observeEvent(input$reset_step, {
    current_step(0)
  })

  shiny::observeEvent(input$simulate_data, {
    data_seed(data_seed() + 1)
    current_step(0)
  })

  current_data <- shiny::reactive({
    simulate_sums_squares_data(seed = data_seed())
  })

  current_summary <- shiny::reactive({
    summarize_sums_squares(current_data())
  })

  output$step_title <- shiny::renderText({
    step_title(current_step())
  })

  output$step_description <- shiny::renderText({
    step_description(current_step())
  })

  output$step_panel <- shiny::renderUI({
    shiny::tagList(
      shiny::tags$div(
        class = "step-label",
        paste0("Step ", current_step(), " of ", max_step)
      ),
      shiny::tags$p(step_description(current_step()))
    )
  })

  output$main_plot <- shiny::renderPlot({
    step <- current_step()
    summary_object <- current_summary()
    data <- summary_object$data
    group_summary <- summary_object$group_summary
    grand_mean <- summary_object$grand_mean

    plot_object <- ggplot2::ggplot() +
      ggplot2::scale_color_manual(values = group_colors) +
      ggplot2::scale_fill_manual(values = group_colors) +
      ggplot2::scale_x_continuous(
        breaks = c(1, 2, 3),
        labels = c("A", "B", "C"),
        limits = c(0.55, 3.55)
      ) +
      ggplot2::labs(
        x = "Group",
        y = "Y"
      ) +
      statsapps_plot_theme()

    if (step >= 1) {
      plot_object <- plot_object +
        ggplot2::geom_hline(
          yintercept = grand_mean,
          color = "#CC0000",
          linewidth = 1.1,
          linetype = "dashed"
        )
    }

    if (step == 2) {
      plot_object <- plot_object +
        ggplot2::geom_segment(
          data = data,
          ggplot2::aes(
            x = x,
            xend = x,
            y = y,
            yend = grand_mean
          ),
          color = "gray35",
          linewidth = 0.9
        )
    }

    if (step >= 3) {
      plot_object <- plot_object +
        ggplot2::geom_segment(
          data = group_summary,
          ggplot2::aes(
            x = x_group - 0.28,
            xend = x_group + 0.28,
            y = group_mean,
            yend = group_mean,
            color = group
          ),
          linewidth = 1.8
        )
    }

    if (step == 4) {
      plot_object <- plot_object +
        ggplot2::geom_segment(
          data = group_summary,
          ggplot2::aes(
            x = x_group,
            xend = x_group,
            y = group_mean,
            yend = grand_mean,
            color = group
          ),
          linewidth = 1.2
        )
    }

    if (step == 5) {
      plot_object <- plot_object +
        ggplot2::geom_segment(
          data = data,
          ggplot2::aes(
            x = x,
            xend = x,
            y = y,
            yend = group_mean,
            color = group
          ),
          linewidth = 0.9
        )
    }

    plot_object +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = x, y = y, fill = group),
        shape = 21,
        color = "black",
        size = 4,
        stroke = 0.7
      ) +
      ggplot2::theme(legend.position = "none")
  })

  output$values_panel <- shiny::renderUI({
    step <- current_step()
    summary_object <- current_summary()
    group_summary <- summary_object$group_summary

    if (step == 0) {
      return(NULL)
    }

    value_items <- list(
      shiny::tags$p(
        shiny::HTML(
          paste0(
            "<strong>Overall mean ",
            "\\(\\left(\\bar{Y}\\right)\\)",
            ":</strong> ",
            format_number(summary_object$grand_mean)
          )
        )
      )
    )

    if (step >= 3) {
      group_lines <- lapply(seq_len(nrow(group_summary)), function(i) {
        shiny::tags$li(
          paste0(
            "Group ",
            group_summary$group[i],
            ": n = ",
            group_summary$n[i],
            ", mean = ",
            format_number(group_summary$group_mean[i])
          )
        )
      })

      value_items <- c(
        value_items,
        list(
          shiny::tags$ul(group_lines)
        )
      )
    }

    if (step >= 4) {
      value_items <- c(
        value_items,
        list(
          shiny::tags$hr(),
          shiny::div(
            class = "sidebar-equation",
            shiny::HTML(
              paste0(
                "<span class='ss-groups'>\\(SS_{groups}\\)</span> ",
                "\\(= \\sum_i n_i\\left(\\bar{Y}_i - \\bar{Y}\\right)^2 =\\) ",
                "<span class='ss-groups'>",
                format_number(summary_object$ss_groups),
                "</span>"
              )
            )
          )
        )
      )
    }

    if (step >= 5) {
      value_items <- c(
        value_items,
        list(
          shiny::div(
            class = "sidebar-equation",
            shiny::HTML(
              paste0(
                "<span class='ss-error'>\\(SS_{error}\\)</span> ",
                "\\(= \\sum_i s_i^2\\left(n_i - 1\\right) =\\) ",
                "<span class='ss-error'>",
                format_number(summary_object$ss_error),
                "</span>"
              )
            )
          )
        )
      )
    }

    if (step >= 6) {
      value_items <- c(
        value_items,
        list(
          shiny::tags$hr(),
          shiny::div(
            class = "sidebar-equation",
            shiny::HTML(
              paste0(
                "<span class='ss-total'>\\(SS_{total}\\)</span> ",
                "\\(=\\) ",
                "<span class='ss-groups'>\\(SS_{groups}\\)</span> ",
                "\\(+\\) ",
                "<span class='ss-error'>\\(SS_{error}\\)</span>"
              )
            )
          ),
          shiny::div(
            class = "ss-value-line",
            shiny::HTML(
              paste0(
                "<span class='ss-total'>",
                format_number(summary_object$ss_total),
                "</span>",
                " = ",
                "<span class='ss-groups'>",
                format_number(summary_object$ss_groups),
                "</span>",
                " + ",
                "<span class='ss-error'>",
                format_number(summary_object$ss_error),
                "</span>"
              )
            )
          )
        )
      )
    }

    shiny::div(
      class = "value-box",
      shiny::tags$h4("Computed values"),
      shiny::withMathJax(
        shiny::tagList(value_items)
      )
    )
  })

  output$breakdown_panel <- shiny::renderUI({
    if (current_step() < 6) {
      return(NULL)
    }

    shiny::tagList(
      shiny::div(
        class = "app-subtitle",
        "Visual partition"
      ),
      shiny::tags$p(
        class = "plot-note",
        "The shaded rectangles represent squared deviations. Their sizes are visual guides; the exact sums are shown above."
      ),
      shiny::plotOutput("breakdown_plot", height = "380px")
    )
  })

  output$breakdown_plot <- shiny::renderPlot({
    summary_object <- current_summary()
    square_data <- make_square_data(summary_object)
    data <- summary_object$data
    group_summary <- summary_object$group_summary

    label_y <- max(data$y) + 0.08 * diff(range(data$y))

    component_label_data <- data.frame(
      component = factor(
        names(component_colors),
        levels = c("Total", "Groups", "Error")
      ),
      x = 0.62,
      y = label_y,
      label = names(component_colors)
    )

    ggplot2::ggplot() +
      ggplot2::geom_rect(
        data = square_data,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = ymin,
          ymax = ymax,
          fill = group
        ),
        color = "black",
        alpha = 0.25
      ) +
      ggplot2::geom_segment(
        data = square_data,
        ggplot2::aes(
          x = x,
          xend = x,
          y = start_y,
          yend = end_y,
          color = group
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(
        data = data,
        ggplot2::aes(x = x, y = y, fill = group),
        shape = 21,
        color = "black",
        size = 3.5,
        stroke = 0.7
      ) +
      ggplot2::geom_hline(
        yintercept = summary_object$grand_mean,
        color = "#CC0000",
        linewidth = 0.9,
        linetype = "dashed"
      ) +
      ggplot2::geom_segment(
        data = group_summary,
        ggplot2::aes(
          x = x_group - 0.25,
          xend = x_group + 0.25,
          y = group_mean,
          yend = group_mean,
          color = group
        ),
        linewidth = 1.4
      ) +
      ggplot2::geom_text(
        data = component_label_data,
        ggplot2::aes(
          x = x,
          y = y,
          label = label,
          color = component
        ),
        hjust = 0,
        fontface = "bold",
        size = 5.5
      ) +
      ggplot2::facet_wrap(~ component, nrow = 1) +
      ggplot2::scale_color_manual(values = c(group_colors, component_colors)) +
      ggplot2::scale_fill_manual(values = group_colors) +
      ggplot2::scale_x_continuous(
        breaks = c(1, 2, 3),
        labels = c("A", "B", "C"),
        limits = c(0.55, 3.85)
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0.05, 0.18))
      ) +
      ggplot2::labs(
        x = "Group",
        y = "Y"
      ) +
      statsapps_plot_theme() +
      ggplot2::theme(
        legend.position = "none",
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank()
      )
  })
}

shiny::shinyApp(ui = ui, server = server)

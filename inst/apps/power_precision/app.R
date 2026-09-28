# A deliberately restricted, dependency-light teaching app.
# Source helpers next to this file first, so a prototype does not accidentally
# pick up helpers from an older installed version of statsapps.
pp_source_dir <- tryCatch({
  files <- vapply(sys.frames(), function(frame) {
    path <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.null(path) || !length(path)) "" else as.character(path[[1]])
  }, character(1))
  files <- files[nzchar(files)]
  if (length(files)) {
    dirname(normalizePath(tail(files, 1)))
  } else {
    character(0)
  }
}, error = function(e) character(0))
pp_app_candidates <- unique(c(
  pp_source_dir, getwd(),
  system.file("apps", "power_precision", package = "statsapps")
))
pp_app_candidates <- pp_app_candidates[nzchar(pp_app_candidates)]
pp_app_directory <- pp_app_candidates[
  file.exists(file.path(pp_app_candidates, "helpers.R"))
][1]
if (is.na(pp_app_directory)) {
  stop(
    "Could not find the power_precision app's helpers.R file.",
    call. = FALSE
  )
}
source(file.path(pp_app_directory, "helpers.R"), local = TRUE)

statsapps_shared_file <- function(...) {
  local_file <- file.path(pp_app_directory, "..", "..", "app_shared", ...)
  if (file.exists(local_file)) return(local_file)
  installed_file <- system.file("app_shared", ..., package = "statsapps")
  if (nzchar(installed_file)) return(installed_file)
  stop("Could not find the shared statsapps app assets.", call. = FALSE)
}
source(statsapps_shared_file("app_settings.R"), local = TRUE)

pp_percent <- function(x) sprintf("%.1f%%", 100 * x)
pp_plot_colors <- c(
  baseline = "#9A9FA6", strong = "#00BD6F", better = "#96D5AD",
  worse = "#EEA29E", same = "#9A9FA6", invalid = "#EEA29E"
)
pp_drink_colors <- c("Decaf" = "#756BB1", "Caffeinated coffee" = "#8C510A")
pp_reference_color <- "#0072B2"

ui <- shiny::fluidPage(
  shiny::tags$head(
    shiny::includeCSS(statsapps_shared_file("statsapps.css")),
    shiny::includeCSS(file.path(pp_app_directory, "power_precision.css"))
  ),
  shiny::div(class = "pp-app",
    shiny::div(class = "pp-layout",
      shiny::sidebarLayout(
        shiny::sidebarPanel(width = 3,
          shiny::div(class = "pp-sidebar",
            shiny::tags$h1(
              class = "app-title",
              "Sample size, effect size and power"
            ),
            shiny::tags$p(class = "pp-intro",
              "This demonstration shows how sample size and model choices affect statistical power and precision for a known effect size."
            ),
            shiny::div(class = "pp-controls",
              shiny::div(class = "pp-model-controls",
                shiny::tags$h2("Add variables to the model"),
                shiny::div(class = "pp-checkbox-row",
                  shiny::checkboxInput("include_age", "Age group", FALSE),
                  shiny::checkboxInput(
                    "include_coffee",
                    "Cups of coffee per day",
                    FALSE
                  ),
                  shiny::checkboxInput("include_sleep", "Hours slept", FALSE)
                ),
                if (pp_defaults$max_interactions >= 1L) shiny::conditionalPanel(
                  condition = if (pp_defaults$max_interactions >= 2L) {
                    "input.include_sleep || (input.include_age && input.include_coffee)"
                  } else "input.include_sleep",
                  shiny::div(class = "pp-checkbox-row pp-interactions",
                    shiny::conditionalPanel("input.include_sleep",
                      shiny::checkboxInput(
                        "interaction_one",
                        "Drink x hours slept",
                        FALSE
                      )
                    ),
                    if (pp_defaults$max_interactions >= 2L) shiny::conditionalPanel(
                      "input.include_age && input.include_coffee",
                      shiny::checkboxInput(
                        "interaction_two",
                        "Age group x daily coffee cups",
                        FALSE
                      )
                    )
                  )
                ),
                shiny::tags$details(class = "pp-repeats",
                  shiny::tags$summary("Repeated measurements"),
                  shiny::div(class = "pp-checkbox-row",
                    shiny::checkboxInput(
                      "repeated",
                      "1-5 measurements per person",
                      FALSE
                    ),
                    shiny::conditionalPanel("input.repeated",
                      shiny::checkboxInput(
                        "account_person",
                        "Random intercept for person",
                        TRUE
                      )
                    )
                  )
                )
              ),
              shiny::div(class = "pp-sample-size",
                shiny::tags$h2("Adjust sample size"),
                shiny::sliderInput("n_people", "Total people",
                  min = pp_defaults$min_people, max = pp_defaults$max_people,
                  value = pp_defaults$n_people, step = pp_defaults$people_step,
                  width = "100%"
                ),
                shiny::tags$p(
                  shiny::textOutput("allocation_note", inline = TRUE)
                )
              )
            ),
            shiny::tags$p(class = "pp-caution",
              "The relationships among sample size, effect size and power can be complex. ",
              shiny::tags$strong(
                "A sample size that gives good power here does not guarantee good power for your data."
              ),
              " Do not use these sample sizes as recommendations for your study."
            )
          )
        ),
        shiny::mainPanel(width = 9,
          shiny::div(class = "pp-main",
            shiny::tags$h2(class = "pp-fictional",
              "Fictional example: Does caffeine affect reaction time?"
            ),
            shiny::tags$p(class = "pp-study",
              shiny::textOutput("study_note", inline = TRUE)
            ),
            shiny::plotOutput("data_plot", height = "340px"),
            shiny::div(class = "pp-data-caption",
              shiny::tags$p(shiny::textOutput("data_note", inline = TRUE)),
              shiny::actionButton("new_sample", "New sample")
            ),
            shiny::div(class = "pp-model",
              shiny::tags$pre(
                shiny::textOutput("model_formula", inline = TRUE)
              ),
              shiny::uiOutput("model_note")
            ),
            shiny::div(class = "pp-results",
              shiny::uiOutput("validity_warning"),
              shiny::div(class = "pp-results-grid",
                shiny::div(class = "pp-result",
                  shiny::tags$h2(
                    shiny::textOutput("power_title", inline = TRUE)
                  ),
                  shiny::plotOutput("power_plot", height = "220px"),
                  shiny::tags$p(class = "pp-feedback",
                    shiny::textOutput("power_feedback", inline = TRUE)
                  )
                ),
                shiny::div(class = "pp-result",
                  shiny::tags$h2("Precision"),
                  shiny::plotOutput("precision_plot", height = "220px"),
                  shiny::tags$p(class = "pp-feedback",
                    shiny::textOutput("precision_feedback", inline = TRUE)
                  )
                )
              )
            )
          )
        )
      )
    ),
    shiny::div(class = "footer-note", "Developed by ",
      shiny::tags$a(href = "https://vbaliga.github.io/", target = "_blank",
                    rel = "noopener noreferrer", "Vikram Baliga"),
      " at the University of British Columbia.")
  )
)

server <- function(input, output, session) {
  sample_seed <- shiny::reactiveVal(pp_defaults$sample_seed)
  selected_spec <- shiny::reactive({
    pp_spec(
      sleep = input$include_sleep, age = input$include_age,
      coffee = input$include_coffee,
      drink_sleep = isTRUE(input$include_sleep) &&
        isTRUE(input$interaction_one),
      age_coffee = isTRUE(input$include_age) && isTRUE(input$include_coffee) &&
        isTRUE(input$interaction_two)
    )
  })
  repeated <- shiny::reactive(isTRUE(input$repeated))
  account <- shiny::reactive(!repeated() || isTRUE(input$account_person))
  n_people <- shiny::reactive({
    n <- input$n_people
    if (is.null(n)) n <- pp_defaults$n_people
    pp_validate_n_people(n)
  })
  study <- shiny::reactive(pp_make_study(sample_seed(), n_people()))
  long_data <- shiny::reactive(pp_sample_data(study(), repeated()))
  same_model <- shiny::reactive(
    identical(selected_spec(), pp_spec()) && account()
  )
  baseline_fit <- shiny::reactive(pp_fit_data(long_data(), pp_spec(), TRUE))
  current_fit <- shiny::reactive({
    if (same_model()) return(baseline_fit())
    pp_fit_data(long_data(), selected_spec(), account())
  })
  baseline_power <- shiny::reactive(pp_power(long_data(), pp_spec(), TRUE))
  current_power <- shiny::reactive({
    if (same_model()) return(baseline_power())
    pp_power(long_data(), selected_spec(), account())
  })

  shiny::observeEvent(input$new_sample, {
    sample_seed(sample_seed() + 1L)
  })
  shiny::observeEvent(input$include_sleep, {
    if (!isTRUE(input$include_sleep)) {
      shiny::updateCheckboxInput(session, "interaction_one", value = FALSE)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(list(input$include_age, input$include_coffee), {
    if (!isTRUE(input$include_age) || !isTRUE(input$include_coffee)) {
      shiny::updateCheckboxInput(session, "interaction_two", value = FALSE)
    }
  }, ignoreInit = TRUE)

  data_view <- shiny::reactive(pp_plot_view(selected_spec()))
  plot_points <- shiny::reactive(pp_plot_points(long_data(), selected_spec()))
  plot_lines <- shiny::reactive(
    pp_prediction_grid(long_data(), selected_spec(), current_fit())
  )

  data_figure <- shiny::reactive({
    spec <- selected_spec()
    view <- data_view()
    data <- plot_points()
    lines <- plot_lines()
    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = plot_x, y = reaction_time_ms)
    )
    if (repeated()) {
      plot <- plot + ggplot2::geom_line(
        ggplot2::aes(group = person), color = "grey70", linewidth = 0.6
      )
    }
    if (!is.null(view$size)) {
      plot <- plot + ggplot2::geom_point(
        ggplot2::aes(color = assigned_drink, size = daily_coffee_cups),
        alpha = 0.8
      ) + ggplot2::scale_size_continuous(
        name = "Daily coffee (cups)", range = c(2.1, 4.4), limits = c(0, 4),
        breaks = c(0, 2, 4)
      )
    } else {
      plot <- plot + ggplot2::geom_point(
        ggplot2::aes(color = assigned_drink), size = 3.1, alpha = 0.85
      )
    }
    if (view$continuous) {
      plot <- plot + ggplot2::geom_line(
        data = lines,
        ggplot2::aes(
          x = plot_x,
          y = fitted,
          color = assigned_drink,
          group = assigned_drink
        ),
        linewidth = 1.1, inherit.aes = FALSE
      ) + ggplot2::scale_x_continuous(name = view$x_label)
    } else {
      plot <- plot + ggplot2::geom_segment(
        data = lines,
        ggplot2::aes(x = plot_x - 0.25, xend = plot_x + 0.25,
                     y = fitted, yend = fitted, color = assigned_drink),
        linewidth = 1.2, inherit.aes = FALSE
      ) + ggplot2::scale_x_continuous(
        breaks = 1:2, labels = c("Decaf", "Caffeinated coffee"),
        limits = c(0.5, 2.5), name = NULL
      )
    }
    if (view$facet) plot <- plot + ggplot2::facet_wrap(~ age_group, nrow = 1)
    plot +
      ggplot2::scale_color_manual(name = "Drink", values = pp_drink_colors) +
      ggplot2::labs(y = "Reaction time (ms)") +
      statsapps_plot_theme() +
      ggplot2::theme(
        legend.position = if (view$continuous) "bottom" else "none",
        legend.title = ggplot2::element_text(size = 13),
        legend.text = ggplot2::element_text(size = 12),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(size = 15),
        panel.spacing = grid::unit(1.2, "lines")
      )
  })
  output$data_plot <- shiny::renderPlot({ data_figure() })
  output$study_note <- shiny::renderText({
    paste0(n_people(), " people: ", n_people() / 2,
           " receive caffeinated coffee; ", n_people() / 2,
           " receive decaf. Simulated data.")
  })
  output$allocation_note <- shiny::renderText({
    paste0(n_people() / 2, " people per drink.")
  })
  output$data_note <- shiny::renderText({
    if (!repeated()) return("Each point is one person.")
    paste0(nrow(long_data()), " measurements from ", n_people(), " people.")
  })
  output$model_formula <- shiny::renderText({
    pp_formula_text(selected_spec(), repeated(), account())
  })
  output$model_note <- shiny::renderUI({
    if (!selected_spec()$drink_sleep) return(NULL)
    shiny::tags$p(class = "pp-model-note", "Drink contrast at 7 hours' sleep.")
  })
  output$validity_warning <- shiny::renderUI({
    if (account()) return(NULL)
    shiny::div(class = "pp-warning", role = "alert",
      "Ignoring person identity: invalid confidence interval."
    )
  })
  output$power_title <- shiny::renderText({
    if (repeated()) "Statistical power (approx.)" else "Statistical power"
  })

  comparison_data <- shiny::reactive({
    fits <- list(baseline_fit())
    powers <- list(baseline_power())
    labels <- "Drink only"
    # The only-row baseline is STILL a baseline, so it is always grey.
    roles <- "baseline"
    if (!same_model()) {
      fits <- c(fits, list(current_fit()))
      powers <- c(powers, list(current_power()))
      labels <- c(labels, "Selected model")
      roles <- c(roles, "current")
    }
    data <- data.frame(
      model = labels, position = rev(seq_along(labels)), role = roles,
      power = vapply(powers, function(x) x$power, numeric(1)),
      estimate = vapply(fits, function(x) x$estimate, numeric(1)),
      lower = vapply(fits, function(x) x$lower, numeric(1)),
      upper = vapply(fits, function(x) x$upper, numeric(1)),
      width = vapply(fits, function(x) x$width, numeric(1)),
      valid = vapply(fits, function(x) x$valid, logical(1))
    )
    data$power_status <- "baseline"
    data$precision_status <- "baseline"
    if (nrow(data) > 1L) {
      data$power_status[2] <- pp_power_status(
        data$power[2],
        data$power[1],
        data$valid[2]
      )
      data$precision_status[2] <- pp_precision_status(
        data$width[2], data$width[1], data$valid[2]
      )
    }
    data
  })

  output$power_feedback <- shiny::renderText({
    pp_power_feedback(current_power()$power, baseline_power()$power,
                      compare = !same_model(), valid = account())
  })
  output$precision_feedback <- shiny::renderText({
    fit <- current_fit()
    pp_precision_feedback(fit$width, baseline_fit()$width, fit$lower, fit$upper,
                          compare = !same_model(), valid = fit$valid)
  })

  output$power_plot <- shiny::renderPlot({
    data <- comparison_data()
    valid <- data[is.finite(data$power), , drop = FALSE]
    invalid <- data[!is.finite(data$power), , drop = FALSE]
    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(y = position, color = power_status)
    ) +
      ggplot2::geom_vline(
        xintercept = pp_defaults$target_power,
        linetype = "dashed", color = "grey65"
      ) +
      ggplot2::geom_segment(
        data = valid,
        ggplot2::aes(x = 0, xend = power, yend = position),
        linewidth = 12, lineend = "butt"
      ) +
      ggplot2::geom_text(
        data = valid,
        ggplot2::aes(x = power + 0.025, label = pp_percent(power)),
        hjust = 0, size = 5.1, color = "#333333", show.legend = FALSE
      )
    if (nrow(invalid)) {
      plot <- plot + ggplot2::geom_text(
        data = invalid, ggplot2::aes(x = 0.03, label = "Invalid analysis"),
        hjust = 0, size = 4.5, color = "#9B3022", show.legend = FALSE
      )
    }
    plot +
      ggplot2::scale_color_manual(values = pp_plot_colors) +
      ggplot2::scale_x_continuous(
        limits = c(0, 1.17),
        breaks = c(0, 0.25, 0.5, pp_defaults$target_power, 1),
        labels = function(x) paste0(100 * x, "%"),
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::scale_y_continuous(
        breaks = data$position, labels = data$model,
        limits = c(0.45, max(data$position) + 0.6), expand = c(0, 0)
      ) +
      ggplot2::labs(
        x = sprintf(
          "Detecting a true %g ms difference",
          abs(pp_defaults$drink_effect)
        ),
        y = NULL
      ) + statsapps_plot_theme() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(size = 13),
        axis.title.x = ggplot2::element_text(size = 14)
      )
  })

  precision_figure <- shiny::reactive({
    data <- comparison_data()
    axis <- pp_precision_axis(data$lower, data$upper)

    # White, borderless labels keep the reference line out of the numbers.
    # Use the supported border argument in both ggplot2 3.x and 4.x.
    label_border <- if ("border.colour" %in% names(formals(ggplot2::geom_label))) {
      list(linewidth = 0)
    } else {
      list(label.size = 0)
    }
    value_labels <- do.call(ggplot2::geom_label, c(list(
      mapping = ggplot2::aes(
        x = (lower + upper) / 2, y = position + 0.25,
        label = sprintf("%+.1f  [%+.1f, %+.1f]", estimate, lower, upper)
      ),
      size = 3.9, color = "black", fill = "white",
      label.padding = grid::unit(0.12, "lines"),
      label.r = grid::unit(0, "lines"), show.legend = FALSE
    ), label_border))

    ggplot2::ggplot(
      data,
      ggplot2::aes(y = position, color = precision_status)
    ) +
      ggplot2::geom_vline(
        xintercept = pp_defaults$drink_effect, linetype = "dashed",
        color = pp_reference_color
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = lower, xend = upper, yend = position), linewidth = 1.8
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = lower,
          xend = lower,
          y = position - 0.07,
          yend = position + 0.07
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = upper,
          xend = upper,
          y = position - 0.07,
          yend = position + 0.07
        ),
        linewidth = 0.8
      ) +
      ggplot2::geom_point(ggplot2::aes(x = estimate), size = 3.5) +
      value_labels +
      ggplot2::annotate(
        "text", x = pp_defaults$drink_effect + 0.025 * diff(axis$limits),
        y = max(data$position) + 0.6,
        label = sprintf("True effect (%g ms)", pp_defaults$drink_effect),
        hjust = 0, size = 3.7, color = pp_reference_color
      ) +
      ggplot2::scale_color_manual(values = pp_plot_colors) +
      ggplot2::scale_x_continuous(
        limits = axis$limits, breaks = axis$breaks, labels = axis$labels,
        expand = ggplot2::expansion(mult = c(0.04, 0.04))
      ) +
      ggplot2::scale_y_continuous(
        breaks = data$position, labels = data$model,
        limits = c(0.45, max(data$position) + 0.8), expand = c(0, 0)
      ) +
      ggplot2::labs(
        x = "Caffeine - decaf (ms)", y = NULL,
        subtitle = if (repeated()) "95% CI (approx.)" else "95% CI"
      ) + statsapps_plot_theme() +
      ggplot2::theme(
        legend.position = "none",
        axis.text.y = ggplot2::element_text(size = 13),
        axis.title.x = ggplot2::element_text(size = 14),
        plot.subtitle = ggplot2::element_text(size = 13, color = "black")
      )
  })
  output$precision_plot <- shiny::renderPlot({ precision_figure() })
}

shiny::shinyApp(ui = ui, server = server)

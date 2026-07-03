## PERMUTATION APP

library(shiny)
library(ggplot2)
library(dplyr)

## Original cricket data from Johnson et al. 1999,
## showcased in Example 13.5 of ABD.
cricket_data <- tibble(
  original_group = c(
    rep("Starved", 11),
    rep("Fed", 13)
  ),
  time_hours = c(
    1.9, 2.1, 3.8, 9.0, 9.6, 13.0, 14.7, 17.9, 21.7, 29.0, 72.3,
    1.5, 1.7, 2.4, 3.6, 5.7, 22.6, 22.8, 39.0, 54.4, 72.1, 73.6,
    79.5, 88.9
  )
) |>
  mutate(
    observation_id = row_number()
  )

## Fixed observed difference from the real study.
observed_starved_mean <- cricket_data |>
  filter(original_group == "Starved") |>
  summarise(mean_time = mean(time_hours)) |>
  pull(mean_time)

observed_fed_mean <- cricket_data |>
  filter(original_group == "Fed") |>
  summarise(mean_time = mean(time_hours)) |>
  pull(mean_time)

observed_difference <- observed_starved_mean - observed_fed_mean

## Group sizes are preserved in every permutation.
n_starved <- sum(cricket_data$original_group == "Starved")
n_fed <- sum(cricket_data$original_group == "Fed")

permute_once <- function(data) {
  randomized_labels <- sample(
    c(rep("Starved", n_starved), rep("Fed", n_fed)),
    size = nrow(data),
    replace = FALSE
  )

  permuted_data <- data |>
    mutate(
      randomized_group = randomized_labels
    )

  randomized_starved_mean <- permuted_data |>
    filter(randomized_group == "Starved") |>
    summarise(mean_time = mean(time_hours)) |>
    pull(mean_time)

  randomized_fed_mean <- permuted_data |>
    filter(randomized_group == "Fed") |>
    summarise(mean_time = mean(time_hours)) |>
    pull(mean_time)

  permuted_difference <- randomized_starved_mean - randomized_fed_mean

  list(
    permuted_data = permuted_data,
    permuted_difference = permuted_difference,
    randomized_starved_mean = randomized_starved_mean,
    randomized_fed_mean = randomized_fed_mean
  )
}

make_slide_table <- function(data, group_name) {
  data |>
    filter(randomized_group == group_name) |>
    arrange(time_hours) |>
    mutate(
      row_class = if_else(original_group == "Fed", "original-fed", "original-starved")
    )
}

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body {
        font-size: 16px;
        color: #222222;
        background-color: #ffffff;
      }

      .container-fluid {
        max-width: 1450px;
      }

      .app-title {
        color: #315f96;
        font-size: 38px;
        font-weight: 400;
        margin-bottom: 8px;
      }

      .explanation-box {
        background-color: #ffffff;
        border-left: 6px solid #315f96;
        padding: 10px 18px;
        margin-bottom: 18px;
      }

      .well {
        padding: 8px;
      }

      .control-box {
        background-color: #f7f7f7;
        padding: 10px;
        border-radius: 6px;
        margin-bottom: 10px;
      }

      .control-box h4 {
        margin-top: 0;
      }

      .small-note {
        color: #555555;
        font-size: 14px;
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

      .current-progress-text {
        font-size: 13px;
        line-height: 1.35;
      }

      .current-progress-text p {
        margin-bottom: 8px;
      }

      .current-progress-heading {
        font-size: 16px;
        margin-top: 0;
        margin-bottom: 10px;
      }

      .equation-large {
        font-size: 28px;
        color: #222222;
        margin-top: 18px;
        margin-bottom: 12px;
        text-align: left;
      }

      .slide-table-wrapper {
        display: flex;
        gap: 32px;
        align-items: flex-start;
        margin-top: 14px;
      }

      .slide-table-block {
        width: 48%;
      }

      table.slide-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 14px;
        text-align: center;
        background-color: white;
      }

      table.slide-table th {
        border-top: 3px solid #222222;
        border-bottom: 2px solid #222222;
        padding: 7px 10px;
        font-size: 14px;
        text-align: center;
        font-weight: 700;
      }

      table.slide-table td {
        border-bottom: 1px solid #e5e5e5;
        padding: 7px 10px;
      }

      table.slide-table tfoot td {
        border-top: 3px solid #222222;
        border-bottom: 3px solid #222222;
        font-weight: 700;
        font-size: 24px;
        padding-top: 10px;
        padding-bottom: 10px;
      }

      .original-fed {
        background-color: #f6dfcf;
      }

      .original-starved {
        background-color: #ffffff;
      }

      .permutation-subtitle {
        font-size: 30px;
        color: #315f96;
        margin-bottom: 12px;
      }

      .histogram-note {
        font-size: 16px;
        color: #444444;
      }

      .nav-tabs > li > a {
        font-size: 17px;
      }

      .explanation-tab {
        max-width: 900px;
        font-size: 18px;
        line-height: 1.45;
      }

      .explanation-tab h3 {
        color: #315f96;
        font-size: 30px;
        font-weight: 400;
        margin-top: 12px;
        margin-bottom: 18px;
      }

      .explanation-tab p {
        margin-bottom: 14px;
      }

      .original-slide-layout {
        display: grid;
        grid-template-columns: minmax(0, 40%) minmax(0, 60%);
        column-gap: 42px;
        align-items: start;
        padding: 8px 4px 4px 4px;
        background-color: #ffffff;
      }

      .original-slide-left {
        min-width: 0;
        padding-left: 2px;
      }

      .original-slide-right {
        min-width: 0;
        padding-top: 8px;
      }

      .lecture-table-title {
        font-size: 16px;
        line-height: 1.3;
        font-weight: 700;
        letter-spacing: 0.1px;
        margin-bottom: 16px;
        max-width: 100%;
      }

      .lecture-link {
        color: #008c95;
        text-decoration: underline;
        font-weight: 700;
      }

      .original-slide-tables {
        display: grid;
        grid-template-columns: minmax(0, 47%) minmax(0, 47%);
        column-gap: 42px;
        align-items: start;
      }

      table.original-lecture-table {
        width: 100%;
        border-collapse: collapse;
        font-size: 17px;
        background-color: white;
      }

      table.original-lecture-table th {
        border-bottom: 3px solid #16323a;
        padding: 5px 9px;
        text-align: left;
        font-weight: 700;
      }

      table.original-lecture-table td {
        border-bottom: 1px solid #e8e8e8;
        padding: 7px 9px;
      }

      table.original-lecture-table td:nth-child(2),
      table.original-lecture-table th:nth-child(2) {
        text-align: center;
      }

      table.original-lecture-table tfoot td {
        border-top: 3px solid #16323a;
        border-bottom: 3px solid #16323a;
        padding-top: 9px;
        padding-bottom: 9px;
        font-weight: 400;
      }

      .blank-row td {
        height: 24px;
        border-bottom: none !important;
      }

      .lecture-equation {
        font-size: 28px;
        line-height: 1.1;
        margin-top: 42px;
        text-align: left;
        color: #000000;
        padding-left: 28px;
      }

      .permutation-layout {
        display: grid;
        grid-template-columns: minmax(0, 40%) minmax(0, 60%);
        column-gap: 28px;
        align-items: start;
      }

      .permutation-left {
        min-width: 0;
      }

      .permutation-right {
        min-width: 0;
        padding-top: 0;
      }

      .permutation-histogram-text {
        font-size: 17px;
        line-height: 1.35;
        margin-bottom: 10px;
      }

      .permutation-note {
        font-size: 17px;
        line-height: 1.35;
        margin-bottom: 12px;
      }

      @media (max-width: 1100px) {
        .original-slide-layout {
          grid-template-columns: 1fr;
          row-gap: 28px;
        }

        .original-slide-tables {
          column-gap: 24px;
        }

        .lecture-equation {
          font-size: 32px;
        }

        .permutation-layout {
          grid-template-columns: 1fr;
          row-gap: 24px;
        }

        .permutation-right {
          padding-top: 0;
        }
      }
    "))
  ),

  titlePanel(
    div(
      class = "app-title",
      "Permutation tests: building a null distribution"
    )
  ),

  div(
    class = "explanation-box",
    tags$div(
      class = "equation-large",
      HTML(
        paste0(
          "Observed: Ȳ<sub>starved</sub> − Ȳ<sub>fed</sub> = ",
          round(observed_starved_mean, 2),
          " − ",
          round(observed_fed_mean, 2),
          " = ",
          round(observed_difference, 2),
          " hours"
        )
      )
    )
  ),

  sidebarLayout(

    sidebarPanel(
      width = 2,

      div(
        class = "control-box",
        h4("Build the null distribution"),

        actionButton("permute_once", "Permute once", width = "100%"),
        br(), br(),

        actionButton("permute_10", "Run 10 more", width = "100%"),
        br(), br(),


        actionButton("permute_1000", "Run 1000 more", width = "100%"),
        br(), br(),

        actionButton("reset", "Reset", width = "100%")
      ),

      div(
        class = "control-box current-progress-text",
        h4(class = "current-progress-heading", "Current progress"),
        textOutput("n_permutations"),
        textOutput("n_lower_tail"),
        uiOutput("p_value_summary")
      )
    ),

    mainPanel(
      width = 10,

      tabsetPanel(

        tabPanel(
          "Explanation",
          br(),
          div(
            class = "explanation-tab",
            h3("How to use this app"),
            p("This app uses the sagebrush cricket data from Johnson et al. 1999, showcased in Example 13.5 of ABD. The 'Original data' tab reproduces Table 13.8-1 for reference."),
            p("The 'Permutatation' tab allows you to perform and visualize permutations. Each permutation randomly reassigns the observed times into two groups with the same sample sizes as the original study: 11 starved and 13 fed."),
            p("The app shows one randomized outcome at a time while also building a histogram of the mean differences from all permutations generated so far. The treatment names are retained, but the observed times have been randomly reassigned. Pale orange undershading identifies values that originally came from the fed group."),
            p("The histogram is built from all permutations generated so far. The goal is to understand what kinds of mean differences are plausible under the null hypothesis. The red bars show permuted mean differences that are less than or equal to the observed study difference of -18.26 hours.")
          )
        ),

        tabPanel(
          "Original data",
          br(),
          div(
            class = "original-slide-layout",

            div(
              class = "original-slide-left",

              div(
                class = "lecture-table-title",
                HTML(
                  "Times to mating (in hours) of female sagebrush crickets that were recently starved or fed. Data from the two treatments are color-coded to more easily identify the origin of each value later in the <span class='lecture-link'>Permutation</span> tab."
                )
              ),

              uiOutput("original_slide_table_only"),

              div(
                class = "lecture-equation",
                HTML(
                  paste0(
                    "Ȳ<sub>1</sub> − Ȳ<sub>2</sub> = ",
                    round(observed_starved_mean, 2),
                    " − ",
                    round(observed_fed_mean, 2),
                    " = ",
                    round(observed_difference, 2)
                  )
                )
              )
            ),

            div(
              class = "original-slide-right",
              plotOutput("original_histograms", height = "540px")
            )
          )
        ),

        tabPanel(
          "Permutation",
          br(),
          div(
            class = "permutation-layout",

            div(
              class = "permutation-left",

              div(class = "permutation-subtitle", "Most recent permutation"),

              p(
                class = "permutation-note",
                HTML(
                  "If more than one permutation is performed using the buttons to the left, <strong>only the most recent permutation is shown</strong>."
                )
              ),

              uiOutput("current_permutation_slide_table"),

              uiOutput("current_permutation_equation")
            ),

            div(
              class = "permutation-right",

              div(class = "permutation-subtitle", "The Null distribution"),

              div(
                class = "permutation-histogram-text",
                HTML(
                  "Each permutation contributes one value of Ȳ<sub>1</sub> − Ȳ<sub>2</sub> to this histogram."
                )
              ),

              plotOutput("null_histogram", height = "420px"),

              uiOutput("histogram_explanation")
            )
          )
        )
      )
    )
  ),

  div(
    class = "footer-note",
    "Developed by Vikram Baliga at the University of British Columbia, using resources associated with the Analysis of Biological Data by Whitlock & Schluter."
  )
)

server <- function(input, output, session) {

  permutation_history <- reactiveVal(tibble(permutation_id = integer(),
                                            permuted_difference = numeric()))

  current_permutation <- reactiveVal(NULL)

  current_permutation_summary <- reactiveVal(NULL)

  add_permutations <- function(n) {
    old_history <- permutation_history()
    old_n <- nrow(old_history)

    new_results <- map(seq_len(n), function(i) {
      result <- permute_once(cricket_data)

      if (i == n) {
        current_permutation(result$permuted_data)
        current_permutation_summary(
          tibble(
            randomized_starved_mean = result$randomized_starved_mean,
            randomized_fed_mean = result$randomized_fed_mean,
            permuted_difference = result$permuted_difference
          )
        )
      }

      tibble(
        permutation_id = old_n + i,
        permuted_difference = result$permuted_difference
      )
    }) |>
      bind_rows()

    permutation_history(bind_rows(old_history, new_results))
  }

  observeEvent(input$permute_once, {
    add_permutations(1)
  })

  observeEvent(input$permute_10, {
    add_permutations(10)
  })

  observeEvent(input$permute_100, {
    add_permutations(100)
  })

  observeEvent(input$permute_1000, {
    add_permutations(1000)
  })

  observeEvent(input$reset, {
    permutation_history(tibble(permutation_id = integer(),
                               permuted_difference = numeric()))
    current_permutation(NULL)
    current_permutation_summary(NULL)
  })

  output$original_slide_table_only <- renderUI({
    starved_data <- cricket_data |>
      filter(original_group == "Starved") |>
      arrange(time_hours)

    fed_data <- cricket_data |>
      filter(original_group == "Fed") |>
      arrange(time_hours)

    starved_rows <- lapply(seq_len(nrow(starved_data)), function(i) {
      tags$tr(
        class = "original-starved",
        tags$td("Starved"),
        tags$td(starved_data$time_hours[i])
      )
    })

    fed_rows <- lapply(seq_len(nrow(fed_data)), function(i) {
      tags$tr(
        class = "original-fed",
        tags$td("Fed"),
        tags$td(fed_data$time_hours[i])
      )
    })

    ## Add blank rows to the shorter table so the two means align visually,
    ## as in the lecture slide.
    starved_rows <- c(
      starved_rows,
      list(
        tags$tr(class = "blank-row", tags$td(""), tags$td("")),
        tags$tr(class = "blank-row", tags$td(""), tags$td(""))
      )
    )

    div(
      class = "original-slide-tables",

      tags$table(
        class = "original-lecture-table",
        tags$thead(
          tags$tr(
            tags$th("Treatment"),
            tags$th("Time (hours)")
          )
        ),
        tags$tbody(starved_rows),
        tags$tfoot(
          tags$tr(
            tags$td("Mean"),
            tags$td(round(observed_starved_mean, 2))
          )
        )
      ),

      tags$table(
        class = "original-lecture-table",
        tags$thead(
          tags$tr(
            tags$th("Treatment"),
            tags$th("Time (hours)")
          )
        ),
        tags$tbody(fed_rows),
        tags$tfoot(
          tags$tr(
            tags$td("Mean"),
            tags$td(round(observed_fed_mean, 2))
          )
        )
      )
    )
  })

  output$original_histograms <- renderPlot({
    cricket_data |>
      ggplot(aes(x = time_hours)) +
      geom_histogram(
        binwidth = 20,
        boundary = 0,
        closed = "left",
        fill = "#d20a11",
        color = "#222222",
        linewidth = 0.7
      ) +
      facet_wrap(
        vars(original_group),
        ncol = 1,
        scales = "free_y",
        labeller = as_labeller(c(
          "Starved" = "Starved females",
          "Fed" = "Fed females"
        ))
      ) +
      scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = seq(0, 8, 2),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = "Time to mating (hours)",
        y = "Frequency"
      ) +
      theme_classic(base_size = 18) +
      theme(
        strip.background = element_blank(),
        strip.text = element_text(
          face = "italic",
          size = 19,
          hjust = 0.86,
          color = "#222222"
        ),
        axis.title = element_text(face = "bold", size = 19),
        axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        axis.text = element_text(color = "#222222", size = 16),
        axis.line = element_line(color = "#222222", linewidth = 0.7),
        axis.ticks = element_line(color = "#222222", linewidth = 0.7),
        panel.spacing = unit(1.7, "lines"),
        plot.margin = margin(4, 18, 4, 4)
      )
  })

  output$current_permutation_slide_table <- renderUI({
    validate(
      need(!is.null(current_permutation()), "Click 'Permute once' or run a batch of permutations to display one randomized outcome.")
    )

    permutation_data <- current_permutation()

    starved_data <- make_slide_table(permutation_data, "Starved")
    fed_data <- make_slide_table(permutation_data, "Fed")

    starved_mean <- mean(starved_data$time_hours)
    fed_mean <- mean(fed_data$time_hours)

    tagList(
      div(
        class = "slide-table-wrapper",

        div(
          class = "slide-table-block",
          tags$table(
            class = "slide-table",
            tags$thead(
              tags$tr(
                tags$th("Treatment"),
                tags$th("Time (hrs)")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(starved_data)), function(i) {
                tags$tr(
                  class = starved_data$row_class[i],
                  tags$td("Starved"),
                  tags$td(starved_data$time_hours[i])
                )
              })
            ),
            tags$tfoot(
              tags$tr(
                tags$td("Mean"),
                tags$td(round(starved_mean, 2))
              )
            )
          )
        ),

        div(
          class = "slide-table-block",
          tags$table(
            class = "slide-table",
            tags$thead(
              tags$tr(
                tags$th("Treatment"),
                tags$th("Time (hrs)")
              )
            ),
            tags$tbody(
              lapply(seq_len(nrow(fed_data)), function(i) {
                tags$tr(
                  class = fed_data$row_class[i],
                  tags$td("Fed"),
                  tags$td(fed_data$time_hours[i])
                )
              })
            ),
            tags$tfoot(
              tags$tr(
                tags$td("Mean"),
                tags$td(round(fed_mean, 2))
              )
            )
          )
        )
      )
    )
  })

  output$current_permutation_equation <- renderUI({
    validate(
      need(!is.null(current_permutation_summary()), "")
    )

    summary <- current_permutation_summary()

    div(
      class = "equation-large",
      HTML(
        paste0(
          "Ȳ<sub>1</sub> − Ȳ<sub>2</sub> = ",
          round(summary$randomized_starved_mean, 2),
          " − ",
          round(summary$randomized_fed_mean, 2),
          " = ",
          round(summary$permuted_difference, 2)
        )
      )
    )
  })

  output$null_histogram <- renderPlot({
    history <- permutation_history()

    validate(
      need(nrow(history) > 0, "Click 'Permute once' or run a batch of permutations to begin building the null distribution.")
    )

    history <- history |>
      mutate(
        lower_tail = permuted_difference <= observed_difference
      )

    ggplot(history, aes(x = permuted_difference)) +
      geom_histogram(
        data = filter(history, !lower_tail),
        binwidth = 2,
        boundary = 0,
        fill = "white",
        color = "black"
      ) +
      geom_histogram(
        data = filter(history, lower_tail),
        binwidth = 2,
        boundary = 0,
        fill = "#c92514",
        color = "black"
      ) +
      geom_vline(
        xintercept = observed_difference,
        linewidth = 1.1,
        color = "#555555"
      ) +
      annotate(
        "text",
        x = observed_difference,
        y = Inf,
        label = round(observed_difference, 2),
        vjust = 1.6,
        hjust = 1.1,
        size = 5,
        color = "#222222"
      ) +
      scale_x_continuous(
        limits = c(-50, 50),
        breaks = seq(-40, 40, 20),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(
        x = "Difference in treatment means\nfrom randomized data (hours)",
        y = "Frequency"
      ) +
      theme_classic(base_size = 20) +
      theme(
        axis.title = element_text(face = "bold"),
        # axis.title.x = element_text(
        #   size = 20,
        #   lineheight = 1.0,
        #   margin = margin(t = 10)
        # ),
        axis.text = element_text(color = "black"),
        plot.margin = margin(4, 6, 12, 6)
      )
  })

  output$n_permutations <- renderText({
    n <- nrow(permutation_history())
    paste0("Number of permutations generated: ", n)
  })

  output$n_lower_tail <- renderText({
    history <- permutation_history()

    if (nrow(history) == 0) {
      return("Lower-tail count: not yet available")
    }

    lower_tail_count <- sum(history$permuted_difference <= observed_difference)

    paste0(
      "Lower-tail count: ",
      lower_tail_count
    )
  })

  output$p_value_summary <- renderUI({
    history <- permutation_history()

    if (nrow(history) == 0) {
      return(tags$p("Two-tailed P-value: not yet available"))
    }

    lower_tail_count <- sum(history$permuted_difference <= observed_difference)
    lower_tail_proportion <- lower_tail_count / nrow(history)
    two_tailed_p <- 2 * lower_tail_proportion

    tags$div(
      tags$p(
        paste0(
          "Lower-tail proportion = ",
          lower_tail_count,
          " / ",
          nrow(history),
          " = ",
          round(lower_tail_proportion, 4)
        )
      ),
      tags$p(
        paste0(
          "Two-tailed P-value = 2 × ",
          round(lower_tail_proportion, 4),
          " = ",
          round(two_tailed_p, 4)
        )
      )
    )
  })

  output$histogram_explanation <- renderUI({
    history <- permutation_history()

    if (nrow(history) == 0) {
      return(tags$p("No permutations have been generated yet."))
    }

    lower_tail_count <- sum(history$permuted_difference <= observed_difference)
    lower_tail_proportion <- lower_tail_count / nrow(history)
    two_tailed_p <- 2 * lower_tail_proportion

    tags$div(
      class = "histogram-note",
      tags$p(
        paste0(
          lower_tail_count,
          " of ",
          nrow(history),
          " outcomes are less than or equal to the observed study difference of ",
          round(observed_difference, 2),
          " hours."
        )
      ),
      tags$p(
        paste0(
          "Two-tailed P-value = 2 × ",
          round(lower_tail_proportion, 4),
          " = ",
          round(two_tailed_p, 4)
        )
      )
    )
  })
}

shinyApp(ui = ui, server = server)

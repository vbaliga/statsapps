## PERMUTATION APP


## Original cricket data from Johnson et al. 1999,
## showcased in Example 13.5 of ABD.
cricket_data <- tibble::tibble(
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
  dplyr::mutate(
    observation_id = dplyr::row_number()
  )

## Fixed observed difference from the real study.
observed_starved_mean <- cricket_data |>
  dplyr::filter(original_group == "Starved") |>
  dplyr::summarize(mean_time = mean(time_hours)) |>
  dplyr::pull(mean_time)

observed_fed_mean <- cricket_data |>
  dplyr::filter(original_group == "Fed") |>
  dplyr::summarize(mean_time = mean(time_hours)) |>
  dplyr::pull(mean_time)

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
    dplyr::mutate(
      randomized_group = randomized_labels
    )

  randomized_starved_mean <- permuted_data |>
    dplyr::filter(randomized_group == "Starved") |>
    dplyr::summarize(mean_time = mean(time_hours)) |>
    dplyr::pull(mean_time)

  randomized_fed_mean <- permuted_data |>
    dplyr::filter(randomized_group == "Fed") |>
    dplyr::summarize(mean_time = mean(time_hours)) |>
    dplyr::pull(mean_time)

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
    dplyr::filter(randomized_group == group_name) |>
    dplyr::arrange(time_hours) |>
    dplyr::mutate(
      row_class = dplyr::if_else(
        original_group == "Fed",
        "original-fed",
        "original-starved"
      )
    )
}

ui <- shiny::fluidPage(

  shiny::tags$head(
    shiny::tags$style(shiny::HTML("
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

  shiny::titlePanel(
    shiny::div(
      class = "app-title",
      "Permutation tests: building a null distribution"
    )
  ),

  shiny::div(
    class = "explanation-box",
    shiny::tags$div(
      class = "equation-large",
      shiny::HTML(
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

  shiny::sidebarLayout(

    shiny::sidebarPanel(
      width = 2,

      shiny::div(
        class = "control-box",
        shiny::h4("Build the null distribution"),

        shiny::actionButton("permute_once", "Permute once", width = "100%"),
        shiny::br(), shiny::br(),

        shiny::actionButton("permute_10", "Run 10 more", width = "100%"),
        shiny::br(), shiny::br(),


        shiny::actionButton("permute_1000", "Run 1000 more", width = "100%"),
        shiny::br(), shiny::br(),

        shiny::actionButton("reset", "Reset", width = "100%")
      ),

      shiny::div(
        class = "control-box current-progress-text",
        shiny::h4(class = "current-progress-heading", "Current progress"),
        shiny::textOutput("n_permutations"),
        shiny::textOutput("n_lower_tail"),
        shiny::uiOutput("p_value_summary")
      )
    ),

    shiny::mainPanel(
      width = 10,

      shiny::tabsetPanel(

        shiny::tabPanel(
          "Explanation",
          shiny::br(),
          shiny::div(
            class = "explanation-tab",
            shiny::h3("How to use this app"),
            shiny::p("This app uses the sagebrush cricket data from Johnson et al. 1999, showcased in Example 13.5 of ABD. The 'Original data' tab reproduces Table 13.8-1 for reference."),
            shiny::p("The 'Permutatation' tab allows you to perform and visualize permutations. Each permutation randomly reassigns the observed times into two groups with the same sample sizes as the original study: 11 starved and 13 fed."),
            shiny::p("The app shows one randomized outcome at a time while also building a histogram of the mean differences from all permutations generated so far. The treatment names are retained, but the observed times have been randomly reassigned. Pale orange undershading identifies values that originally came from the fed group."),
            shiny::p("The histogram is built from all permutations generated so far. The goal is to understand what kinds of mean differences are plausible under the null hypothesis. The red bars show permuted mean differences that are less than or equal to the observed study difference of -18.26 hours.")
          )
        ),

        shiny::tabPanel(
          "Original data",
          shiny::br(),
          shiny::div(
            class = "original-slide-layout",

            shiny::div(
              class = "original-slide-left",

              shiny::div(
                class = "lecture-table-title",
                shiny::HTML(
                  "Times to mating (in hours) of female sagebrush crickets that were recently starved or fed. Data from the two treatments are color-coded to more easily identify the origin of each value later in the <span class='lecture-link'>Permutation</span> tab."
                )
              ),

              shiny::uiOutput("original_slide_table_only"),

              shiny::div(
                class = "lecture-equation",
                shiny::HTML(
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

            shiny::div(
              class = "original-slide-right",
              shiny::plotOutput("original_histograms", height = "540px")
            )
          )
        ),

        shiny::tabPanel(
          "Permutation",
          shiny::br(),
          shiny::div(
            class = "permutation-layout",

            shiny::div(
              class = "permutation-left",

              shiny::div(class = "permutation-subtitle", "Most recent permutation"),

              shiny::p(
                class = "permutation-note",
                shiny::HTML(
                  "If more than one permutation is performed using the buttons to the left, <strong>only the most recent permutation is shown</strong>."
                )
              ),

              shiny::uiOutput("current_permutation_slide_table"),

              shiny::uiOutput("current_permutation_equation")
            ),

            shiny::div(
              class = "permutation-right",

              shiny::div(class = "permutation-subtitle", "The null distribution"),

              shiny::div(
                class = "permutation-histogram-text",
                shiny::HTML(
                  "Each permutation contributes one value of Ȳ<sub>1</sub> − Ȳ<sub>2</sub> to this histogram."
                )
              ),

              shiny::plotOutput("null_histogram", height = "420px"),

              shiny::uiOutput("histogram_explanation")
            )
          )
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
    " at the University of British Columbia, using resources associated with ",
    shiny::tags$a(
      href = "https://whitlockschluter3e.zoology.ubc.ca/index.html",
      target = "_blank",
      rel = "noopener noreferrer",
      "The Analysis of Biological Data"
    ),
    " by Whitlock & Schluter."
  )
)

server <- function(input, output, session) {

  permutation_history <- shiny::reactiveVal(tibble::tibble(permutation_id = integer(),
                                            permuted_difference = numeric()))

  current_permutation <- shiny::reactiveVal(NULL)

  current_permutation_summary <- shiny::reactiveVal(NULL)

  add_permutations <- function(n) {
    old_history <- permutation_history()
    old_n <- nrow(old_history)

    new_results <- purrr::map(seq_len(n), function(i) {
      result <- permute_once(cricket_data)

      if (i == n) {
        current_permutation(result$permuted_data)
        current_permutation_summary(
          tibble::tibble(
            randomized_starved_mean = result$randomized_starved_mean,
            randomized_fed_mean = result$randomized_fed_mean,
            permuted_difference = result$permuted_difference
          )
        )
      }

      tibble::tibble(
        permutation_id = old_n + i,
        permuted_difference = result$permuted_difference
      )
    }) |>
      dplyr::bind_rows()

    permutation_history(dplyr::bind_rows(old_history, new_results))
  }

  shiny::observeEvent(input$permute_once, {
    add_permutations(1)
  })

  shiny::observeEvent(input$permute_10, {
    add_permutations(10)
  })

  shiny::observeEvent(input$permute_100, {
    add_permutations(100)
  })

  shiny::observeEvent(input$permute_1000, {
    add_permutations(1000)
  })

  shiny::observeEvent(input$reset, {
    permutation_history(tibble::tibble(permutation_id = integer(),
                               permuted_difference = numeric()))
    current_permutation(NULL)
    current_permutation_summary(NULL)
  })

  output$original_slide_table_only <- shiny::renderUI({
    starved_data <- cricket_data |>
      dplyr::filter(original_group == "Starved") |>
      dplyr::arrange(time_hours)

    fed_data <- cricket_data |>
      dplyr::filter(original_group == "Fed") |>
      dplyr::arrange(time_hours)

    starved_rows <- lapply(seq_len(nrow(starved_data)), function(i) {
      shiny::tags$tr(
        class = "original-starved",
        shiny::tags$td("Starved"),
        shiny::tags$td(starved_data$time_hours[i])
      )
    })

    fed_rows <- lapply(seq_len(nrow(fed_data)), function(i) {
      shiny::tags$tr(
        class = "original-fed",
        shiny::tags$td("Fed"),
        shiny::tags$td(fed_data$time_hours[i])
      )
    })

    ## Add blank rows to the shorter table so the two means align visually,
    ## as in the lecture slide.
    starved_rows <- c(
      starved_rows,
      list(
        shiny::tags$tr(class = "blank-row", shiny::tags$td(""), shiny::tags$td("")),
        shiny::tags$tr(class = "blank-row", shiny::tags$td(""), shiny::tags$td(""))
      )
    )

    shiny::div(
      class = "original-slide-tables",

      shiny::tags$table(
        class = "original-lecture-table",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Treatment"),
            shiny::tags$th("Time (hours)")
          )
        ),
        shiny::tags$tbody(starved_rows),
        shiny::tags$tfoot(
          shiny::tags$tr(
            shiny::tags$td("Mean"),
            shiny::tags$td(round(observed_starved_mean, 2))
          )
        )
      ),

      shiny::tags$table(
        class = "original-lecture-table",
        shiny::tags$thead(
          shiny::tags$tr(
            shiny::tags$th("Treatment"),
            shiny::tags$th("Time (hours)")
          )
        ),
        shiny::tags$tbody(fed_rows),
        shiny::tags$tfoot(
          shiny::tags$tr(
            shiny::tags$td("Mean"),
            shiny::tags$td(round(observed_fed_mean, 2))
          )
        )
      )
    )
  })

  output$original_histograms <- shiny::renderPlot({
    cricket_data |>
      ggplot2::ggplot(ggplot2::aes(x = time_hours)) +
      ggplot2::geom_histogram(
        binwidth = 20,
        boundary = 0,
        closed = "left",
        fill = "#d20a11",
        color = "#222222",
        linewidth = 0.7
      ) +
      ggplot2::facet_wrap(
        ggplot2::vars(original_group),
        ncol = 1,
        scales = "free_y",
        labeller = ggplot2::as_labeller(c(
          "Starved" = "Starved females",
          "Fed" = "Fed females"
        ))
      ) +
      ggplot2::scale_x_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, 8, 2),
        expand = ggplot2::expansion(mult = c(0, 0.12))
      ) +
      ggplot2::labs(
        x = "Time to mating (hours)",
        y = "Frequency"
      ) +
      ggplot2::theme_classic(base_size = 18) +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(
          face = "italic",
          size = 19,
          hjust = 0.86,
          color = "#222222"
        ),
        axis.title = ggplot2::element_text(face = "bold", size = 19),
        axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 12)),
        axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
        axis.text = ggplot2::element_text(color = "#222222", size = 16),
        axis.line = ggplot2::element_line(color = "#222222", linewidth = 0.7),
        axis.ticks = ggplot2::element_line(color = "#222222", linewidth = 0.7),
        panel.spacing = ggplot2::unit(1.7, "lines"),
        plot.margin = ggplot2::margin(4, 18, 4, 4)
      )
  })

  output$current_permutation_slide_table <- shiny::renderUI({
    shiny::validate(
      shiny::need(!is.null(current_permutation()), "Click 'Permute once' or run a batch of permutations to display one randomized outcome.")
    )

    permutation_data <- current_permutation()

    starved_data <- make_slide_table(permutation_data, "Starved")
    fed_data <- make_slide_table(permutation_data, "Fed")

    starved_mean <- mean(starved_data$time_hours)
    fed_mean <- mean(fed_data$time_hours)

    shiny::tagList(
      shiny::div(
        class = "slide-table-wrapper",

        shiny::div(
          class = "slide-table-block",
          shiny::tags$table(
            class = "slide-table",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Treatment"),
                shiny::tags$th("Time (hrs)")
              )
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(starved_data)), function(i) {
                shiny::tags$tr(
                  class = starved_data$row_class[i],
                  shiny::tags$td("Starved"),
                  shiny::tags$td(starved_data$time_hours[i])
                )
              })
            ),
            shiny::tags$tfoot(
              shiny::tags$tr(
                shiny::tags$td("Mean"),
                shiny::tags$td(round(starved_mean, 2))
              )
            )
          )
        ),

        shiny::div(
          class = "slide-table-block",
          shiny::tags$table(
            class = "slide-table",
            shiny::tags$thead(
              shiny::tags$tr(
                shiny::tags$th("Treatment"),
                shiny::tags$th("Time (hrs)")
              )
            ),
            shiny::tags$tbody(
              lapply(seq_len(nrow(fed_data)), function(i) {
                shiny::tags$tr(
                  class = fed_data$row_class[i],
                  shiny::tags$td("Fed"),
                  shiny::tags$td(fed_data$time_hours[i])
                )
              })
            ),
            shiny::tags$tfoot(
              shiny::tags$tr(
                shiny::tags$td("Mean"),
                shiny::tags$td(round(fed_mean, 2))
              )
            )
          )
        )
      )
    )
  })

  output$current_permutation_equation <- shiny::renderUI({
    shiny::validate(
      shiny::need(!is.null(current_permutation_summary()), "")
    )

    summary <- current_permutation_summary()

    shiny::div(
      class = "equation-large",
      shiny::HTML(
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

  output$null_histogram <- shiny::renderPlot({
    history <- permutation_history()

    shiny::validate(
      shiny::need(nrow(history) > 0, "Click 'Permute once' or run a batch of permutations to begin building the null distribution.")
    )

    history <- history |>
      dplyr::mutate(
        lower_tail = permuted_difference <= observed_difference
      )

    ggplot2::ggplot(history, ggplot2::aes(x = permuted_difference)) +
      ggplot2::geom_histogram(
        data = dplyr::filter(history, !lower_tail),
        binwidth = 2,
        boundary = 0,
        fill = "white",
        color = "black"
      ) +
      ggplot2::geom_histogram(
        data = dplyr::filter(history, lower_tail),
        binwidth = 2,
        boundary = 0,
        fill = "#c92514",
        color = "black"
      ) +
      ggplot2::geom_vline(
        xintercept = observed_difference,
        linewidth = 1.1,
        color = "#555555"
      ) +
      ggplot2::annotate(
        "text",
        x = observed_difference,
        y = Inf,
        label = round(observed_difference, 2),
        vjust = 1.6,
        hjust = 1.1,
        size = 5,
        color = "#222222"
      ) +
      ggplot2::scale_x_continuous(
        limits = c(-50, 50),
        breaks = seq(-40, 40, 20),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        expand = ggplot2::expansion(mult = c(0, 0.1))
      ) +
      ggplot2::labs(
        x = "Difference in treatment means\nfrom randomized data (hours)",
        y = "Frequency"
      ) +
      ggplot2::theme_classic(base_size = 20) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(face = "bold"),
        # axis.title.x = element_text(
        #   size = 20,
        #   lineheight = 1.0,
        #   margin = margin(t = 10)
        # ),
        axis.text = ggplot2::element_text(color = "black"),
        plot.margin = ggplot2::margin(4, 6, 12, 6)
      )
  })

  output$n_permutations <- shiny::renderText({
    n <- nrow(permutation_history())
    paste0("Number of permutations generated: ", n)
  })

  output$n_lower_tail <- shiny::renderText({
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

  output$p_value_summary <- shiny::renderUI({
    history <- permutation_history()

    if (nrow(history) == 0) {
      return(shiny::tags$p("Two-tailed P-value: not yet available"))
    }

    lower_tail_count <- sum(history$permuted_difference <= observed_difference)
    lower_tail_proportion <- lower_tail_count / nrow(history)
    two_tailed_p <- 2 * lower_tail_proportion

    shiny::tags$div(
      shiny::tags$p(
        paste0(
          "Lower-tail proportion = ",
          lower_tail_count,
          " / ",
          nrow(history),
          " = ",
          round(lower_tail_proportion, 4)
        )
      ),
      shiny::tags$p(
        paste0(
          "Two-tailed P-value = 2 × ",
          round(lower_tail_proportion, 4),
          " = ",
          round(two_tailed_p, 4)
        )
      )
    )
  })

  output$histogram_explanation <- shiny::renderUI({
    history <- permutation_history()

    if (nrow(history) == 0) {
      return(shiny::tags$p("No permutations have been generated yet."))
    }

    lower_tail_count <- sum(history$permuted_difference <= observed_difference)
    lower_tail_proportion <- lower_tail_count / nrow(history)
    two_tailed_p <- 2 * lower_tail_proportion

    shiny::tags$div(
      class = "histogram-note",
      shiny::tags$p(
        paste0(
          lower_tail_count,
          " of ",
          nrow(history),
          " outcomes are less than or equal to the observed study difference of ",
          round(observed_difference, 2),
          " hours."
        )
      ),
      shiny::tags$p(
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

shiny::shinyApp(ui = ui, server = server)

library(shiny)
library(bslib)
library(tidyverse)

# Data --------------------------------------------------------------------
school <- collegeScorecard::college_load_tidy_school()
scorecard <- collegeScorecard::college_load_tidy_scorecard()

scorecard_recent <-
  scorecard |>
  filter(academic_year == max(academic_year)) |>
  select(id, n_undergrads, cost_avg, rate_admissions, rate_completion)

school <- left_join(school, scorecard_recent, by = "id")

range_slider <- function(data, column, label, by = 15000, step = by) {
  val_range <- range(data[[column]], na.rm = TRUE)
  val_range[1] <- floor(val_range[1] / by) * by
  val_range[2] <- ceiling(val_range[2] / by) * by

  sliderInput(
    inputId = column,
    label = label,
    value = val_range,
    min = val_range[1],
    max = val_range[2],
    step = step,
    ticks = FALSE
  )
}

# Inputs ------------------------------------------------------------------
input_var <-
  varSelectInput(
    inputId = "var",
    label = "School Variable",
    school |> select(where(\(x) length(unique(x)) < 60))
  )

input_n_undergrads <-
  range_slider(
    school,
    "n_undergrads",
    "Number of Undergrad Students",
    by = 15000,
    step = 5000
  )

input_cost_avg <-
  range_slider(
    school,
    "cost_avg",
    "Average Yearly Cost",
    by = 2500
  )

# UI ----------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(version = 5, preset = "shiny"),
  titlePanel("01 - Hello bslib!"),
  sidebarLayout(
    sidebarPanel(
      input_var,
      input_n_undergrads,
      input_cost_avg
    ),
    mainPanel(
      plotOutput("plot", height = 700)
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  output$plot <- renderPlot({
    school |>
      filter(
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
      ) |>
      ggplot() +
      aes(y = !!input$var) +
      geom_bar() +
      labs(title = input$var, y = NULL) +
      theme_minimal(16)
  })
}

shinyApp(ui, server)

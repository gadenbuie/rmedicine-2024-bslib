library(shiny)
library(bslib)
library(tidyverse)

# Data --------------------------------------------------------------------
source("00_read-data.R")

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


# UI ----------------------------------------------------------------------

ui <- page_fluid(
  theme = bs_theme(version = 5, preset = "shiny"),
  titlePanel("01 - Hello bslib!"),
  sidebarLayout(
    sidebarPanel(
      varSelectInput(
        inputId = "var",
        label = "School Variable",
        school |> select(where(\(x) length(unique(x)) < 60))
      ),
      range_slider(
        school,
        "n_undergrads",
        "Number of Undergrad Students",
        by = 15000,
        step = 5000
      ),
      range_slider(
        school,
        "cost_avg",
        "Average Yearly Cost",
        by = 2500
      )
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

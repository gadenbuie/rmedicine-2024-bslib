library(dplyr)
library(plotly)
library(collegeScorecard)

# Data --------------------------------------------------------------------
scorecard_recent <-
  scorecard |>
  filter(academic_year == max(academic_year)) |>
  select(id, n_undergrads, cost_avg, rate_admissions, rate_completion)

school <- left_join(school, scorecard_recent, by = "id")

school_types <- levels(school$control)
school_degrees <- levels(school$deg_predominant) |> setdiff("Graduate")
school_locales <- levels(school$locale_type)

# Inputs ------------------------------------------------------------------
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

input_school_type <-
  checkboxGroupInput(
    "school_type",
    "Type of School",
    choices = school_types,
    selected = school_types,
    inline = FALSE
  )

input_deg_predmoninant <-
  selectInput(
    "deg_predominant",
    "Predominant Degree Type",
    choices = school_degrees,
    selected = "Bachelor"
  )


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  r_scorecard <- reactive({
    school_filter <-
      school |>
      filter(
        between(n_undergrads, input$n_undergrads[1], input$n_undergrads[2]),
        between(cost_avg, input$cost_avg[1], input$cost_avg[2])
      )

    if (length(input$school_type)) {
      school_filter <-
        school_filter |>
        filter(control %in% input$school_type)
    }

    school_filter <-
      school_filter |>
      filter(deg_predominant == input$deg_predominant)

    scorecard |>
      semi_join(school_filter, by = "id")
  })

  plotly_cleaner <- function(x) {
    x |>
      config(displayModeBar = FALSE) |>
      layout(margin = list(l = 0, r = 0, b = 0))
  }

  filter_recent_complete_year <- function(scorecard, column) {
    academic_year <- scorecard[!is.na(scorecard[[column]]), ]$academic_year
    scorecard |> filter(academic_year == !!max(academic_year))
  }

  output$plot_rate_admissions <- renderPlotly({
    r_scorecard() |>
      filter_recent_complete_year("rate_admissions") |>
      filter(!is.na(rate_admissions)) |>
      plot_ly(x = ~rate_admissions, type = "histogram") |>
      layout(xaxis = list(title = "Rate")) |>
      plotly_cleaner()
  })

  output$plot_rate_completion <- renderPlotly({
    r_scorecard() |>
      filter_recent_complete_year("rate_completion") |>
      filter(rate_completion > 0) |>
      plot_ly(x = ~rate_completion, type = "histogram") |>
      layout(xaxis = list(title = "Rate")) |>
      plotly_cleaner()
  })
}

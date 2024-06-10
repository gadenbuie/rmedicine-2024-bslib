function(input, output, session) {
  r_scorecard <- reactive({
    school_filter <- school

    school_filter <- school_filter[
      school_filter$deg_predominant == input$deg_predominant,
    ]

    scorecard[scorecard$id %in% school_filter$id, ]
  })

  plotly_cleaner <- function(x) {
    x |>
      config(displayModeBar = FALSE) |>
      layout(margin = list(l = 0, r = 0, b = 0))
  }

  filter_recent_complete_year <- function(scorecard, column) {
    academic_year <- scorecard[!is.na(scorecard[[column]]), ]$academic_year
    scorecard[scorecard$academic_year == max(academic_year), ]
  }

  output$plot_rate_admissions <- renderPlotly({
    r_scorecard() |>
      filter_recent_complete_year("rate_admissions") |>
      (\(x) x[!is.na(x$rate_admissions), ])() |>
      plot_ly(x = ~rate_admissions, type = "histogram") |>
      layout(xaxis = list(title = "Rate")) |>
      plotly_cleaner()
  })
}
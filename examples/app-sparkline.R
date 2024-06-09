library(shiny)
library(bslib)
library(plotly)
library(tidyverse)
library(collegeScorecard)

# Directly from bslib docs (Components > Value Boxes > Expandable Sparklines)
# https://rstudio.github.io/bslib/articles/value-boxes/index.html#expandable-sparklines
plotly_sparkline <- function(data, x, y, ..., color = "white", formatter = identity) {
  plot_ly(data) %>%
    add_lines(
      x = x,
      y = y,
      text = as.formula(sprintf("~ formatter(%s)", as.character(y[[2]]))),
      hoverinfo = "text",
      color = I(color),
      span = I(1),
      fill = "tozeroy",
      alpha = 0.2
    ) %>%
    layout(
      xaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
      yaxis = list(visible = FALSE, showgrid = FALSE, title = ""),
      hovermode = "x",
      margin = list(t = 0, r = 0, l = 0, b = 0),
      font = list(color = color),
      paper_bgcolor = "transparent",
      plot_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = FALSE) %>%
    htmlwidgets::onRender(
      "function(el) {
        el.closest('.bslib-value-box')
          .addEventListener('bslib.card', function(ev) {
            Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
          })
      }"
    )
}

# Data --------------------------------------------------------------------
school_1k <-
  scorecard |>
  slice_max(academic_year, by = id, n = 1) |>
  filter(n_undergrads > 1000) |>
  semi_join(school, y = _, by = "id")

# UI ----------------------------------------------------------------------
ui <- page_sidebar(
  title = "Value Boxes with Sparklines",
  sidebar = sidebar(
    selectInput("name", "School Name", NULL),
    actionButton("random_school", "Random School", fontawesome::fa_i("shuffle"))
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Undergrad Students",
      value = textOutput("text_n_undergrads"),
      showcase = plotlyOutput("sparkline_n_undergrads"),
      showcase_layout = "bottom",
      full_screen = TRUE
    ),
    value_box(
      title = "Average Yearly Cost",
      value = textOutput("text_cost_avg"),
      theme = "primary",
      showcase = plotlyOutput("sparkline_cost_avg"),
      showcase_layout = "bottom",
      full_screen = TRUE
    ),
    value_box(
      title = "Completion Rate",
      value = textOutput("text_rate_completion"),
      theme = "bg-gradient-orange-red",
      showcase = plotlyOutput("sparkline_rate_completion"),
      showcase_layout = "bottom",
      full_screen = TRUE
    )
  ),

  # pretend this card is a plot
  card(class = "text-bg-light", style = "min-height: 300px")
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  observe({
    updateSelectizeInput(session, "name", choices = school_1k$name, server = TRUE)
  })

  observeEvent(input$random_school, {
    updateSelectizeInput(session, "name", choices = school_1k$name, selected = sample(school_1k$name, 1), server = TRUE)
  })

  r_scorecard <- eventReactive(input$name, {
    scorecard |>
      semi_join(school_1k |> filter(name == input$name), by = "id") |>
      arrange(academic_year)
  })

  output$text_n_undergrads <- renderText({
    n_undergrads <- r_scorecard()$n_undergrads |> tail(1)
    validate(need(n_undergrads, "No data"))
    scales::number(n_undergrads, big.mark = ",")
  })

  output$sparkline_n_undergrads <- renderPlotly({
    plotly_sparkline(
      r_scorecard(),
      x = ~academic_year,
      y = ~n_undergrads,
      color = "black",
      formatter = scales::label_number(big.mark = ",")
    )
  })

  output$text_cost_avg <- renderText({
    cost_avg <- r_scorecard()$cost_avg |> tail(1)
    validate(need(cost_avg, "No data"))
    scales::dollar(cost_avg)
  })

  output$sparkline_cost_avg <- renderPlotly({
    plotly_sparkline(
      r_scorecard(),
      x = ~academic_year,
      y = ~cost_avg,
      formatter = scales::label_dollar(accuracy = 10)
    )
  })

  output$text_rate_completion <- renderText({
    rate_completion <- r_scorecard()$rate_completion |> tail(1)
    validate(need(rate_completion, "No data"))
    scales::percent(rate_completion)
  })

  output$sparkline_rate_completion <- renderPlotly({
    plotly_sparkline(
      r_scorecard(),
      x = ~academic_year,
      y = ~rate_completion,
      formatter = scales::label_percent(accuracy = 0.1)
    )
  })
}

shinyApp(ui, server)

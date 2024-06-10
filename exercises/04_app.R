library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(tidyverse)
library(collegeScorecard)


# Data --------------------------------------------------------------------
school_1k <-
  scorecard |>
  slice_max(academic_year, by = id, n = 1) |>
  filter(n_undergrads > 1000) |>
  semi_join(school, y = _, by = "id") |>
  filter(!is.na(latitude))

# UI ----------------------------------------------------------------------
ui <- page_sidebar(
  title = "04 - Column Layouts",
  sidebar = sidebar(
    selectInput("name", "School Name", NULL),
    actionButton("random_school", "Random School", fontawesome::fa_i("shuffle"))
  ),
  value_box(
    title = "Undergrad Students",
    value = textOutput("text_n_undergrads"),
    showcase = fontawesome::fa_i("people-roof")
  ),
  value_box(
    title = "Average Yearly Cost",
    value = textOutput("text_cost_avg"),
    theme = "primary",
    showcase = fontawesome::fa_i("money-check-dollar")
  ),
  value_box(
    title = "Completion Rate",
    value = textOutput("text_rate_completion"),
    theme = "bg-gradient-orange-red",
    showcase = fontawesome::fa_i("user-graduate")
  ),

  plotlyOutput("plot_cost_by_income"),
  leafletOutput("map_school")
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  # School Selection ----
  observe({
    updateSelectizeInput(session, "name", choices = school_1k$name, server = TRUE)
  })

  observeEvent(input$random_school, {
    updateSelectizeInput(session, "name", choices = school_1k$name, selected = sample(school_1k$name, 1), server = TRUE)
  })

  r_scorecard <- eventReactive(input$name, {
    scorecard |>
      inner_join(school_1k |> filter(name == input$name), by = "id") |>
      slice_max(academic_year, n = 1, with_ties = FALSE)
  })

  # Value box text ----
  output$text_n_undergrads <- renderText({
    n_undergrads <- r_scorecard()$n_undergrads
    validate(need(n_undergrads, "No data"))
    scales::number(n_undergrads, big.mark = ",")
  })

  output$text_cost_avg <- renderText({
    cost_avg <- r_scorecard()$cost_avg
    validate(need(cost_avg, "No data"))
    scales::dollar(cost_avg)
  })

  output$text_rate_completion <- renderText({
    rate_completion <- r_scorecard()$rate_completion
    validate(need(rate_completion, "No data"))
    scales::percent(rate_completion)
  })

  # Plot: Cost by income ----
  output$plot_cost_by_income <- renderPlotly({
    income_level <- c(
      "0_30k" = "$0 - $30k",
      "30_48k" = "$30k - $48k",
      "48_75k" = "$48k - $75k",
      "75_110k" = "$75k - $110k",
      "110k_plus" = "$110k+"
    )

    r_scorecard() |>
      slice_max(academic_year, n = 1) |>
      select(starts_with("cost_avg_income")) |>
      pivot_longer(everything(), names_prefix = "cost_avg_income_") |>
      mutate(name = factor(name, names(income_level), income_level)) |>
      plot_ly(
        x = ~value,
        y = ~name,
        type = "bar",
        hoverinfo = "text",
        textposition = "none",
        text = ~paste0(
          "Income Level: ", name, "<br>",
          "Average Cost: ", scales::dollar(value)
        )
      ) |>
      layout(
        xaxis = list(title = "Average Yearly Cost"),
        yaxis = list(title = "Income Level")
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # Map: School location ----
  output$map_school <- renderLeaflet({
    validate(need(r_scorecard()$latitude, "No location data"))

    r_scorecard() |>
      leaflet() |>
      addTiles() |>
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~name
      )
  })
}

shinyApp(ui, server)

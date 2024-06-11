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
ui <- page_navbar(
  title = "05 - Fillable Layouts",
  sidebar = sidebar(
    selectInput("name", "School Name", NULL),
    actionButton("random_school", "Random School", fontawesome::fa_i("shuffle"))
  ),
  nav_panel(
    "Plots",
    layout_column_wrap(
      width = 1 / 3,
      card(
        card_header("Undergraduate Students by Academic Year"),
        plotlyOutput("plot_n_undergrads")
      ),
      card(
        card_header("Average Yearly Cost"),
        plotlyOutput("plot_cost_avg")
      ),
      card(
        card_header("Completion Rate"),
        plotlyOutput("plot_rate_completion")
      )
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(
        card_header("Cost by Income"),
        plotlyOutput("plot_cost_by_income")
      ),
      card(
        class = "text-bg-secondary",
        card_header("School Location"),
        card_body(
          padding = 0,
          leafletOutput("map_school")
        )
      )
    )
  ),
  nav_panel(
    "Info",
    card(
      card_header("School Information"),
      tableOutput("table_school")
    )
  )
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
      mutate(
        year = sub("-.+", "", academic_year),
        year = as.integer(year)
      )
  })

  validate_not_all_missing <- function(x, column) {
    validate(need(any(!is.na(x[[column]])), "No data"))
  }

  # Plot: Undergrad Students by Academic Year ----
  output$plot_n_undergrads <- renderPlotly({
    validate_not_all_missing(r_scorecard(), "n_undergrads")
    r_scorecard() |>
      plot_ly(
        x = ~year,
        y = ~n_undergrads,
        type = "scatter",
        mode = "lines+markers",
        hoverinfo = "text",
        text = ~ paste0(
          "Undergrad Students: ",
          scales::number(n_undergrads, big.mark = ",")
        )
      ) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "")
      ) |>
      config(displayModeBar = FALSE)
  })

  # Plot: Average Yearly Cost ----
  output$plot_cost_avg <- renderPlotly({
    validate_not_all_missing(r_scorecard(), "cost_avg")
    r_scorecard() |>
      plot_ly(
        x = ~year,
        y = ~cost_avg,
        type = "scatter",
        mode = "lines+markers",
        hoverinfo = "text",
        text = ~ paste0(
          "Average Yearly Cost: ",
          scales::dollar(cost_avg)
        )
      ) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "")
      ) |>
        config(displayModeBar = FALSE)
  })

  output$plot_rate_completion <- renderPlotly({
    validate_not_all_missing(r_scorecard(), "rate_completion")

    r_scorecard() |>
      plot_ly(
        x = ~year,
        y = ~rate_completion,
        type = "scatter",
        mode = "lines+markers",
        hoverinfo = "text",
        text = ~ paste0(
          "Completion Rate: ",
          scales::percent(rate_completion)
        )
      ) |>
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "")
      ) |>
      config(displayModeBar = FALSE)
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
        text = ~ paste0(
          "Income Level: ",
          name,
          "<br>",
          "Average Cost: ",
          scales::dollar(value)
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

  # Table: Selected School Information ----
  output$table_school <- renderTable({
    req(input$name)

    school_1k |>
      filter(name == input$name) |>
      select(-id) |>
      mutate(
        across(starts_with("rate"), scales::percent),
        across(where(is.logical), \(x) ifelse(x, "Yes", "No")),
        across(where(is.numeric), \(x) scales::number(x, accuracy = 0.01))
      ) |>
      pivot_longer(everything(), names_to = "Variable", values_to = "Value") |>
      mutate(
        Variable = str_replace_all(Variable, "_", " "),
        Variable = str_to_title(Variable)
      ) |>
      filter(!is.na(Value))
  })
}

shinyApp(ui, server)

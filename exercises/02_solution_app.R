library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(collegeScorecard)

# Data --------------------------------------------------------------------
school_types <- levels(school$control)
school_degrees <- levels(school$deg_predominant) |> setdiff("Graduate")
school_locales <- levels(school$locale_type)

# Inputs ------------------------------------------------------------------
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

input_group_by <-
  radioButtons(
    "group_by",
    "Group By",
    choices = c(
      "Campus Setting" = "locale_type",
      "Highest Degree" = "deg_highest",
      "Testing Requirements" = "adm_req_test"
    )
  )

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "02 - First bslib Layout",
  sidebar = sidebar(
    input_school_type,
    input_deg_predmoninant,
  ),
  layout_columns(
    card(
      card_header("Admissions Rate"),
      plotlyOutput("plot_rate_admissions")
    ),
    card(
      card_header("Completion Rate"),
      plotlyOutput("plot_rate_completion")
    )
  ),
  card(
    card_header("Cost vs Earnings"),
    layout_sidebar(
      sidebar = sidebar(
        input_group_by
      ),
      plotlyOutput("plot_cost_earnings")
    ),
    full_screen = TRUE
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  r_scorecard <- reactive({
    school_filter <- school
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

  output$plot_cost_earnings <- renderPlotly({
    r_scorecard() |>
      filter_recent_complete_year("amnt_earnings_med_10y") |>
      left_join(school, by = "id") |>
      plot_ly(
        x = ~cost_avg,
        y = ~amnt_earnings_med_10y,
        color = ~get(input$group_by),
        hoverinfo = "text",
        text = ~paste0(
          "School: ", name, "<br>",
          "Undergrads: ", scales::number(n_undergrads, big.mark=","), "<br>",
          "Average Cost: ", scales::dollar(cost_avg), "<br>",
          "Median Earnigns: ", scales::dollar(amnt_earnings_med_10y), "<br>",
          "Admissions Rate: ", scales::percent(rate_admissions), "<br>",
          "Completion Rate: ", scales::percent(rate_completion)
        )
      ) |>
      layout(
        xaxis = list(title = "Average Cost"),
        yaxis = list(title = "Median Earnings")
      )
  })
}

shinyApp(ui, server)

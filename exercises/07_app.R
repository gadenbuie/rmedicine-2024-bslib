library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(collegeScorecard)

# Inputs ------------------------------------------------------------------
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

## Instructions:
## Use a `popover()` to hide the radio buttons until the user is ready for them.
## You can use `bsicon::bs_icon()` for the icon. Check out
## https://icons.getbootstrap.com/ for icon inspiration.

ui <- page_fillable(
  title = "06 - Navset Cards",
  input_group_by,
  card(
    card_header("Cost vs Earnings"),
    plotlyOutput("plot_cost_earnings")
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  filter_recent_complete_year <- function(scorecard, column) {
    academic_year <- scorecard[!is.na(scorecard[[column]]), ]$academic_year
    scorecard |> filter(academic_year == !!max(academic_year))
  }

  plotly_cost_earnings <- function(group_by) {
    scorecard |>
      filter_recent_complete_year("amnt_earnings_med_10y") |>
      left_join(school, by = "id") |>
        plot_ly(
          x = ~cost_avg,
          y = ~amnt_earnings_med_10y,
          color = ~get(group_by),
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
          yaxis = list(title = "Median Earnings"),
          margin = list(l = 0, r = 0, b = 0)
        ) |>
      config(displayModeBar = FALSE)
  }

  output$plot_cost_earnings <- renderPlotly({
    plotly_cost_earnings(input$group_by)
  })
}

shinyApp(ui, server)

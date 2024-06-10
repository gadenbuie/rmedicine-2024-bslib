function(input, output, session) {
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

  output$plot_cost_earnings_by_locale_type <- renderPlotly({
    plotly_cost_earnings("locale_type")
  })

  output$plot_cost_earnings_by_deg_highest <- renderPlotly({
    plotly_cost_earnings("deg_highest")
  })

  output$plot_cost_earnings_by_adm_req_test <- renderPlotly({
    plotly_cost_earnings("adm_req_test")
  })
}
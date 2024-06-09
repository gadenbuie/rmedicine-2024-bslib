library(shiny)
library(bslib)
library(tidyverse)
library(collegeScorecard)

# Feel free to use either of these packages for icons!
# library(fontawesome)  # usage: fontawesome::fa_i("icon-name")
# library(bsicons)      # usage: bsicons::bs_icon("icon-name")

# Data --------------------------------------------------------------------
school_1k <-
  scorecard |>
  slice_max(academic_year, by = id, n = 1) |>
  filter(n_undergrads > 1000) |>
  semi_join(school, y = _, by = "id")

# UI ----------------------------------------------------------------------
ui <- page_sidebar(
  title = "03 - Value Boxes",
  sidebar = sidebar(
    selectInput("name", "School Name", NULL),
    actionButton("random_school", "Random School", fontawesome::fa_i("shuffle"))
  ),

  layout_columns(
    fill = FALSE,
    # !! Put value boxes here !!
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
      slice_max(academic_year, n = 1, with_ties = FALSE)
  })

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
}

shinyApp(ui, server)
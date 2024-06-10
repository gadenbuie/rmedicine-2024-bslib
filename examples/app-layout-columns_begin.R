library(shiny)
library(bslib)
library(glue)
library(tidyverse)
library(collegeScorecard)

ui <- page_fillable(
  sliderInput("n", "Top N Schools", min = 1, max = 20, value = 9, ticks = FALSE),
  uiOutput("layout_cards")
)

server <- function(input, output, session) {
  output$layout_cards <- renderUI({
    # layout cards here
  })

  top_n_schools <- reactive({
    scorecard |>
      filter(n_undergrads > 1000) |>
      slice_max(academic_year, n = 1) |>
      slice_max(cost_avg, n = input$n) |>
      arrange(desc(cost_avg)) |>
      left_join(school, by = "id")
  })

  colors <- c("blue", "indigo", "purple", "pink", "red", "orange", "yellow", "green", "teal", "cyan")

  cards <- reactive({
    req(top_n_schools())
    set.seed(42 ** 3.8)

    ## Create a value box for each of the top-N most costly schools
    # value_box(
    #   title = name,
    #   value = scales::dollar(cost_avg),
    #   theme = sample(colors, 1)
    # )
  })

}

shinyApp(ui, server)
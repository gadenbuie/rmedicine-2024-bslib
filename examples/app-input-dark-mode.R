library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(collegeScorecard)

thematic::thematic_on()

ui <- page_sidebar(
  title = "Dark mode demo",
  sidebar = sidebar(
    checkboxGroupInput("school_type", "School type", levels(school$control)),
    sliderInput("cost_max", "Max cost", min = 0, max = 70000, value = 70000, step = 500, ticks = FALSE),
    actionButton("button", "Do Something"),
    input_dark_mode()
  ),
  card(
    card_header("School Size"),
    plotOutput("plot_school_size")
  )
)

server <- function(input, output, session) {
  schools <- reactive({
    schools <- scorecard |>
      slice_max(academic_year, by = id, n = 1) |>
      filter(cost_avg <= input$cost_max) |>
      inner_join(school, y = _, by = "id")

    if (length(input$school_type)) {
      schools <- schools |> filter(control %in% input$school_type)
    }

    schools
  })

  output$plot_school_size <- renderPlot({
    ggplot(schools()) +
      aes(x = n_undergrads) +
      geom_histogram(bins = 10) +
      theme_minimal(18)
  })
}

shinyApp(ui, server)
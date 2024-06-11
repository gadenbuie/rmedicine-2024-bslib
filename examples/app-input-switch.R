library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(collegeScorecard)

ui <- page_fillable(
  h2("Input Switch Example"),
  layout_column_wrap(
    width = 1 / 2,
    fill = FALSE,
    div(
      checkboxInput("most_get_in", "Most People Get In", TRUE),
      checkboxInput("most_graduate", "Most People Graduate", TRUE),
      checkboxInput("require_test", "Requires Test Scores", TRUE),
    ),
    div(
      checkboxInput("want_certificate", "I Want a Certificate", FALSE),
      checkboxInput("want_associates", "I Want an Associate's Degree", FALSE),
      checkboxInput("want_bachelors", "I Want a Bachelor's Degree", FALSE)
    )
  ),
  card(
    card_body(
      padding = 0,
      leafletOutput("map")
    )
  )
)

server <- function(input, output, session) {
  schools <- reactive({
    sc_filter <-
      scorecard |>
      slice_max(academic_year, by = id, n = 1)

    if (input$most_get_in) {
      sc_filter <- sc_filter |> filter(rate_admissions > 0.5)
    }

    if (input$most_graduate) {
      sc_filter <- sc_filter |> filter(rate_completion > 0.5)
    }

    school_filter <- school |>
      filter(
        between(latitude, 24.4, 49.4),
        between(longitude, -125.0, -67.0),
      )

    if (input$require_test) {
      school_filter <- school_filter |> filter(adm_req_test %in% c("Required", "Recommended"))
    }
    deg_want <- c(
      if (input$want_certificate) "Certificate",
      if (input$want_associates) "Associate",
      if (input$want_bachelors) "Bachelor"
    )
    if (length(deg_want)) {
      school_filter <- school_filter |> filter(deg_predominant %in% deg_want)
    }

    if (nrow(school_filter) > 500) {
      school_filter <- school_filter |> slice_sample(n = 500)
    }

    school_filter
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      addMarkers(
        data = schools(),
        lat = ~latitude,
        lng = ~longitude,
        popup = ~name
      )
  })
}

shinyApp(ui, server)
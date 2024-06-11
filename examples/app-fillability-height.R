library(shiny)
library(bslib)
library(leaflet)
library(collegeScorecard)

lehigh <- school[school$name == "Lehigh University", ]

card_basic <- function(..., class = NULL, height = NULL) {
  div(
    class = "card", class = class,
    style = htmltools::css(height = height),
    div(
      class = "card-body",
      ...
    )
  )
}

ui <- page_fixed(
  card_basic(
    class = "text-bg-dark",
    leafletOutput("map_lehigh", height = "300px")
  )
)

server <- function(input, output, session) {
  output$map_lehigh <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = lehigh$longitude, lat = lehigh$latitude, zoom = 13) |>
      addMarkers(lng = lehigh$longitude, lat = lehigh$latitude, popup = lehigh$name)
  })
}

shinyApp(ui, server)

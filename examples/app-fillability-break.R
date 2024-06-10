library(shiny)
library(bslib)
library(leaflet)
library(collegeScorecard)
source("lehigh.R")

lehigh <- school[school$name == "Lehigh University", ]

ui <- page_fillable(
  card(
    class = "text-bg-light",
    card_body(
      h3("Lehigh University"),
      p_lehigh_1, p_lehigh_2, p_lehigh_3,
      actionButton("visit", "Visit School"),
      leafletOutput("map_lehigh"),
    )
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
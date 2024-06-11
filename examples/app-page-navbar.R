library(shiny)
library(bslib)

ui <- page_navbar(
  nav_panel(
    "One",
    layout_columns(
      card(class = "text-bg-primary", "A"),
      card(class = "text-bg-secondary", "B"),
    ),
    card(class = "bg-teal", "C")
  ),
  nav_panel(
    "Two",
    card(class = "bg-orange text-light", "D")
  )
)

shinyApp(ui, \(...) { })

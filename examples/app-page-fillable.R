library(shiny)
library(bslib)

ui <- page_fillable(
  layout_columns(
    card(class = "text-bg-primary", "A"),
    card(class = "text-bg-secondary", "B"),
  ),
  card(class = "bg-teal", "C"),
  card(class = "bg-orange text-light", "D")
)

shinyApp(ui, \(...) { })

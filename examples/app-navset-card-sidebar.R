library(shiny)
library(bslib)

ui <- page_fillable(
  navset_card_tab(
    sidebar = "global",
    selected = "Second tab name",
    nav_panel(
      "First tab name",
      "... first tab content"
    ),
    nav_panel(
      "Second tab name",
      class = "p-0",
      padding = 0,
      layout_sidebar(
        sidebar = sidebar("local", position = "right"),
        class = "m-0",
        border = TRUE,
        "... second tab content"
      )
    )
  )
)

shinyApp(ui, \(...) { })

library(shiny)
library(bslib)

source("app-accordion-support.R", local = TRUE)

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  title = "Accordion Example",
  sidebar = sidebar(
    input_school_type,
    input_deg_predmoninant,
    input_n_undergrads,
    input_cost_avg
  ),
  layout_columns(
    card(
      card_header("Admissions Rate"),
      plotlyOutput("plot_rate_admissions")
    ),
    card(
      card_header("Completion Rate"),
      plotlyOutput("plot_rate_completion")
    )
  )
)

shinyApp(ui, server)

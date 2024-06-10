library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(collegeScorecard)


# UI ----------------------------------------------------------------------

ui <- page_fillable(
  title = "06 - Navset Cards",
  navset_card_tab(
    nav_panel(
      "Campus Setting",
      plotlyOutput("plot_cost_earnings_by_locale_type")
    ),
    nav_panel(
      "Highest Degree",
      plotlyOutput("plot_cost_earnings_by_deg_highest")
    ),
    nav_panel(
      "Testing Requirements",
      plotlyOutput("plot_cost_earnings_by_adm_req_test")
    )
  )
)


shinyApp(ui, source("06_server.R")$value)

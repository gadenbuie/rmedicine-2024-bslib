library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(collegeScorecard)


# UI ----------------------------------------------------------------------

ui <- page_fillable(
  title = "06 - Navset Cards",
  "Campus Setting",
  plotlyOutput("plot_cost_earnings_by_locale_type"),
  "Highest Degree",
  plotlyOutput("plot_cost_earnings_by_deg_highest"),
  "Testing Requirements",
  plotlyOutput("plot_cost_earnings_by_adm_req_test")
)


shinyApp(ui, source("06_server.R")$value)

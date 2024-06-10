library(shiny)
library(bslib)
library(plotly)
library(collegeScorecard)

school_degrees <- levels(school$deg_predominant)
school_degrees <- setdiff(school_degrees, "Graduate")

input_deg_predmoninant <-
  selectInput(
    "deg_predominant",
    "Predominant Degree Type",
    choices = school_degrees,
    selected = "Bachelor"
  )


page_fillable(
  input_deg_predmoninant,
  plotlyOutput("plot_rate_admissions")
)

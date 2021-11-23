#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Size-selective fishing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("u",
                   "Exploitation Rate:",
                   min = 0,
                   max = 1,
                   value = 0.0,
                   step = 0.025),
       radioButtons("fishing.method", "Fish Method",
                    c(
                      "Fish Aggregating Device" =  "fad",
                      "School Sets" = "school",
                      "Dolphin Sets" = "dolphin",
                      "Longline" = "longline")),
       radioButtons("rec.type", "Recruitment",
                    c(
                      "Constant" = "const",
                      "Spawning biomass-dependent" = "var"
                    ))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Catch and Biomass by Age", plotOutput("age_eq_plot")),
        tabPanel("Equilibrium Summary", tableOutput("eq_table")),
        tabPanel("Recruitment", plotOutput("rec_plot"))
       # tabPanel("Catch vs. Exploitation", plotOutput("catch_plot"))
      )
    )
  )
))


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
    titlePanel("Fishing Simulator"),
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "catch",
          "Catch Level:",
          min = 1,
          max = 20,
          value = 10
        ),
        sliderInput(
          "n.start",
          "Starting Population Size:",
          min = 1,
          max = 100,
          value = 50
        ),
        radioButtons(
          "prodtype",
          "Production Type",
          c(
            "Constant Per-Capita Production"  =  "const",
            "Compensatory Per-Capita Production" = "compensatory"
          )
        )
      ),
      mainPanel(plotOutput(outputId = "simplot"))
      )
    )
  )
  
  
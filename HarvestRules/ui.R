#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyalert)
library(shinyjs)
# Define UI for application that draws a histogram
shinyUI(tagList(useShinyjs(), useShinyalert(),
                fluidPage(
  
  # Application title
  titlePanel("Harvest Strategies"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("sigma",
                  "Environmental Noise",
                  min = 0,
                  max = 0.6,
                  value = 0.2
      ),
      ########## Java addition ##################
      actionLink("help1", label="What does this do?", icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
      hr(),
      sliderInput("bstart",
                  "Starting Biomass (x 1000)",
                  min = 0,
                  max = 100,
                  value = 33
      ),
      hr(),
       radioButtons("hr", "Harvest Strategy:",
                    c("Constant Catch" = "const",
                      "Constant Exploitation Rate" = "f",
                      "Constant Escapement" = "esc")
    ),
       hr(),
       sliderInput("catch",
                   "Catch:",
                   min = 0,
                   max = 6000,
                   value = 4500,
                   step = 10),
       actionLink("help2", label="When do I use this?", icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
       sliderInput("f",
                   "Exploitaton Rate (F):",
                   min = 0,
                   max = .2,
                   value = 0.1),
       actionLink("help3", label="When do I use this?", icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
       sliderInput("esc",
                   "Escapement (x 1,000):",
                   min = 20,
                   max = 60,
                   value = 30),
       actionLink("help4", label="When do I use this?", icon=icon("info-circle"), style="color: #1faad7; font-size: 14px"),
    hr(),
    actionButton("do.one", "Run Once"),
    actionButton("do", "Run Many Times and Summarize"),
    hr()
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Single Model Run", plotOutput("simplot")),
        tabPanel("Summary of Many Runs", tableOutput("sumtable"))
    )
  )
)
)
)
)

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
  titlePanel("Stock Status Simulator"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
       sliderInput("fmax",
                   "Maximum fishing intensity",
                   min = 0,
                   max = 0.4,
                   value = 0.3,
                   ticks = FALSE,
                   ),
       radioButtons(
         "fishing.pattern",
         "Fishing Pattern",
         c(
           "Pattern 1"  =  1,
           "Pattern 2" = 2,
           "Pattern 3" = 3,
           "Pattern 4"= 4
         )
         )
       ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Stock Status Plot", plotOutput("simplot")),
        tabPanel("Population and Catch", plotOutput("catchplot"))
      )
    )
  )
))

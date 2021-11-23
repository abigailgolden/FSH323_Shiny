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
    titlePanel("Open Access Fisheries"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("c",
                        "Cost per Boat:",
                        min = 10000,
                        max = 35000,
                        value = 15000),
        sliderInput("p",
                    "Price per pound:",
                    min = 0,
                    max = 1.0,
                    value = 0.45)
    ,
    sliderInput("q1000",
                "Boat catching power:",
                min = 0.15,
                max = 0.65,
                value = 0.4)
),
    
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
))

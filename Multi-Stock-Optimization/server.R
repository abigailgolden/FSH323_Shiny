#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  
  prod.fun <- function(b, r, K) b * r * (1 - b/ K)

  plot.fun <- function(f) {
    blist <- seq(0, 10000,length.out = 100)
    library(viridis)
    cols <- plasma(5)[c(2,3,4)]
    r <- c(0.5, 0.2, 0.075)
    k <- c(4000, 7500, 10000)
    plot(blist, prod.fun(blist, r[1], k[1]),
         type = "n",
         xlab = "Stock Biomass",
         ylab = "Surplus Production or Catch",
         ylim = c(0, 550),
         yaxs = "i",
         xlim = c(0, 10000),
         xaxs = "i",
         cex.lab = 1.5,
         cex.axis = 1.5,
         las =1)
    abline(h = c(100, 200, 300, 400, 500), col = "gray", lwd = 1)
    abline(v = seq(500, 10000, by = 500), col= "gray", lwd = 1)
    lines(blist, prod.fun(blist, r[1], k[1]),
          lwd = 3,
          col = cols[1])
                        
    
    lines(blist, prod.fun(blist, r[2], k[2]),
          lwd = 3,
          col = cols[2]
    )
    
    lines(blist, prod.fun(blist, r[3], k[3]),
          lwd = 3,
          col = cols[3]
    )
    lines(blist, f* blist, 
          lwd = 4,
          col = "black")
    
    # points at intersection
    if (f <= r[1]) {
      b <- k[1] * (1 - f/r[1])
      points(b, prod.fun(b, r[1], k[1]),
             pch = 21,
             bg = cols[1],
             col = cols[1],
             cex = 2)
      text(x = b, y = f*b, round(f*b,-1),pos = 2, cex = 1.5, col = cols[1])
    }
    if (f <= r[2]) {
      b <- k[2] * (1 - f/r[2])
      points(b, prod.fun(b, r[2], k[2]),
             pch = 21,
             bg = cols[2],
             col = cols[2],
             cex = 2)
      text(x = b, y = f*b, round(f*b,-1),pos = 2, cex = 1.5, col = cols[2])
    }
    if (f <= r[3]) {
      b <- k[3] * (1 - f/r[3])
      points(b, prod.fun(b, r[3], k[3]),
             pch = 21,
             bg = cols[3],
             col = cols[3],
             cex = 2)
      if (b < 1000)text(x = b, y = f*b, round(f*b,-1),pos = 4, cex = 1.5, col = cols[3]) 
      if (b>=1000) text(x = b, y = f*b, round(f*b,-1),pos = 2, cex = 1.5, col = cols[3])
      
    }
    
    text(y = 25, x = 3100, "Stock 1", col = cols[1], cex = 1.5)
    text(y = 25, x = 6350, "Stock 2", col = cols[2], cex = 1.5)
    text(y = 25, x = 8900, "Stock 3", col = cols[3], cex = 1.5)
  }

    output$distPlot <- renderPlot({plot.fun(input$f)})

})

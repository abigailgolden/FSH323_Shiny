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
  plot.stocks <- function(f) {
  r1 <- 0.05
  r2 <- 0.2
  r3 <- 0.6
  
  k1 <- 100
  k2 <- 75
  k3 <- 50
  fs <- rep(f, 30)
  b1 <- b2 <- b3 <- c1 <- c2 <- c3 <- rep(NA, 30)
  
  b1[1] <- k1
  b2[1] <- k2
  b3[1] <- k3
  
  c1[1] <- k1 * fs[1]
  c2[1] <- k2* fs[1]
  c3[1] <- k3* fs[1]
  cex.mult <- 1.25
  prod.fun <- function(b, r, k) b + b*r * ( 1- b/k)
  for (i in 2:30) {
    b1[i] <- max(0,prod.fun(b1[i-1], r1, k1) - c1[i-1])
    c1[i] <- b1[i] * fs[i]
    
    b2[i] <- max(0,prod.fun(b2[i-1], r2, k2) - c2[i-1])
    c2[i] <- b2[i] * fs[i]
    
    b3[i] <- max(0,prod.fun(b3[i-1], r3, k3) - c3[i-1])
    c3[i] <- b3[i] * fs[i]
  }
  
  bmsy1<-k1/2
  bmsy2 <- k2/2
  bmsy3 <- k3/3
  fmsy1 <- r1/2
  fmsy2 <- r2/2
  fmsy3 <- r3/2
  
  library(viridis)
  cols <- rev(plasma(30))
  par(mfrow = c(2,1), las = 1, mar = c(4,4,1,1))
  plot(1:30, b1,
       xlim <- c(0,30),
       ylim <- c(0,k1),
       type = "l",
       lwd = 2,
       xlab = "years",
       ylab = "Biomass",
       col = cols[5],
       cex.lab = cex.mult,
       cex.axis = cex.mult
  )
  
  lines(1:30, b2,
        lwd = 2,
        col = cols[15])
  
  lines(1:30, b3,
        lwd = 2,
        col = cols[30])
  legend("topright", legend = c("Stock 1", "Stock 2", "Stock 3"), lwd = 2, lty = "solid",col = cols[c(3, 15, 30)], cex= cex.mult)
  abline(h = 0.5 * bmsy1, lty = "dotted", lwd = 2, col = cols[3])
  abline(h = 0.5 * bmsy2, lty = "dotted", lwd = 2, col = cols[15])
  abline(h = 0.5 * bmsy3, lty = "dotted", lwd = 2, col = cols[30])
  
  totalcatch <- round(rowSums(cbind(c1,c2, c3))[30],2)
  plot(1:30, c1,
       xlim <- c(0,30),
       ylim <- c(0,20),
       type = "l",
       lwd = 2,
       xlab = "years",
       ylab = "Catch",
       col = cols[5],
       cex.lab = cex.mult,
       cex.axis = cex.mult
  )
  
  lines(1:30, c2,
        lwd = 2,
        col = cols[15])
  
  lines(1:30, c3,
        lwd = 2,
        col = cols[30])
  points(30, rowSums(cbind(c1, c2, c3))[30],
        pch = 21,
        bg = "black",
        cex = 2)
  legend("topright", legend = "total catch final year", pch = 21, pt.bg = "black",cex = cex.mult)
                    


  }
  
  get_totalcatch <- function(f) {
    r1 <- 0.05
    r2 <- 0.2
    r3 <- 0.6
    
    k1 <- 100
    k2 <- 90
    k3 <- 80
    fs <- rep(f, 30)
    b1 <- b2 <- b3 <- c1 <- c2 <- c3 <- rep(NA, 30)
    
    b1[1] <- k1
    b2[1] <- k2
    b3[1] <- k3
    
    c1[1] <- k1 * fs[1]
    c2[1] <- k2* fs[1]
    c3[1] <- k3* fs[1]
    cex.mult <- 1.25
    prod.fun <- function(b, r, k) b + b*r * ( 1- b/k)
    for (i in 2:30) {
      b1[i] <- max(0,prod.fun(b1[i-1], r1, k1) - c1[i-1])
      c1[i] <- b1[i] * fs[i]
      
      b2[i] <- max(0,prod.fun(b2[i-1], r2, k2) - c2[i-1])
      c2[i] <- b2[i] * fs[i]
      
      b3[i] <- max(0,prod.fun(b3[i-1], r3, k3) - c3[i-1])
      c3[i] <- b3[i] * fs[i]
      
    }
    return(paste0("Total catch in year 30 = ",round(rowSums(cbind(c1, c2, c3))[30],2)))
}
  
   
  output$stockplot <- renderPlot({plot.stocks(input$f)},
                                 height = 700,
                                 width = 400)
  output$totalcatch <- renderText({get_totalcatch(input$f)})
  
})

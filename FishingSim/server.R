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
  simpop <- function(n.start, catch, prodtype) {
    r.base <- 0.2
    n.list <- 0:100
    par(xpd = F, las = 1)
    library(viridis)
    cols <- rev(plasma(20))
    line.col <- plasma(3)[1:2]
    text.mult <- 1.5
    if (prodtype == "const") {
      r <- r.base
      p.list <- r * n.list
    }
    if (prodtype == "compensatory") {
      # repeat for compensatory
      K <- 100
      r <- r.base * 2
      p.list <- r * n.list * (1 - n.list / K)
    }
    plot(
      n.list,
      p.list,
      type = "l",
      col = line.col[1],
      lwd = 3,
      xlab = "Population Size",
      ylab = "Productivity or Catch",
      xlim = c(0, 100),
      ylim = c(0, 20),
      cex.axis = text.mult,
      cex.lab = text.mult
    )
    abline(h = catch, lwd = 3, col = line.col[2])
    abline(
      v = n.start,
      lty = "dashed",
      col = cols[1],
      lwd = 2
    )
    legend(
      "topleft",
      legend = c("Production", "Catch"),
      lty = "solid",
      lwd = 3,
      col = line.col,
      bty = "n",
      cex = text.mult
    )
    output <- rep(NA, times = 1000)
    output[1] = n.start
    n.loops <- length(output)
    if (prodtype == "const")
      for (i in 2:n.loops)
        output[i] <- max(0, output[i - 1] * r + output[i - 1] - catch)
    
    if (prodtype == "compensatory")
      for (i in 2:n.loops)
        output[i] <-
      max(0, output[i - 1] * r * (1 - output[i - 1] / K) + output[i - 1] - catch)
    
    abline(v = output[length(output)], lty = "dashed", col = cols[20])
    par(xpd = T)
    legend(
      x = 50,
      y = 24,
      horiz = T,
      xjust = 0.5,
      legend = c("Initial Population size", "Final Population Size"),
      pch = 21,
      pt.bg = cols[c(1, 20)],
      bty = "n",
      cex = text.mult
    )
    output <- output[output <= 120]
    par(xpd = NA)
    
  
    for (i in 2:length(output)) {
      segments(output[i-1], 21.5, output[i], 21.5,lwd = 2, col = cols[i])
    }
    points(
      output,
      rep(21.5, length(output)),
      pch = 21,
      bg = cols,
      col = cols,
      cex = 1.5
    )
    
  }
  
  
  output$simplot <- renderPlot({
    simpop(
      n.start = input$n.start,
      catch = input$catch,
      prodtype = input$prodtype
    )
  },
  width = 480,
  height = 480)
})

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
    plot.fun <- function(p, c, q1000) {
        r <-0.2
        K <- 100000
        q <- q1000/1000    
        Ef <- seq(0, 500, by = 1)
        fs <- q* Ef 
        bs <- (r-fs)*K/r
        cs <- fs * bs
        vals <- cs *p * 2204
        costs <- c *  Ef
        library(viridis)
        cols <- plasma(4)[c(2,3)]
        par(mfrow= c(3,1), las = 1, xpd = F, mar = c(4,7,1,1))
        plot(Ef, vals / 1000,
             type = "l",
             col = cols[1],
             lwd = 3,
             xlab = "# of boats",
             ylab = "",
             xlim = c(0, 500),
             ylim = c(0, 9999),
             xaxs = "i",
             yaxs = "i",
             cex.lab = 1.75,
             cex.axis = 1.75
        )
        mtext("Catch Value and Cost (thousands)", side = 2, line = 5, las =0, cex = 1.25)
        lines(Ef, costs / 1000,
              lwd = 3,
              col = cols[2]
        )
        equil_ef <- max(0,r/q*(1-c/(p*2204*q*K)))
        equil_f <- q * equil_ef
        equil_b <- (r - equil_f ) * K / r
        equil_c <- equil_f* equil_b
        equil_val <- equil_f* equil_c * p * 2204
        text(x = 0, y = 9000, labels = paste0("Catch Value = ",round(equil_val/1000,0)), pos = 4, cex = 1.75)
        text(x = 0, y = 8000, labels = paste0("Catch  = ",round(equil_c/1000,2)), pos = 4, cex = 1.75)
        par(xpd = F)
        abline(v = equil_ef, col = "black", lwd  = 2, lty = "dashed")
        plot(Ef, cs ,
             type = "l",
             col = cols[1],
             lwd = 3,
             xlab = "# of boats",
             ylab = "",
             xlim = c(0, 500),
             ylim = c(0, 6000),
             xaxs = "i",
             yaxs = "i",
             cex.lab = 1.75,
             cex.axis = 1.75
        )
        mtext("Catch ( mt)", side = 2, line = 5, las =0, cex = 1.25)
        abline(v = equil_ef, col = "black", lwd  = 2, lty = "dashed")
        
        plot(Ef, bs / 1000,
             type = "l",
             lwd = 3,
             col = cols[1],
             xlab  = "# of boats",
             ylab = "",
             xlim = c(0, 500),
             ylim = c(0, 100),
             xaxs = "i",
             yaxs = "i",
             cex.lab = 1.75,
             cex.axis = 1.75)
        
        mtext("Population Biomass (thousand mt)", side = 2, line = 5, las =0, cex = 1.25)
    par(xpd = F)
    abline(v = equil_ef, col = "black", lwd  = 2, lty = "dashed")
    # # make surplus production plot
    # 
    # blist <- seq(0, K, length.out = 100)
    # plot(blist / 1000, r*blist * (1 - blist/K),
    #      type = "l",
    #      lwd = 3,
    #      col = cols[1],
    #      xlab  = "Population Biomass (thousand mt)",
    #      ylab = "",
    #      xlim = c(0, 100),
    #      ylim = c(0, 5000),
    #      xaxs = "i",
    #      yaxs = "i",
    #      cex.lab = 1.75,
    #      cex.axis = 1.75)
    # points(x = equil_b/1000, y = equil_c, pch = 21, bg = cols[2], cex = 3, col = cols[2])
    # mtext("Surplus Production", side = 2, line = 5, las =0, cex = 1.25)
    
}
    output$distPlot <- renderPlot({plot.fun(input$p, input$c, input$q1000)
        
       
    },
    width = 500,
    heigh = 700)

})

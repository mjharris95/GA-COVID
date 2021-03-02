library("PerformanceAnalytics")
library(tidyverse)
library(magrittr)
load("C:/Users/mallj/Onedrive/Mordecai/COVID/Data/ga_tidy_df")

my_heights<-sapply(c("white", "black", "hispanic", "asian", "indigenous"), function(x) max(hist(unlist(df[x]))$counts)*1.1)

chart.Correlation.no_line <-
  function (R, histogram = TRUE, method=c("pearson", "kendall", "spearman"), ...)
  { # @author R Development Core Team
    # @author modified by Peter Carl & Marek Lahoda
    # Visualization of a Correlation Matrix. On top the (absolute) value of the correlation plus the result 
    # of the cor.test as stars. On botttom, the bivariate scatterplots, with a linear regression fit. 
    # On diagonal, the histograms with probability, density and normal density (gaussian) distribution.
    
    x = checkData(R, method="matrix")
    
    if(missing(method)) method=method[1] #only use one
    cormeth <- method
    
    # Published at http://addictedtor.free.fr/graphiques/sources/source_137.R
    panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs", method=cormeth, cex.cor, ...)
    {
      usr <- par("usr"); on.exit(par(usr))
      par(usr = c(0, 1, 0, 1))
      r <- cor(x, y, use=use, method=method) # MG: remove abs here
      txt <- format(c(r, 0.123456789), digits=digits)[1]
      txt <- paste(prefix, txt, sep="")
      if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
      
      test <- cor.test(as.numeric(x),as.numeric(y), method=method)
      # borrowed from printCoefmat
      Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                       cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                       symbols = c("***", "**", "*", ".", " "))
      # MG: add abs here and also include a 30% buffer for small numbers
      text(0.5, 0.5, txt, cex = cex * (abs(r) + .3) / 1.3)
      text(.8, .8, Signif, cex=cex, col=2)
    }
    
    #remove method from dotargs
    dotargs <- list(...)
    dotargs$method <- NULL
    rm(method)
    
    hist.panel = function (x, ...=NULL ) {
      par(new = TRUE)
      my_hist<-hist(x)
      hist(x,
           col = "light gray",
           probability = TRUE,
           axes = FALSE,
           main = "",
           ylim=c(0, max(my_hist$counts)*1.1))
      #lines(density(x, na.rm=TRUE),
      #      col = "red",
       #     lwd = 1)
      # adding line representing density of normal distribution with parameters correponding to estimates of mean and standard deviation from the data 
      ax.x = seq(min(x), max(x), 0.1)                                                  # ax.x containts points corresponding to data range on x axis
      density.est = dnorm(ax.x, mean = mean(x), sd = sd(x))   # density corresponding to points stored in vector ax.x 
      #lines(ax.x, density.est, col = "blue", lwd = 1, lty = 1)                                # adding line representing density into histogram
      #rug(x)
    }
    
    # Linear regression line fit over points
    reg <- function(x, y, ...) {
      points(x,y, cex=.3, ...)
      abline(lm(y~x), col = "red") 
    }
    
    # Draw the chart
    if(histogram)
      pairs(x, gap=0, lower.panel=reg, upper.panel=panel.cor, diag.panel=hist.panel, cex.axis=1)
    else
      pairs(x, gap=0, lower.panel=reg, upper.panel=panel.cor) 
}



pdf(file="C:/Users/mallj/OneDrive/Mordecai/COVID/Figs/sup1.pdf", sup1, height=4.213, width=4.213)

df %>%
  dplyr::select(White=white, "Black or \nAfrican \nAmerican"=black, "Hispanic or \nLatino"=hispanic, 
                Asian=asian, "American \nIndian and \nAlaska \nNative"=indigenous) %>%
  chart.Correlation.no_line(histogram=TRUE, pch=10, cex=3) -> sup1

dev.off()
##/////////////////////////////////////////////////////////////////////////////
## plot_Histogram.R
##/////////////////////////////////////////////////////////////////////////////
#authors: Sebastian Kreutzer (1), Michael Dietze (2)
##organisation: 1 - JLU Giessen, Germany
##              2 - TU Dresden, Germany
##version: 0.4
##date: 2013-03-04
##=============================================================================
##description:
##		--	plots histogram with standard error idea from Rex Galbarith

plot_Histogram <- function (
  values,
	mtext,
  cex.global,
  breaks,
  se,
  rug,
  normal_curve, # optionally add a normal curve to the plot
  summary,
  summary.pos,
  colour,
  ...
) {
  ## Set general parameters ---------------------------------------------------
  ## Check/set default parameters
  if(missing(cex.global) == TRUE) {cex.global = 1}
  if(missing(mtext) == TRUE) {mtext <- ""}
  if(missing(se) == TRUE) {se = TRUE}
  if(missing(rug) == TRUE) {rug = TRUE}
  if(missing(colour) == TRUE) {colour = c("white", "black", "red", "black")}
  if(missing(summary) == TRUE) {summary <- ""}
  if(missing(normal_curve) == TRUE) {normal_curve = FALSE}  
  
  ## Check/set additional plot parameters
  extraArgs <- list(...) # read out additional arguments list
  
  main.plot <- if("main" %in% names(extraArgs)) {
    extraArgs$main
    } else {
      "Histogram"
    }
  
 xlab.plot <- if("xlab" %in% names(extraArgs)) {
    extraArgs$xlab
    } else {
      expression(paste(D[e], " Distribution"))
    }
  
  ylab.plot <- if("ylab" %in% names(extraArgs)) {
    extraArgs$ylab
    } else {
      c("Density",
        "Standard Error")
    }
  
  breaks.plot <- if("breaks" %in% names(extraArgs)) {
    extraArgs$breaks
  } else {
    hist(values[,1], plot = FALSE)$breaks
  }
  
  xlim.plot <- if("xlim" %in% names(extraArgs)) {
    extraArgs$xlim
  } else {
    range(breaks.plot)
  }

  pch.plot <- if("pch" %in% names(extraArgs)) {
    extraArgs$pch
  } else {
    1
  }
  
  ## Set plot area format
  par(oma = c(0,0,0,2),
      cex=cex.global)

  ## Plot histogram -----------------------------------------------------------
  hist(values[,1],
	  	 main = main.plot,
       xlab = xlab.plot,
       ylab = ylab.plot[1],
       xlim = xlim.plot,
       breaks = breaks.plot,
       freq = !normal_curve,
       col = colour[1]
  )
  
  ## Optionally, add rug ------------------------------------------------------
  if(rug == TRUE) {rug(values[,1], col = colour[2])}

  ## Optionally, add a normal curve based on the data -------------------------
  if(normal_curve == TRUE){
    ## cheat the R check routine, tztztz how neat
    x <- NULL
    rm(x)
    
    ## add normal distribution curve
    curve(dnorm(x,
                mean = mean(na.exclude(values[,1])),
                sd = sd(na.exclude(values[,1]))),
          col = colour[3],
          add = TRUE,
          lwd = 1.2 * cex.global)
  }
  
  ## Optionally, add standard error plot --------------------------------------
  if(se == TRUE) {
    par(new = TRUE)
    plot(values[,1:2],
         xlim = xlim.plot,
         pch = pch.plot,
         col = colour[4],
         main = "",
         xlab = "",
         ylab = "",
         axes = FALSE,
         frame.plot = FALSE
         )
    axis(side = 4,
         labels = TRUE,
         cex = cex.global
         )
    mtext(ylab.plot[2], 
          side = 4, 
          line = 3,
          cex = cex.global)
    
    par(new = FALSE)
  }
  
  ## Optionally add user-defined mtext
  mtext(side = 3,
          text = mtext,
          cex = 0.8 * cex.global) 
  
  ## Optionally add statistical summary ---------------------------------------
  ## Calculate statistical summary
  n_De      <- nrow(values) # number of samples
  mean_De   <- mean(values[,1]) # mean
  median_De <- median(values[,1]) # median
  sd_De     <- sd(values[,1]) # standard deviation
  
  ## Paste statistics
  label.text <- paste(ifelse("n" %in% summary == TRUE,
                             paste("n = ", 
                                   n_De, 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("mean" %in% summary == TRUE,
                             paste("mean = ", 
                                   round(mean_De, 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("median" %in% summary == TRUE,
                             paste("median = ", 
                                   round(median_De, 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdrel" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De/mean_De * 100, 2), " %",
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdrabs" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De, 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("serel" %in% summary == TRUE,
                             paste("se = ", 
                                   round(sd_De/(n_De * mean_De) * 100, 2), 
                                   " %\n", 
                                   sep = ""),
                             ""),
                      ifelse("sebas" %in% summary == TRUE,
                             paste("se = ", 
                                   round(sd_De/n_De, 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      sep = "")
  
  if(missing(summary.pos)==TRUE) {summary.pos <- c(xlim.plot[1], 
                                                   max(values[,2]))}
  
  text(x = summary.pos[1],
       y = summary.pos[2],
       adj = c(0, 1),
       labels = label.text,
       col = "black", 
       cex = 0.8 * cex.global)
  
}
##===========================================================================##
##EOF##


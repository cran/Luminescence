##//////////////////////////////////////////////
##//plot_KDE.R
##/////////////////////////////////////////////
##======================================
#authors: Sebastian Kreutzer (1), Michael Dietze (2)
#organisation: 1 - JLU Giessen, Germany
#              2 - TU Dresden, Germany
#vers.: 3.0
#date: 10/12/2012
##======================================

plot_KDE <- function(					
  values, # De values for plotting (data.frame)
  distribution.parameters = "",
  summary = "", # optional statistical summary of the data
	bw = "nrd0", # vary bin-width if necessary, choose a numeric value for manual setting
  ... # Additional arguments to pass on to plotting commands
) {
  # General statistical summary parameters -----------------------------------------------------------------------
	values    <- values[order(values[,1]),] # Sort data set in increasing order
	density   <- density(values[,1], kernel = "gaussian", bw = bw) # calculate density function
	n_De      <- nrow(values) # number of values in the plot
	mean_De   <- mean(values[,1]) # calculate mean
	median_De <- median(values[,1]) # calculate median
	sd_De     <- sd(values[,1]) # calculate standard deviation
	xkdemax   <- density$x[density$y==(max(density$y))] # calculate maximum KDE position
  
  # Set plot format parameters -----------------------------------------------------------------------
	extraArgs <- list(...) # read out additional arguments list
	main      <- if("main" %in% names(extraArgs)) {extraArgs$main} else # assign main text
                 {expression(paste(D[e], " Distribution"))}
	xlab      <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else # assign x axis label
                 {expression(paste(D[e], " [Gy]"))}
	ylabs     <- if("ylabs" %in% names(extraArgs)) {extraArgs$ylabs} else # assign y axes labels
                 {c("density", "cumulative frequency")}
	xlim      <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else # assign x axis limits
                 {c(min(values[,1] - mean(values[,2])), max(values[,1] + mean(values[,2])))}
  ylims     <- if("ylims" %in% names(extraArgs)) {extraArgs$ylims} else # assign y axes limits
	                    {c(min(density$y), max(density$y), 1, n_De)}
	colours   <- if("col" %in% names(extraArgs)) {extraArgs$col} else # assign colours
                 {c("#3F489D", "black", "black", "gray86")}
	cex       <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1} # assign scaling factor
	 
  # Create empty plot with defined format -----------------------------------------------------------------------
  par(oma = c(0,0,0,2), cex = cex) # adjust plot area properties
  plot(NA, # create empty plot to set plot dimensions
	     xlim = xlim,
	     ylim = c(1, n_De),
	     main = "",
	     xlab = "",
	     ylab = "",
	     axes = FALSE,
	     frame.plot = FALSE)

  # Optionally, plot sd polygon -----------------------------------------------------------------------
  if("sd" %in% distribution.parameters == TRUE) { # add sd polygon, optional
	  polygon(x = c(mean(values[,1]) - sd(values[,1]), mean(values[,1]) + sd(values[,1]),
	                mean(values[,1]) + sd(values[,1]), mean(values[,1]) - sd(values[,1])),
	          y = c(0, 0, n_De + 1, n_De + 1),
	          col = colours[4],
            border = FALSE)
	}
  
  # Optionally, plot qr polygon -----------------------------------------------------------------------
	if("qr" %in% distribution.parameters == TRUE) { # add qr polygon, optional
	  polygon(x = c(quantile(values[,1], 0.25), 
                  quantile(values[,1], 0.75),
	                quantile(values[,1], 0.75), 
                  quantile(values[,1], 0.25)),
	          y = c(0, 0, n_De + 1, n_De + 1),
	          col = colours[4],
	          border = FALSE)
	}

  # Optionally, plot vertical mean line -----------------------------------------------------------------------
	if("mean" %in% distribution.parameters == TRUE) {
	  abline(v = mean_De) # add mean line
	  text(mean_De * 1.005, n_De * 0.98,  # add text
         "mean", 
         srt = 90, 
         pos = 1,
         cex = 0.8 * cex)
	 }

  # Optionally, plot vertical median line -----------------------------------------------------------------------
  if("median" %in% distribution.parameters == TRUE) {
	  abline(v = median_De) # add median line
	  text(median_De * 1.005, n_De * 0.98, # add text
	       "median", 
	       srt = 90, 
	       pos = 1,
	       cex = 0.8 * cex)
	}  

  # Optionally, plot vertical KDE max line -----------------------------------------------------------------------
  if("kdemax" %in% distribution.parameters == TRUE) {
	  abline(v = xkdemax) # add KDE max line
	  text(xkdemax * 1.005, n_De * 0.98, # add text
	       "KDE max", 
	       srt = 90, 
	       pos = 1,
	       cex = 0.8 * cex)
	}

  # add probability density plot -----------------------------------------------------------------------
  par(new = TRUE)
  plot(density$x, density$y, # plot probability density graph
       type = "l", 
       main = main, 
       xlab = xlab, 
       ylab = ylabs[1],
       xlim = xlim,
       ylim = ylims[1:2],
       col = colours[1],
       cex = cex)
  
  # Create empty overlay plot -----------------------------------------------------------------------
  par(new = TRUE) # adjust plot options
  plot(NA, # add empty plot, scaled to secondary plot content
	     xlim = xlim,
       ylim = ylims[3:4],
       main = "",
       xlab = "",
       ylab = "",
       axes = FALSE,
	     frame.plot = FALSE)

  # Add secondary y-axis -----------------------------------------------------------------------
  axis(side = 4, labels = TRUE) # add second y-axis
  mtext(ylabs[2], side = 4, line = 3, cex = cex) # add second y-axis label
  
  # Add De error bars -----------------------------------------------------------------------
    arrows(values[,1]-values[,2]/2, # add De error bars
         1:length(values[,1]), 
	       values[,1]+values[,2]/2, 
		     1:length(values[,1]), 
 			   code = 3,
         angle = 90,
	       length = 0.05,
         col = colours[3])
  
  # Add De measurements -----------------------------------------------------------------------
    points(values[,1], 1:n_De, # add De values
         col = colours[2], 
         pch = 20)
  
  # Add optional descriptive statistics texts -----------------------------------------------------------------------
  ypos <- 2 # set vertical position variable
  if("n" %in% summary == TRUE) { # optionally, plot number of samples, n
    mtext (side = 3, padj = ypos, paste("n = ", n_De, sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
  if("mean" %in% summary == TRUE) { # optionally, plot mean De, mean
    mtext (side = 3, padj = ypos, paste("mean = ", round(mean_De, 2), sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
  if("median" %in% summary == TRUE) { # optionally, plot median De, median
    mtext (side = 3, padj = ypos, paste("median = ", round(median_De, 2), sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
	if("kdemax" %in% summary == TRUE) { # optionally, plot max KDE position
	  mtext (side = 3, padj = ypos, paste("KDE max = ", round(xkdemax, 2), sep = ""), 
	         col = "black", adj = 0.025, cex = 0.8 * cex)
	  ypos <- ypos + 1.5 # move vertical text position down one increment
	}
  
  if("sdrel" %in% summary == TRUE) { # optionally, plot relative standard deviation, sdrel
    mtext (side = 3, padj = ypos, paste("sd = ",
           round(sd_De / mean_De * 100, 2), " %", sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
  if("sdabs" %in% summary == TRUE) { # optionally, plot absolute standard deviation, sdabs
    mtext (side = 3, padj = ypos, paste("sd = ", round(sd_De, 2), sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
  if("serel" %in% summary == TRUE) { # optionally, plot realvie standard error, serel
    mtext (side = 3, padj = ypos, paste("se = ", 
           round(sd_De / (n_De * mean_De) * 100, 2), " %", sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
  
  if("seabs" %in% summary == TRUE) { # optionally, plot absolute standard error, seabs
    mtext (side = 3, padj = ypos, paste("se = ", round(sd_De / n_De, 2), sep = ""), 
           col = "black", adj = 0.025, cex = 0.8 * cex)
    ypos <- ypos + 1.5 # move vertical text position down one increment
  }
}#EndOf function
##==================================================================================================##
##EOF##

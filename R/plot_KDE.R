##/////////////////////////////////////////////////////////////////////////////
##//plot_KDE.R
##/////////////////////////////////////////////////////////////////////////////
##=============================================================================
#authors: Sebastian Kreutzer (1), Michael Dietze (2)
##organisation: 1 - JLU Giessen, Germany
##              2 - TU Dresden, Germany
##version: 3.0.4
##date: 2013-03-13
##=============================================================================

plot_KDE <- function(					
  values, # De values for plotting (data.frame)
  distribution.parameters,
  summary, # optional statistical summary of the data
  summary.pos, # optinal summary position coordinates
  bw = "nrd0", # if necessary vary bin-width by numeric value
  ... # Additional arguments to pass on to plotting commands
) {
  ## Check/set default parameters ---------------------------------------------
  if(missing(distribution.parameters)==TRUE) {distribution.parameters = ""}
  if(missing(summary)==TRUE) {summary = ""}
  
  ## General statistical summary parameters -----------------------------------
	## Sort data set in ascending order
  values    <- values[order(values[,1]),]
	## calculate density function
  density   <- density(values[,1], kernel = "gaussian", bw = bw)
	## calculate some further parameters
  n_De      <- nrow(values) # number of samples
	mean_De   <- mean(values[,1]) # mean
	median_De <- median(values[,1]) # median
	sd_De     <- sd(values[,1]) # standard deviation
	xkdemax   <- density$x[density$y==(max(density$y))] # maximum KDE position
  
  ## Set plot format parameters -----------------------------------------------
	extraArgs <- list(...) # read out additional arguments list
	main      <- if("main" %in% names(extraArgs)) {extraArgs$main} else
                 {expression(bold(paste(D[e], " Distribution")))}
	xlab      <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else
                 {expression(paste(D[e], " [Gy]"))}
	ylabs     <- if("ylabs" %in% names(extraArgs)) {extraArgs$ylabs} else
                 {c("density", "cumulative frequency")}
	xlim      <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
                 {c(min(values[,1] - mean(values[,2])), max(values[,1] + mean(
                   values[,2])))}
  ylims     <- if("ylims" %in% names(extraArgs)) {extraArgs$ylims} else
	                    {c(min(density$y), max(density$y), 1, n_De)}
	colours   <- if("col" %in% names(extraArgs)) {extraArgs$col} else
                 {c("#3F489D", "black", "black", "gray86")}
	cex       <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1}
  
  fun       <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}
  
  # Create empty plot with defined format -------------------------------------
  par(oma = c(0,0,0,2), cex = cex, mfrow=c(1,1)) # adjust plot area properties
  plot(NA, # create empty plot to set plot dimensions
	     xlim = xlim,
	     ylim = c(1, n_De),
	     main = "",
	     xlab = "",
	     ylab = "",
	     axes = FALSE,
	     frame.plot = FALSE)

  # Optionally, plot sd polygon -----------------------------------------------
  if("sd" %in% distribution.parameters == TRUE) { # add sd polygon, optional
	  polygon(x = c(mean(values[,1]) - sd(values[,1]), 
                  mean(values[,1]) + sd(values[,1]),
	                mean(values[,1]) + sd(values[,1]), 
                  mean(values[,1]) - sd(values[,1])),
	          y = c(0, 0, n_De + 1, n_De + 1),
	          col = colours[4],
            border = FALSE)
	}
  
  # Optionally, plot qr polygon -----------------------------------------------
	if("qr" %in% distribution.parameters == TRUE) { # add qr polygon, optional
	  polygon(x = c(quantile(values[,1], 0.25), 
                  quantile(values[,1], 0.75),
	                quantile(values[,1], 0.75), 
                  quantile(values[,1], 0.25)),
	          y = c(0, 0, n_De + 1, n_De + 1),
	          col = colours[4],
	          border = FALSE)
	}

  # Optionally, plot vertical mean line ---------------------------------------
	if("mean" %in% distribution.parameters == TRUE) {
	  abline(v = mean_De) # add mean line
	  text(mean_De * 1.005, n_De * 0.98,  # add text
         "mean", 
         srt = 90, 
         pos = 1,
         cex = 0.8 * cex)
	 }

  # Optionally, plot vertical median line -------------------------------------
  if("median" %in% distribution.parameters == TRUE) {
	  abline(v = median_De) # add median line
	  text(median_De * 1.005, n_De * 0.98, # add text
	       "median", 
	       srt = 90, 
	       pos = 1,
	       cex = 0.8 * cex)
	}  

  # Optionally, plot vertical KDE max line ------------------------------------
  if("kdemax" %in% distribution.parameters == TRUE) {
	  abline(v = xkdemax) # add KDE max line
	  text(xkdemax * 1.005, n_De * 0.98, # add text
	       expression(KDE[max]), 
	       srt = 90, 
	       pos = 1,
	       cex = 0.8 * cex)
	}

  # add probability density plot ----------------------------------------------
  par(new = TRUE)
  plot(density$x, density$y, # plot probability density graph
       type     = "l", 
       main     = main, 
       xlab     = xlab, 
       ylab     = ylabs[1],
       xlim     = xlim,
       ylim     = ylims[1:2],
       col      = colours[1],
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)
  
  # Create empty overlay plot -------------------------------------------------
  par(new = TRUE) # adjust plot options
  plot(NA, # add empty plot, scaled to secondary plot content
	     xlim = xlim,
       ylim = ylims[3:4],
       main = "",
       xlab = "",
       ylab = "",
       axes = FALSE,
	     frame.plot = FALSE)

  # Add secondary y-axis ------------------------------------------------------
  axis(side = 4, labels = TRUE, cex.axis = cex) # add second y-axis
  mtext(ylabs[2], side = 4, line = 3, cex = cex) # add second y-axis label
  
  # Add De error bars ---------------------------------------------------------
    arrows(values[,1]-values[,2]/2, # add De error bars
         1:length(values[,1]), 
	       values[,1]+values[,2]/2, 
		     1:length(values[,1]), 
 			   code = 3,
         angle = 90,
	       length = 0.05,
         col = colours[3])
  
  # Add De measurements -------------------------------------------------------
    points(values[,1], 1:n_De, # add De values
         col = colours[2], 
         pch = 20)
  
  # Add optional descriptive statistics texts ---------------------------------
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
                      ifelse("kdemax" %in% summary == TRUE,
                             paste("KDE max = ", 
                                   round(xkdemax, 2), 
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdrel" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De/mean_De * 100, 2), " %",
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("sdabs" %in% summary == TRUE,
                             paste("sd = ", 
                                   round(sd_De, 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      ifelse("serel" %in% summary == TRUE,
                             paste("se = ", 
                                   round((sd_De / sqrt(n_De)) / 
                                           mean_De * 100, 2), 
                                   " %\n", 
                                   sep = ""),
                             ""),
                      ifelse("seabs" %in% summary == TRUE,
                             paste("se = ", 
                                   round(sd_De / sqrt(n_De), 2),
                                   "\n", 
                                   sep = ""),
                             ""),
                      sep = "")
  
  if(missing(summary.pos)==TRUE) {summary.pos <- c(xlim[1], ylims[4])}
    
  text(x = summary.pos[1],
       y = summary.pos[2],
       adj = c(0, 1),
       labels = label.text,
       col = "black", 
       cex = 0.8 * cex)
  
  ##FUN by R Luminescence Team
  if(fun==TRUE){sTeve()}
  
}
##===========================================================================##
##EOF##

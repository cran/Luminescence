plot_KDE <- structure(function( # Plot kernel density estimate with statistics
  ### Function plots a kernel density estimate of measurement values in combination with 
  ### the actual values and associated error bars in ascending order. 
  ### Optionally, statistical measures such as mean, median, standard 
  ### deviation, standard error and quartile range can be provided visually 
  ### and numerically.
                      
  # ===========================================================================
  #authors: Sebastian Kreutzer (1), Michael Dietze (2)
  ##organisation: 1 - JLU Giessen, Germany
  ##              2 - GFZ Potsdam, Germany
  ##version: 3.2
  ##date: 2013-11-25
  # ===========================================================================

  values, 
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame}: two columns: De (\code{values[,1]})
  ### and De error (\code{values[,2]}). For plotting multiple data sets, these
  ### must be provided as \code{list} (e.g., \code{list(dataset1, dataset2)}).
  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): excludes \code{NA} values from the data
  ### set prior to any further operations.
  distribution.parameters,
  ### \code{\link{character}} (optional): plots additional distribution 
  ### parameters. Can be one or more out of "mean", "median", "kdemax" 
  ### (maximum value of probability density function), "sd" (standard 
  ### deviation) and "qr" (quartile range). Example: 
  ### \code{distribution.parameter=c("median","mean","sd")}. Mean and 
  ### median are potted as a line, the standard deviation is shown as a 
  ### gray polygon. Polygons are not possible for multiple data sets.
  summary,
  ### \code{\link{character}} (optional): adds numerical output to the plot. 
  ### Can be one or more out of: "n" (number of samples), "mean" (mean De 
  ### value), "median" (median of the De values), "kdemax" (maximum value of 
  ### probability density function), "kurtosis" (kurtosis), "skewness"
  ### (skewness), "sdrel" (relative standard deviation in 
  ### percent), "sdabs" (absolute standard deviation), "serel" (relative 
  ### standard error) and "seabs" (absolute standard error). Summary
  ### information is not possible for multiple datasets.
  summary.pos,
  ### \code{\link{numeric}} or \code{\link{character}} (with default): optional  
  ### position coordinates or keyword for the statistical summary. Y-coordinate  
  ### refers to the right hand y-axis.
  bw = "nrd0",
  ### \code{\link{character}} (with default): bin-width, choose a numeric 
  ### value for manual setting.
  output = FALSE,
  ### \code{\link{logical}}: Optional output of numerical plot parameters.
  ### These can be useful to reproduce similar plots. Default is \code{FALSE}.
  ...
  ### further arguments and graphical parameters passed to \code{\link{plot}}.
) {
  
  ## Homogenise input data format
  if(is(values, "list") == FALSE) {values <- list(values)}
  
  ## Check input data (values)
  for(i in 1:length(values)) {
    if(is(values[[i]], "RLum.Results")==FALSE & 
       is(values[[i]], "data.frame")==FALSE){
      stop(paste("[plot_KDE] Error: Wrong input data format",
                 "(!= 'data.frame' or 'RLum.Results')"))
    } else {
      if(is(values[[i]], "RLum.Results")==TRUE){
        values[[i]] <- get_RLum.Results(values[[i]])[,1:2]
      }
    }
  }

  ## Check/set default parameters ---------------------------------------------
  if(missing(distribution.parameters)==TRUE) {distribution.parameters = ""}
  if(missing(summary)==TRUE) {summary = ""}
  
  ## Optionally, count and exclude NA values and print result
  if(na.exclude == TRUE) {
    for(i in 1:length(values)) {
      n.NA <- sum(!complete.cases(values[[i]]))
      if(n.NA == 1) {print("1 NA value excluded.")
      } else if(n.NA > 1) {print(paste(n.NA, "NA values excluded."))}
      values[[i]] <- na.exclude(values[[i]])
    }
  }
  
  ## Merge global data set for plot limit calculation
  values.global <- values[[1]][,1:2]
  if(length(values) > 1){
    for(i in 2:length(values)) {
      colnames(values[[i]]) <- colnames(values[[1]])
      values.global <- rbind(values.global[,1:2], values[[i]][,1:2])
    }
  }
  
  ## Global number of samples and density estimates
  n_De.global <- numeric(length(values))
  density.global <- c(1,0)
  for(j in 1:length(values)) {
    n_De.global[j] <- nrow(values[[j]])
    density.range <- range(density(values[[j]][,1], 
                               kernel = "gaussian", 
                               bw = bw)$y)
    density.global[1] <- ifelse(density.range[1] < density.global[1], 
                             density.range[1], density.global[1])
    density.global[2] <- ifelse(density.range[2] > density.global[2], 
                                density.range[2], density.global[2])
  }
    
  ## Set plot format parameters -----------------------------------------------
  extraArgs <- list(...) # read out additional arguments list
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
    {expression(bold(paste(D[e], " Distribution")))}
  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else
    {expression(paste(D[e], " [s]"))}
  ylabs <- if("ylabs" %in% names(extraArgs)) {extraArgs$ylabs} else
    {c("density", "cumulative frequency")}
  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else
    {c(min(values.global[,1] - values.global[,2]), max(values.global[,1] + 
      values.global[,2]))}
  ylims <- if("ylims" %in% names(extraArgs)) {extraArgs$ylims} else
    {c(density.global[1], density.global[2], 1, max(n_De.global))}
  colours <- if(length(values) > 1) {
    if("col" %in% names(extraArgs)){  
        matrix(rep(extraArgs$col, each = 4), 
               nrow = length(values), 
               byrow = TRUE)
      } else {
        matrix(rep(1:length(values), 4), nrow = length(values))
      }
  } else {
    if("col" %in% names(extraArgs)) {
      matrix(c(extraArgs$col), nrow = 1)
    } else {
      matrix(c("#3F489D", "black", "black", "gray86"), nrow = 1)
    }
  }
  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else {1}
  fun <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}
    
  # Create empty plot with defined format -------------------------------------
  par(oma = c(0,0,0,2), cex = cex, mfrow=c(1,1)) # adjust plot area properties
  plot(NA, # create empty plot to set plot dimensions
       xlim = xlim,
       ylim = c(1, max(n_De.global)),
       main = "",
       xlab = "",
       ylab = "",
       axes = FALSE,
       frame.plot = FALSE)
     
  ## Loop through all data sets -----------------------------------------------
  for(i in 1:length(values)) {
    ## Sort data set in ascending order
    values[[i]] <- values[[i]][order(values[[i]][,1]),]
    ## calculate density function
    density <- density(values[[i]][,1], kernel = "gaussian", bw = bw)
    ## calculate some further parameters
    n_De <- nrow(values[[i]]) # number of samples
    mean_De <- mean(values[[i]][,1]) # mean
    median_De <- median(values[[i]][,1]) # median
    sd_De <- sd(values[[i]][,1]) # standard deviation
    xkdemax <- density$x[density$y==(max(density$y))] # maximum KDE position
       
    # Optionally, plot sd polygon ---------------------------------------------
    if(length(values) == 1) {
       if("sd" %in% distribution.parameters == TRUE) {
         polygon(x = c(mean(values[[i]][,1]) - sd(values[[i]][,1]), 
                       mean(values[[i]][,1]) + sd(values[[i]][,1]),
                       mean(values[[i]][,1]) + sd(values[[i]][,1]), 
                       mean(values[[i]][,1]) - sd(values[[i]][,1])),
                 y = c(0, 0, n_De + 1, n_De + 1),
                 col = colours[i, 4],
                 border = FALSE)
       }
    }
       
    # Optionally, plot qr polygon ---------------------------------------------
    if(length(values) == 1) {
      if("qr" %in% distribution.parameters == TRUE) {
        polygon(x = c(quantile(values[[i]][,1], 0.25), 
                      quantile(values[[i]][,1], 0.75),
                      quantile(values[[i]][,1], 0.75), 
                      quantile(values[[i]][,1], 0.25)),
                y = c(0, 0, n_De + 1, n_De + 1),
                col = colours[i, 4],
                border = FALSE)
      }
    }
    # Optionally, plot vertical mean line -------------------------------------
    if("mean" %in% distribution.parameters == TRUE) {
      abline(v = mean_De, col = colours[i, 2]) # add mean line
      text(mean_De * 1.005, max(n_De.global) * 0.98,  # add text
           "mean", 
           srt = 90, 
           pos = 1,
           col = colours[i, 2],
           cex = 0.8 * cex)
    }
    
    # Optionally, plot vertical median line -----------------------------------
    if("median" %in% distribution.parameters == TRUE) {
      abline(v = median_De, col = colours[i, 2]) # add median line
      text(median_De * 1.005, max(n_De.global) * 0.98, # add text
           "median", 
           srt = 90, 
           pos = 1,
           col = colours[i, 2],
           cex = 0.8 * cex)
    }  
    
    # Optionally, plot vertical KDE max line ----------------------------------
    if("kdemax" %in% distribution.parameters == TRUE) {
      abline(v = xkdemax, col = colours[i, 2]) # add KDE max line
      text(xkdemax * 1.005, max(n_De.global) * 0.98, # add text
           expression(KDE[max]), 
           srt = 90, 
           pos = 1,
           col = colours[i, 2],
           cex = 0.8 * cex)
    }
  }

  # add probability density plot ----------------------------------------------
  par(new = TRUE)
  plot(NA, # plot probability density plot
       main     = main, 
       xlab     = xlab, 
       ylab     = ylabs[1],
       xlim     = xlim,
       ylim     = ylims[1:2],
       cex      = cex,
       cex.lab  = cex,
       cex.main = cex,
       cex.axis = cex)
  for(i in 1:length(values)) {
    density <- density(values[[i]][,1], kernel = "gaussian", bw = bw)
    lines(density$x, density$y, col = colours[i, 1])
  }
    
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
  for(i in 1:length(values)) {
    
    ## calculate some further parameters
    n_De      <- nrow(values[[i]]) # number of samples
    mean_De   <- mean(values[[i]][,1]) # mean
    median_De <- median(values[[i]][,1]) # median
    sd_De     <- sd(values[[i]][,1]) # standard deviation
    xkdemax   <- density$x[density$y==(max(density$y))] # maximum KDE position
    
    arrows(values[[i]][,1] - values[[i]][,2]/2, # add De error bars
           1:length(values[[i]][,1]), 
           values[[i]][,1] + values[[i]][,2]/2, 
           1:length(values[[i]][,1]), 
           code = 3,
           angle = 90,
           length = 0.05,
           col = colours[i, 3])
  
    # Add De measurements -------------------------------------------------------
    points(values[[i]][,1], 1:n_De, # add De values
           col = colours[i, 3], 
           pch = 20)
  }
  
  # Add optional descriptive statistics texts ---------------------------------
  if(length(values) == 1) {
    ## calculate skewness
    skewness <- (mean((values[[1]][,1] - mean(values[[1]][,1]))^3)) / 
                  (sd(values[[1]][,1])^3)
    kurtosis <- (mean((values[[1]][,1] - mean(values[[1]][,1]))^4) ) / 
                  ((sd(values[[1]][,1])^4)) - 3
    
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

                        ifelse("skewness" %in% summary == TRUE,
                               paste("skewness = ", 
                                     round(skewness, 2),
                                     "\n", 
                                     sep = ""),
                               ""),
                        ifelse("kurtosis" %in% summary == TRUE,
                               paste("kurtosis = ", 
                                     round(kurtosis, 2),
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
    
    
    
    if(missing(summary.pos) == TRUE) {
      summary.pos <- c(xlim[1], ylims[4])
      summary.adj <- c(0, 1)
    } else if(length(summary.pos) == 2) {
      summary.pos <- summary.pos
      summary.adj <- c(0, 1)
    } else if(summary.pos[1] == "topleft") {
      summary.pos <- c(xlim[1], ylims[4])
      summary.adj <- c(0, 1)
    } else if(summary.pos[1] == "top") {
      summary.pos <- c(mean(xlim), ylims[4])
      summary.adj <- c(0.5, 1)
    } else if(summary.pos[1] == "topright") {
      summary.pos <- c(xlim[2], ylims[4])
      summary.adj <- c(1, 1)
    }  else if(summary.pos[1] == "left") {
      summary.pos <- c(xlim[1], mean(ylims[3:4]))
      summary.adj <- c(0, 0.5)
    } else if(summary.pos[1] == "center") {
      summary.pos <- c(mean(xlim), mean(ylims[3:4]))
      summary.adj <- c(0.5, 0.5)
    } else if(summary.pos[1] == "right") {
      summary.pos <- c(xlim[2], mean(ylims[3:4]))
      summary.adj <- c(1, 0.5)
    }else if(summary.pos[1] == "bottomleft") {
      summary.pos <- c(xlim[1], ylims[3])
      summary.adj <- c(0, 0)
    } else if(summary.pos[1] == "bottom") {
      summary.pos <- c(mean(xlim), ylims[3])
      summary.adj <- c(0.5, 0)
    } else if(summary.pos[1] == "bottomright") {
      summary.pos <- c(xlim[2], ylims[3])
      summary.adj <- c(1, 0)
    }
    
    text(x = summary.pos[1],
         y = summary.pos[2],
         adj = summary.adj,
         labels = label.text,
         cex = 0.8 * cex,
         col = 1)
  }

  ##FUN by R Luminescence Team
  if(fun==TRUE){sTeve()}
  
  if(output == TRUE) {
    return(list(summary.pos = summary.pos))
  }
    
  
  ##details<<
  ## The function allows passing several plot arguments, such as \code{main}, 
  ## \code{xlab}, \code{cex}. However, as the figure is an overlay of two 
  ## separate plots, \code{ylim} must be specified in the order: c(ymin_axis1, 
  ## ymax_axis1, ymin_axis2, ymax_axis2). Similarly, if other than the default 
  ## colours are desired, the argument \code{col} must be provided with colours in 
  ## the following order: probability density function, De values, De error 
  ## bars, sd or qr polygon. See examples for some further explanations. For 
  ## details on the calculation of the bin-width (parameter \code{bw}), see 
  ## \code{\link{density}}.
  
  ##seealso<<
  ## \code{\link{density}}, \code{\link{plot}}
  
  ##referencs<<
  ## Berger, G.W., 2010. An alternate form of probability-distribution plot 
  ## for De values. Ancient TL 28, pp. 11-21. \cr
  ## Berger, G.W., 2011. Response to Galbraith. Ancient TL 29, pp. 48-50. \cr
  ## Galbraith, R.F., 2011. Some comments arising from Berger (2010). Ancient 
  ## TL 29, pp. 41-47. \cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, pp.1-27.
  
  ##note<<
  ## The plot output is no 'PD' plot (cf. the discussion of Berger and 
  ## Galbraith in Ancient TL; see references)!

}, ex=function(){
  # read example data set
  data(ExampleData.DeValues, envir = environment())
  
  # native function call
  plot_KDE(ExampleData.DeValues)
  
  # function call with some user-defined modifications
  plot_KDE(ExampleData.DeValues,
           main = "Plot of Dose distribution data",
           col = c("red", "black", "grey", "cyan"),
           xlab = "Equivalent dose [Gy]",
           ylabs = c("KDE estimate", "cumulative De values"),
           distribution.parameters = c("qr", "median"), 
           xlim = c(2000, 5000),
           ylims = c(0, 0.005, -5, 50),
           summary = c("n", "median", "serel", "seabs"),
           cex = 0.8)
  
  # function call with complete numerical statistical description output
  plot_KDE(ExampleData.DeValues,
           distribution.parameters = c("qr", "kdemax"), 
           summary = c("n", "mean", "median", "kdemax", "serel", 
                       "sdrel", "sdabs", "seabs"))
  
  # function call with two data sets
  dataset1 <- ExampleData.DeValues[1:8,]
  dataset2 <- ExampleData.DeValues[9:25,]
  
  plot_KDE(list(dataset1, dataset2), col = c("blue", "orange"))
})
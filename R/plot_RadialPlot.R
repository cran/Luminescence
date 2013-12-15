plot_RadialPlot <- structure(function(# Function to create a Radial Plot
  ### A Galbraith's radial plot is produced on a logarithmic or a linear 
  ### scale.

  # ===========================================================================
  ##author<<
  ## Michhael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, JLU Giessen 
  ## (Germany)\cr
  ## Based on a rewritten S script of Rex Galbraith, 2010\cr

  ##section<<
  ## version 0.5.1 [2013-12-12]
  # ===========================================================================
  
  data,
  ### \code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
  ### (required): for \code{data.frame} two columns: De (\code{data[,1]})
  ### and De error (\code{data[,2]}). To plot several data sets in one plot,
  ### the data sets must be provided as \code{list}, e.g. 
  ### \code{list(data.1, data.2)}.
  na.exclude = TRUE,
  ### \code{\link{logical}} (with default): excludes \code{NA} values from the data 
  ### set prior to any further operations.
  log.z = TRUE,
  ### \code{\link{logical}} (with default): Option to display the z-axis
  ### in logarithmic scale. Default is \code{TRUE}.
  central.value,
  ### \code{\link{numeric}}: User-defined central value, primarily used for
  ### horizontal centering of the z-axis.
  plot.ratio,
  ### \code{\link{numeric}}: User-defined plot area ratio (i.e. curvature of
  ### the z-axis). If omitted, the default value (\code{4.5/5.5}) is used and
  ### modified automatically to optimise the z-axis curvature. 
  ### The parameter should be decreased when data points are plotted outside
  ### the z-axis or when the z-axis gets too elliptic.
  bar.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the bar showing the 2-sigma range around the central value. To 
  ### disable the bar, use \code{"none"}. Default is \code{"grey"}.
  grid.col,
  ### \code{\link{character}} or \code{\link{numeric}} (with default): colour 
  ### of the grid lines (originating at [0,0] and stretching to the z-scale).  
  ### To disable grid lines, use \code{"none"}. Default is \code{"grey"}.
  legend.text,
  ### \code{\link{character}}: optional vector with legend item names (e.g.
  ### sample ID). Legend will only be plotted if legend text is provided.
  stats,
  ### \code{\link{character}}: additional labels of statistically important
  ### values in the plot. One or more out of the following: \code{"min"}, 
  ### \code{"max"}, \code{"median"}.
  summary = FALSE,
  ### \code{\link{logical}} (with default): Shows statistical summary of 
  ### sample or sample groups below plot main header, default is 
  ### \code{FALSE}.
  output = FALSE,
  ### \code{\link{logical}}: Optional output of numerical plot parameters.
  ### These can be useful to reproduce similar plots. Default is \code{FALSE}.
  ...
  ### Further plot arguments to pass. \code{xlab} must be a vector of length 2,
  ### specifying the upper and lower x-axes labels.
) {
  ## Homogenise input data format
  if(is(data, "list") == FALSE) {data <- list(data)}
  
  ## Check input data
  for(i in 1:length(data)) {
    if(is(data[[i]], "RLum.Results") == FALSE & 
         is(data[[i]], "data.frame") == FALSE) {
      stop(paste("[plot_RadialPlot] Error: Input data format is neither",
                 "'data.frame' nor 'RLum.Results'"))
    } else {
      if(is(data[[i]], "RLum.Results") == TRUE) {
        data[[i]] <- get_RLum.Results(data[[i]])[,1:2]
      }
    }
  }
  
  ## check data and parameter consistency--------------------------------------
  if(missing(stats) == TRUE) {stats <- numeric(0)}
  if(missing(bar.col) == TRUE) {bar.col <- rep("grey80", length(data))}
  if(missing(grid.col) == TRUE) {grid.col <- rep("grey70", length(data))}
  
  ## check z-axis log-option for grouped data sets
  if(is(data, "list") == TRUE & length(data) > 1 & log.z == FALSE) {
    warning(paste("Option 'log.z' is not set to 'TRUE' altough more than one",
                  "data set (group) is provided."))
  }

  ## optionally, remove NA-values
  if(na.exclude == TRUE) {
    for(i in 1:length(data)) {
      data[[i]] <- na.exclude(data[[i]])
    }
  }
  
  ## remove negative values when z-axis is in log-scale
  if(log.z == TRUE) {
    for(i in 1:length(data)) {
      data.test <- data[[i]][,1] <= 0
      data[[i]] <- data[[i]][!data.test,]
      data.negative <- paste(seq(1, length(data.test))[data.test == TRUE], 
                             collapse = ", ")
      if(sum(data.test) > 0) {
        warning(paste("Warning! The following lines contain negative values: ",
                      data.negative,
                      ". These lines are removed when log.z == TRUE.", 
                      sep = ""))
      }
    }
  }
  
  
  ## calculate and append statistical measures --------------------------------
  
  ## z-values based on log-option
  z <- sapply(1:length(data), function(x){
    if(log.z == TRUE) {log(data[[x]][,1])} else {data[[x]][,1]}})
  if(is(z, "list") == FALSE) {z <- list(z)}
  data <- lapply(1:length(data), function(x) {
     cbind(data[[x]], z[[x]])})
  rm(z)

  ## calculate se-values based on log-option
  se <- sapply(1:length(data), function(x){
    if(log.z == TRUE) {data[[x]][,2] / data[[x]][,1]} else {data[[x]][,2]}})
  if(is(se, "list") == FALSE) {se <- list(se)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], se[[x]])})
  rm(se)
  
  ## calculate central values
  z.central <- sapply(1:length(data), function(x){
    sum(data[[x]][,3] / data[[x]][,4]^2) / sum(1 / data[[x]][,4]^2)})
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], z.central[[x]])})
  rm(z.central)
  
  ## calculate precision
  precision <- sapply(1:length(data), function(x){
    1 / data[[x]][,4]})
  if(is(precision, "list") == FALSE) {precision <- list(precision)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], precision[[x]])})
  rm(precision)
  
  ## calculate standard estimate
  std.estimate <- sapply(1:length(data), function(x){
    (data[[x]][,3] - data[[x]][,5]) / data[[x]][,4]})
  if(is(std.estimate, "list") == FALSE) {std.estimate <- list(std.estimate)}
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})
  
  ## append empty standard estimate for plotting
  data <- lapply(1:length(data), function(x) {
    cbind(data[[x]], std.estimate[[x]])})
  rm(std.estimate)
  
  ## generate global data set
  data.global <- data[[1]]
  if(length(data) > 1) {
    for(i in 2:length(data)) {
      data.global <- rbind(data.global, data[[i]])
    }
  }
  
  ## create column names
  colnames(data.global) <- c("De", 
                             "error", 
                             "z", 
                             "se", 
                             "z.central",
                             "precision", 
                             "std.estimate", 
                             "std.estimate.plot")

  ## calculate global central value as weighted average
  z.central.global <- sum(data.global[,3] / data.global[,4]^2) / 
      sum(1 / data.global[,4]^2)
  
  ## optionally adjust zentral value by user-defined value
  if(missing(central.value) == FALSE) {
    z.central.global <- ifelse(log.z == TRUE, 
      log(central.value), central.value)
  }
  

  ## create column names
  for(i in 1:length(data)) {
    colnames(data[[i]]) <- c("De", 
                             "error", 
                             "z", 
                             "se", 
                             "z.central",
                             "precision", 
                             "std.estimate", 
                             "std.estimate.plot")
  }

  ## re-calculate standardised estimate for plotting
  for(i in 1:length(data)) {
    data[[i]][,8] <- (data[[i]][,3] - z.central.global) / data[[i]][,4]
  }
  
  data.global.plot <- data[[1]][,8]
  if(length(data) > 1) {
    for(i in 2:length(data)) {
      data.global.plot <- c(data.global.plot, data[[i]][,8])
    }
  }
  data.global[,8] <- data.global.plot
  
  ## print warning for too small scatter
  if(max(abs(1 / data.global[6])) < 0.001) {
  warning("Variance < 1%! For what do you need a scatter plot?")
}
  
  ## read out additional arguments---------------------------------------------
  extraArgs <- list(...)
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else
    {expression(paste(D[e], " Distribution"))}
  
  sub <- if("sub" %in% names(extraArgs)) {extraArgs$sub} else {""}
  
  if("xlab" %in% names(extraArgs)) {
    if(length(extraArgs$xlab) != 2) {
      stop("Argmuent xlab is not of length 2!")
    } else {xlab <- extraArgs$xlab}
  } else {
    xlab <- c(if(log.z == TRUE) {
      "Relative error [%]"
      } else {
        "Error"
        }, 
      "Precision")
  }
  
  ylab <- if("ylab" %in% names(extraArgs)) {
    extraArgs$ylab
    } else {
      "Standardised estimate"
    }
  
  zlab <- if("zlab" %in% names(extraArgs)) {
    extraArgs$zlab
    } else {
      expression(paste(D[e], "[Gy]"))
    }
  
  if("zlim" %in% names(extraArgs)) {
    limits.z <- extraArgs$zlim
  } else {
    z.span <- (mean(data.global[,1]) * 0.5) / (sd(data.global[,1]) * 100)
    z.span <- ifelse(z.span > 1, 0.9, z.span)
    limits.z <- c((0.9 - z.span) * min(data.global[[1]]),
                  (1.1 + z.span) * max(data.global[[1]]))
  }
  
  if("xlim" %in% names(extraArgs)) {
    limits.x <- extraArgs$xlim
  } else {
    limits.x <- c(0, max(data.global[,6]))
  }
  
  if("ylim" %in% names(extraArgs)) {
    limits.y <- extraArgs$ylim
  } else {
    y.span <- (mean(data.global[,1]) * 10) / (sd(data.global[,1]) * 100)
    y.span <- ifelse(y.span > 1, 0.98, y.span)
    limits.y <- c(-(1 + y.span) * max(abs(data.global[,7])), 
                   (1 + y.span) * max(abs(data.global[,7])))
  }
  
  cex <- if("cex" %in% names(extraArgs)) {
    extraArgs$cex
  } else {
    1
  }
  
  lty <- if("lty" %in% names(extraArgs)) {
    extraArgs$lty
    } else {
      rep(2, length(data))
    }
  
  lwd <- if("lwd" %in% names(extraArgs)) {
    extraArgs$lwd
    } else {
      rep(1, length(data))
    }
  
  pch <- if("pch" %in% names(extraArgs)) {
    extraArgs$pch
    } else {
      rep(1, length(data))
    }
  
  col <- if("col" %in% names(extraArgs)) {
    extraArgs$col
    } else {
      1:length(data)
    }
  
  tck <- if("tck" %in% names(extraArgs)) {
    extraArgs$tck
  } else {
    NA
  }
  
  tcl <- if("tcl" %in% names(extraArgs)) {
    extraArgs$tcl
  } else {
    -0.5
  }
  
  show <- if("show" %in% names(extraArgs)) {extraArgs$show} else {TRUE}
  if(show != TRUE) {show <- FALSE}
  
  fun <- if("fun" %in% names(extraArgs)) {
    extraArgs$fun
  } else {
    FALSE
  }
  
  ## define auxiliary plot parameters -----------------------------------------
  
  ## optionally adjust plot ratio
  if(missing(plot.ratio) == TRUE) {
    if(log.z == TRUE) {
      plot.ratio <- 1 /  (1 * ((max(data.global[,6]) - min(data.global[,6])) / 
        (max(data.global[,7]) - min(data.global[,7]))))
    } else {
      plot.ratio <- 4.5 / 5.5
    }
  } 
  
  ## calculate conversion factor for plot coordinates
  f <- (max(data.global[,6]) - min(data.global[,6])) / 
       (max(data.global[,7]) - min(data.global[,7])) * plot.ratio
  
  ## calculate major and minor z-tick values
  tick.values.major <- round(pretty(limits.z, n = 5), 0)
  tick.values.minor <- round(pretty(limits.z, n = 25), 0)
  
  tick.values.major <- tick.values.major[tick.values.major >= 
    min(tick.values.minor)]
  tick.values.major <- tick.values.major[tick.values.major <= 
    max(tick.values.minor)]
  tick.values.minor <- tick.values.minor[tick.values.minor >= 
    min(tick.values.major)]
  tick.values.minor <- tick.values.minor[tick.values.minor <= 
    max(tick.values.major)]
  
  if(log.z == TRUE) {
    tick.values.major <- log(tick.values.major)
    tick.values.minor <- log(tick.values.minor)
  }
  
  ## calculate z-axis radius
  r <- max(sqrt((data.global[,6])^2+(data.global[,7] * f)^2)) #* 1.2
   
  ## calculate major z-tick coordinates
  tick.x1.major <- r / sqrt(1 + f^2 * (
    tick.values.major - z.central.global)^2)
  tick.y1.major <- (tick.values.major - z.central.global) * tick.x1.major
  tick.x2.major <- (1 + 0.015 * cex) * r / sqrt(
    1 + f^2 * (tick.values.major - z.central.global)^2)
  tick.y2.major <- (tick.values.major - z.central.global) * tick.x2.major
  ticks.major <- cbind(tick.x1.major, 
                       tick.x2.major, 
                       tick.y1.major, 
                       tick.y2.major)
  
  ## calculate minor z-tick coordinates
  tick.x1.minor <- r / sqrt(1 + f^2 * (
    tick.values.minor - z.central.global)^2)
  tick.y1.minor <- (tick.values.minor - z.central.global) * tick.x1.minor
  tick.x2.minor <- (1 + 0.007 * cex) * r / sqrt(
    1 + f^2 * (tick.values.minor - z.central.global)^2)
  tick.y2.minor <- (tick.values.minor - z.central.global) * tick.x2.minor
  ticks.minor <- cbind(tick.x1.minor, 
                       tick.x2.minor, 
                       tick.y1.minor, 
                       tick.y2.minor)
  
  
  ## calculate z-label positions
  label.x <- 1.03 * r / sqrt(1 + f^2 *
    (tick.values.major - z.central.global)^2)
  label.y <- (tick.values.major - z.central.global) * tick.x2.major
  
  ## create z-axes lables
  if(log.z == TRUE) {
    label.z.text <- round(exp(tick.values.major), 0)
  } else {
    label.z.text <- round(tick.values.major, 0)
  }

  labels <- cbind(label.x, label.y, label.z.text)
  
  ## calculate coordinates for 2-sigma-polygon overlay
  polygons <- matrix(nrow = length(data), ncol = 8)
  
  for(i in 1:length(data)) {
    polygons[i,1:4] <- c(limits.x[1], 
                         limits.x[1], 
                         x2 <- r / sqrt(1 + f^2 * (data[[i]][1,5] - 
                           z.central.global)^2) * 1.1,
                         x2 <- r / sqrt(1 + f^2 * (data[[i]][1,5] - 
                           z.central.global)^2) * 1.1)
    polygons[i,5:8] <- c(-2, 
                         2,
                         (data[[i]][1,5] - z.central.global) * 
                           polygons[i,3] + 2,
                         (data[[i]][1,5] - z.central.global) * 
                           polygons[i,4] - 2)
  }
  
  ## calculate node coordinates for semi-circle
  ellipse.values <- seq(from = min(c(tick.values.major, tick.values.minor)), 
                        to = max(c(tick.values.major, tick.values.minor)), 
                        length.out = 500)
  ellipse.x <- r / sqrt(1 + f^2 * (ellipse.values - z.central.global)^2)
  ellipse.y <- (ellipse.values - z.central.global) * ellipse.x
  ellipse <- cbind(ellipse.x, ellipse.y)
  ellipse.lims <- rbind(range(ellipse[,1]), range(ellipse[,2]))
  
  ## calculate statistical labels
  if(length(stats ==1)) {stats <- rep(stats, 2)}
  stats.data <- matrix(nrow = length(stats), ncol = 3)
  
  if("min" %in% stats == TRUE) {
    stats.data[1, 3] <- data.global[data.global[,1] == min(data.global[,1]), 1]
    stats.data[1, 1] <- data.global[data.global[,1] == stats.data[1, 3], 6]
    stats.data[1, 2] <- data.global[data.global[,1] == stats.data[1, 3], 8]
  }
  
  if("max" %in% stats == TRUE) {
    stats.data[2, 3] <- data.global[data.global[,1] == max(data.global[,1]), 1]
    stats.data[2, 1] <- data.global[data.global[,1] == stats.data[2, 3], 6]
    stats.data[2, 2] <- data.global[data.global[,1] == stats.data[2, 3], 8]
  }
  
  if("median" %in% stats == TRUE) {
    stats.data[3, 3] <- data.global[data.global[,1] == 
                                      median(data.global[,1]), 1][1]
    stats.data[3, 1] <- data.global[data.global[,1] == stats.data[3, 3], 6][1]
    stats.data[3, 2] <- data.global[data.global[,1] == stats.data[3, 3], 8][1]
  }
  
  ## calculate statistical summary
  summary.data <- matrix(nrow = length(data), ncol = 3)
  
  for(i in 1:length(data)) {
    ## assign n
    summary.data[i,1] <- nrow(data[[i]])
    ## assign central value
    summary.data[i,2] <- round(ifelse(log.z == TRUE, 
                                      exp(data[[i]][1, 5]), 
                                      data[[i]][1, 5])
                               , 1)
    ## assign percent within 2-sigma-range
    summary.data[i,3] <- round(sum(data[[i]][,7] > -2 & data[[i]][,7] < 2) /
                                nrow(data[[i]]) * 100 , 1)
  }
  
  ## build text expressions
  summary.text <- paste("n = ", 
                        summary.data[,1], " | ", 
                        "central value = ", 
                        summary.data[,2], " | ",
                        "within 2-sigma = ", 
                        summary.data[,3], 
                        " %", 
                        sep = "")
  
  ## recalculate axes limits if necessary
  limits.z.x <- range(ellipse[,1])
  limits.z.y <- range(ellipse[,2])
  if(!("ylim" %in% names(extraArgs))) {
    if(limits.z.y[1] < 0.66 * limits.y[1]) {
      limits.y[1] <- 1.8 * limits.z.y[1]
    }
    if(limits.z.y[2] > 0.77 * limits.y[2]) {
      limits.y[2] <- 1.3 * limits.z.y[2]
    }
    limits.y <- c(-max(abs(limits.y)), max(abs(limits.y)))
  }
  if(!("xlim" %in% names(extraArgs))) {
    if(limits.z.x[2] > 1.1 * limits.x[2]) {
      limits.x[2] <- limits.z.x[2]
    }
  }
  
  ## Generate plot ------------------------------------------------------------
  
  ## check if plotting is enabled
  if(show == TRUE) {
    
    ## setup plot area
    if(summary == TRUE) {
      toplines <- length(data)
    } else {toplines <- 1}
    
    par(oma = c(1, 1, 0, 0),
        mar = c(4, 4, 1 + toplines, 7),
        xpd = TRUE,
        cex = cex)
    
    ## create empty plot
    plot(NA, 
         xlim = limits.x, 
         ylim = limits.y,
         main = "",
         sub = sub,
         xlab = "", 
         ylab = ylab,
         xaxs = "i",
         yaxs = "i",
         frame.plot = FALSE, 
         axes = FALSE)
    
    ## calculate upper x-axis label values
    label.x.upper <- if(log.z == TRUE) {
      as.character(round(1/axTicks(side = 1)[-1] * 100, 1))
    } else {
      as.character(round(1/axTicks(side = 1)[-1], 1))
    }
    
    ## optionally, plot 2-sigma-bar
    if(bar.col[1] != "none") {
      for(i in 1:length(data)) {
        polygon(x = polygons[i,1:4], 
                y = polygons[i,5:8],
                lty = "blank",
                col = bar.col[i])
      }
    }
    
    ## optionally, add grid lines
    if(grid.col[1] != "none") {
      for(i in 1:length(tick.x1.major)) {
        lines(x = c(limits.x[1], tick.x1.major[i]),
              y = c(0, tick.y1.major[i]),
              col = grid.col)
      }
    }
    
    ## optionally, plot central value lines
    if(lwd[1] > 0 & lty[1] > 0) {
      for(i in 1:length(data)) {
        x2 <- r / sqrt(1 + f^2 * (
          data[[i]][1,5] - z.central.global)^2)
        y2 <- (data[[i]][1,5] - z.central.global) * x2
        lines(x = c(limits.x[1], x2),
              y = c(0, y2),
              lty = lty[i],
              lwd = lwd[i],
              col = col[i])
      }
    }
    
    ## overplot unwanted parts
    polygon(x = c(ellipse[,1], limits.x[2] * 2, limits.x[2] * 2),
            y = c(ellipse[,2], max(ellipse[,2]), min(ellipse[,2])),
            col = "white",
            lty = 0)
    
    ## add plot title
    title(main = main, line = toplines)
    
    ## plot lower x-axis (precision)
    axis(side = 1)
    mtext(side = 1, 
          text = xlab[2], 
          line = 3, 
          cex = cex)
    
    ## plot upper x-axis (standard error)
    axis(side = 1, 
         tck = 0.02, 
         lwd = 0, 
         lwd.ticks = 1, 
         at = axTicks(side = 1)[-1],
         labels = FALSE)
    axis(side = 1,
         lwd = 0,
         labels = label.x.upper,
         at = axTicks(side = 1)[-1],
         line = -3)
    mtext(side = 1, 
          text = xlab[1], 
          line = -4,
          cex = cex)
    
    ## plot y-axis
    axis(side = 2, 
         at = seq(-2, 2, by = 2))
    
    ## plot minor z-ticks
    for(i in 1:length(tick.values.minor)) {
      lines(x = c(tick.x1.minor[i], tick.x2.minor[i]),
            y = c(tick.y1.minor[i], tick.y2.minor[i]))
    }
    
    ## plot major z-ticks
    for(i in 1:length(tick.values.major)) {
      lines(x = c(tick.x1.major[i], tick.x2.major[i]),
            y = c(tick.y1.major[i], tick.y2.major[i]))
    }
    
    ## plot z-axis
    lines(ellipse)
    
    ## plot z-values
    text(x = label.x,
         y = label.y,
         label = label.z.text, 0)
    
    ## plot z-label
    mtext(side = 4, 
          text = zlab, 
          line = 5, 
          las = 3,
          cex = cex)
    
    ## plot values
    for(i in 1:length(data)) {
      points(data[[i]][,6], 
             data[[i]][,8], 
             col = col[i],
             pch = pch[i])
    }
    
    ##optionally add min, max, median sample text
    if(length(stats) > 0) {
      text(x = stats.data[,1],
           y = stats.data[,2],
           labels = round(stats.data[,3], 1),
           pos = 2,
           cex = 0.85)
    }
    
    ## optionally plot legend
    if(missing(legend.text) == FALSE) {
      legend(x = "topleft",
             legend = legend.text,
             lty = lty,
             lwd = lwd,
             pch = pch,
             col = col,
             text.col = col,
             cex = 0.9 * cex,
             bty = "n")
    }
    
    ## optionally add statistical summary
    if(summary == TRUE) {
      if(length(summary.text) > 0) {
        for(i in 1:length(summary.text)) {
          mtext(side = 3, 
                line = -i,
                text = summary.text[i],
                col = col[i],
                cex = 0.9 * cex)
        }
      }
    }
    
    ##FUN by R Luminescence Team
    if(fun==TRUE){sTeve()}
  }
  
  if(output == TRUE) {
    return(list(data = data,
                data.global = data.global,
                xlim = limits.x,
                ylim = limits.y,
                zlim = limits.z,
                r = r,
                plot.ratio = plot.ratio,
                ticks.major = ticks.major,
                ticks.minor = ticks.minor,
                labels = labels,
                polygons = polygons,
                ellipse.lims = ellipse.lims))
  }
  
  ### Returns a plot object.
  
  ##details<<
  ## Details and the theoretical background of the radial plot are given 
  ## in the cited literature. This function is based on an S script of Rex 
  ## Galbraith. To reduce the manual adjustments, the function has been 
  ## rewritten. Thanks to Rex Galbraith for useful comments on this function.
  ## \cr Plotting can be disabled by adding the argument 
  ## \code{plot = "FALSE"}, e.g. to return only numeric plot output.
  
  ##references<<
  ## Galbraith, R.F., 1988. Graphical Display of Estimates Having Differing 
  ## Standard Errors. Technometrics, 30 (3), pp. 271-281.
  ##
  ## Galbraith, R.F., 1990. The radial plot: Graphical assessment of spread in 
  ## ages. International Journal of Radiation Applications and Instrumentation. 
  ## Part D. Nuclear Tracks and Radiation Measurements, 17, 3, 207-214. 
  ##
  ## Galbraith, R. & Green, P., 1990. Estimating the component ages in a 
  ## finite mixture. International Journal of Radiation Applications and 
  ## Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17, 3 
  ## 197-206. 
  ##
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
  ## track ages. Nuclear Tracks And Radiation Measurements, 21, 4,  
  ## 459-470. 
  ##
  ## Galbraith, R.F., 1994. Some Applications of Radial Plots. Journal of the 
  ## American Statistical Association, 89 (428), pp. 1232-1242. \cr\cr
  ## Galbraith, R.F., 2010. On plotting OSL equivalent doses. Ancient TL, 
  ## 28 (1), 1-10. 
  ##
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and 
  ## some recommendations. Quaternary Geochronology, 11, 1-27. 

  ##seealso<<
  ## \code{\link{plot}}, \code{\link{plot_KDE}}, \code{\link{plot_Histogram}}
  
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## plot the example data straightforward
  plot_RadialPlot(data = ExampleData.DeValues)
  
  ## now with linear z-scale
  plot_RadialPlot(data = ExampleData.DeValues,
                  log.z = FALSE)
  
  ## now with output of the plot parameters
  plot1 <- plot_RadialPlot(data = ExampleData.DeValues,
                           log.z = FALSE,
                           output = TRUE)
  plot1
  plot1$zlim
  
  ## now with adjusted z-scale limits
  plot_RadialPlot(data = ExampleData.DeValues,
                 log.z = FALSE,
                 zlim = c(2000, 4000))
  
  ## now the two plots with serious but seasonally changing fun
  #plot_RadialPlot(data = data.3, fun = TRUE)
  
  ## now with user-defined central value, in log-scale again
  plot_RadialPlot(data = ExampleData.DeValues,
                  central.value = 3500)
  
  ## now with legend, colour, different points and smaller scale
  plot_RadialPlot(data = ExampleData.DeValues,
                  legend.text = "Sample 1",
                  col = "tomato4",
                  bar.col = "peachpuff",
                  pch = "R",
                  cex = 0.8)
  
  ## now without 2-sigma bar, grid lines and central value line
  plot_RadialPlot(data = ExampleData.DeValues,
                  bar.col = "none",
                  grid.col = "none",
                  lwd = 0)
  
  ## now with user-defined axes labels
  plot_RadialPlot(data = ExampleData.DeValues,
                  xlab = c("Data error [%]",
                           "Data precision"),
                  ylab = "Scatter",
                  zlab = "Equivalent dose [Gy]")
  
  ## now with minimum, maximum and median value indicated
  plot_RadialPlot(data = ExampleData.DeValues,
                  central.value = 3500,
                  stats = c("min", "max", "median"))
  
  ## now with a brief statistical summary header
  plot_RadialPlot(data = ExampleData.DeValues,
                  summary = TRUE)
  
  ## now the data set is split into sub-groups, one is manipulated
  data.1 <- ExampleData.DeValues[1:15,]
  data.2 <- ExampleData.DeValues[16:25,] * 1.3
  
  ## now a common dataset is created from the two subgroups
  data.3 <- list(data.1, data.2)
  
  ## now the two data sets are plotted in one plot
  plot_RadialPlot(data = data.3)
  
  ## now with some graphical modification
  plot_RadialPlot(data = data.3,
                  col = c("darkblue", "darkgreen"),
                  bar.col = c("lightblue", "lightgreen"),
                  pch = c(2, 6),
                  summary = TRUE)
})


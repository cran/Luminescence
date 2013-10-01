##//////////////////////////////////////////////////////////////////////////////
##//analyse_IRSAR.RF.R
##//////////////////////////////////////////////////////////////////////////////
##
##==============================================================================
##author: Sebastian Kreutzer
##organisation: Freiberg Instruments, JLU Giessen
##version: 0.1
##date: 2013-08-22
##==============================================================================
##TODO - keep fit.range in mind for De calculation
##TODO - example with example data

analyse_IRSAR.RF <- function(object,
                             sequence.structure = c("NATURAL", "REGENERATED"),
                             
                             fit.range.min, ##optional
                             fit.range.max,
                             fit.trace = FALSE,
                             fit.MC.runs = 10, 
                             
                             output.plot = TRUE, 
                             xlab.unit = "s",
                             legend.pos = "bottom",
                             ...
                             ){
  

##=============================================================================#
## INTEGRITY TESTS
##=============================================================================#

  ##MISSING INPUT
  if(missing("object")==TRUE){
    stop("[analyse_IRSAR.RF] Error: No value set for 'object'!")
  }
  
  ##INPUT OBJECTS
  if(is(object, "RLum.Analysis")==FALSE){
    stop("[analyse_IRSAR.RF] Error: Input object is not of type 'RLum.Analyis'!")
  }

  # Protocol Integrity Checks -------------------------------------------------- 
  
  ##ANALYSE SEQUENCE OBJECT STRUCTURE
  
  ##set vector for sequence structure 
  temp.protocol.step <- rep(sequence.structure, length(object@records))[1:length(object@records)]
  
  ##grep object strucute
  temp.sequence.structure <- get_structure.RLum.Analysis(object)
  
 
  ##set values for step
  temp.sequence.structure[,"protocol.step"] <- temp.protocol.step
    
  ##set fit.range for fitting
    
    if(missing(fit.range.min)==TRUE){fit.range.min <- 1}
    if(missing(fit.range.max)==TRUE){fit.range.max <- max(
      temp.sequence.structure$n.channels)}
    
    ##set to full range if no value is given
    fit.range <- c(fit.range.min:fit.range.max)
       
    ##if value if given, check the validity
    if(min(fit.range)< 1 | max(fit.range)>max(temp.sequence.structure$n.channels)){
      
      fit.range <- c(1:max(temp.sequence.structure$n.channels))
      warning("[analyse_IRSAR.RF] Fit range out of bounds, set to full data set extend.")

  }
    

  
##=============================================================================#
## PLOT PARAMETERS
##=============================================================================#  
  
  ##get channel resolution (should be equal for all curves)
  resolution.RF <- round(object@records[[1]]@data[2,1]-
                   object@records[[1]]@data[1,1], digits=2)
  
  # Set plot format parameters -----------------------------------------------------------------------
  extraArgs <- list(...) # read out additional arguments list
  
  main      <- if("main" %in% names(extraArgs)) {extraArgs$main} else # assign main text
  {"IR-RF"}
  
  xlab      <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else # assign x axis label
  {paste("time [", xlab.unit, "]", sep="")}
  
  ylab     <- if("ylabs" %in% names(extraArgs)) {extraArgs$ylabs} else # assign y axes labels
  {paste("IR-RF [cts/",resolution.RF," ", xlab.unit,"]",sep = "")}
  
  
  
##=============================================================================#
## FITTING 
##=============================================================================#
 
  # set values for fitting --------------------------------------------------
  
  ##grep first regenerated curve 
  values.regenerated <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="REGENERATED","id"]]]@data)
  
  values.regenrated <- as.data.frame(object@records[[2]]@data)
  values.regenrated.x <- values.regenrated[fit.range,1]
  values.regenrated.y <- values.regenrated[fit.range,2]
  
  
## REGENERATED SIGNAL
# set function for fitting ------------------------------------------------

fit.function <- as.formula(y~phi.0-(delta.phi*((1-exp(-lambda*x))^beta))) 

##stretched expontial function according to Erfurt et al. (2003)
## + phi.0 >> initial IR-RF flux
## + delta.phi >> dose dependent change of the IR-RF flux
## + lambda >> exponential parameter
## + beta >> dispersive factor

# set start parameter estimation ------------------------------------------
 
  fit.parameters.start <- c(
    phi.0 = max(values.regenrated.y),
    lambda = 0.0001,
    beta = 1,
    delta.phi = 2*(max(values.regenrated.y)-min(values.regenrated.y)))    

# start nls fitting -------------------------------------------------------
  
##Monte Carlo approach for fitting

  fit.parameters.results.MC.results <- data.frame()
  
  ##produce set of start paramters
  phi.0.MC <- rep(fit.parameters.start["phi.0"], fit.MC.runs)
  lambda.MC <- seq(0.0001, 0.001, by=(0.001-0.0001)/fit.MC.runs) ##TODO
  beta.MC <- rep(fit.parameters.start["beta"], fit.MC.runs)
  delta.phi.MC <- rep(fit.parameters.start["delta.phi"], fit.MC.runs)
  
  ##start fitting loop
  for(i in 1:fit.MC.runs){
  
  fit.MC <-try(nls(fit.function, 
                trace = FALSE, 
                data = data.frame(x=values.regenrated.x, y=values.regenrated.y), 
                algorithm = "port",
                start = list(
                  phi.0 = phi.0.MC[i],
                  delta.phi = delta.phi.MC[i],
                  lambda = lambda.MC[i],
                  beta = beta.MC[i]),
                nls.control(
                  maxiter = 100,
                  warnOnly = FALSE,
                  minFactor = 1/1024),
                lower = c(phi.0 = .Machine$double.xmin, 
                          delta.phi = .Machine$double.xmin, 
                          lambda = .Machine$double.xmin, 
                          beta = .Machine$double.xmin),
                upper = c(phi.0 = max(values.regenrated.y), 
                          delta.phi = max(values.regenrated.y),     
                          lambda = 1, 
                          beta = 100)),
               silent=TRUE)
           
   if(inherits(fit.MC,"try-error") == FALSE){  
   
      temp.fit.parameters.results.MC.results <- coef(fit.MC)
      
      fit.parameters.results.MC.results[i,"phi.0"] <- temp.fit.parameters.results.MC.results["phi.0"]
      fit.parameters.results.MC.results[i,"lambda"] <- temp.fit.parameters.results.MC.results["lambda"]
      fit.parameters.results.MC.results[i,"delta.phi"] <- temp.fit.parameters.results.MC.results["delta.phi"]
      fit.parameters.results.MC.results[i,"beta"] <- temp.fit.parameters.results.MC.results["beta"]
    
   }      
  }
  
 ##FINAL fitting after successful MC
 if(length(na.omit(fit.parameters.results.MC.results)[,1])!=0){
   
      ##choose median as final fit version
      fit.parameters.results.MC.results <- sapply(na.omit(fit.parameters.results.MC.results), median)

  
      ##try final fitting 
      fit <-try(nls(fit.function, 
                trace = fit.trace, 
                data = data.frame(x=values.regenrated.x, y=values.regenrated.y), 
                algorithm = "port",
                start = list(
                  phi.0 = fit.parameters.results.MC.results["phi.0"],
                  delta.phi = fit.parameters.results.MC.results["delta.phi"],
                  lambda = fit.parameters.results.MC.results["lambda"],
                  beta = fit.parameters.results.MC.results["beta"]),
                nls.control(
                  maxiter = 500,
                  warnOnly = FALSE,
                  minFactor = 1/4096),
                lower = c(phi.0 = .Machine$double.xmin, 
                        delta.phi = .Machine$double.xmin, 
                        lambda = .Machine$double.xmin, 
                        beta = .Machine$double.xmin),
                upper = c(phi.0 = max(values.regenrated.y), 
                          delta.phi = max(values.regenrated.y), 
                          lambda = 1, beta = 100)),
                silent=FALSE)
 }else{
   
   class(fit) <- "try-error"
   
 }
# get parameters ----------------------------------------------------------

if(inherits(fit,"try-error") == FALSE){
 
  fit.parameters.results <- coef(fit)
  
}else{
  
  fit.parameters.results <- NA
  
}

##=============================================================================#
## CALCULATING
##=============================================================================#
  
      
  ##grep values from natural signal 
  values.natural <- as.data.frame(object@records[[
    temp.sequence.structure[temp.sequence.structure$protocol.step=="NATURAL","id"]]]@data)
  
  values.natural.mean <- mean(values.natural[,2])
  values.natural.sd <- sd(values.natural[,2])
  
  values.natural.error.lower <- values.natural.mean + values.natural.sd 
  values.natural.error.upper <- values.natural.mean - values.natural.sd 
  
  if(is.na(fit.parameters.results[1]) == FALSE){
  
  De.mean <- round(log(-((values.natural.mean - fit.parameters.results["phi.0"])/
                      -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
                      -fit.parameters.results["lambda"], digits=2)
          
  De.error.lower <- round(log(-((values.natural.error.lower - fit.parameters.results["phi.0"])/
                     -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
                     -fit.parameters.results["lambda"],digits=2)
    
  De.error.upper <- round(log(-((values.natural.error.upper - fit.parameters.results["phi.0"])/
                            -fit.parameters.results["delta.phi"])^(1/fit.parameters.results["beta"])+1)/
                            -fit.parameters.results["lambda"],digits=2)
  
  }else{
    
    De.mean <- NA
    De.error.lower <- NA
    De.error.upper <- NA
       
  }
  
##=============================================================================#
## PLOTTING
##=============================================================================#
if(output.plot==TRUE){
  
  ##colours 
  col <- get("col", pos = .LuminescenceEnv)

  ##set plot frame
  layout(matrix(c(1,2),2,1,byrow=TRUE),c(2), c(1.5,0.4), TRUE)
  par(oma=c(1,1,1,1), mar=c(0,4,3,0))
  
  ##open plot area
  plot(NA,NA,
       xlim=c(0,max(temp.sequence.structure$x.max)),
       ylim=c(min(temp.sequence.structure$y.min), max(temp.sequence.structure$y.max)),
       xlab="",
       xaxt="n",
       ylab=ylab,
      main=main, 
       log="")

  ##plotting measured signal 
  points(values.regenerated[,1], values.regenerated[,2], pch=3, col="grey")
  
  ##mark values used for fitting
  points(values.regenrated.x, values.regenrated.y, pch=3, col=col[18])

  ##show fitted curve COLORED
  
    ##dummy to trick R CMD check
    x<-NULL; rm(x)
  
  curve(fit.parameters.results["phi.0"]-
          (fit.parameters.results["delta.phi"]*
          ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
        add=TRUE,
        from = values.regenerated[min(fit.range), 1],
        to = values.regenerated[max(fit.range), 1],
        col="red")

  ##show fitted curve GREY (previous red curve)
  curve(fit.parameters.results["phi.0"]-
        (fit.parameters.results["delta.phi"]*
           ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
      add=TRUE,
      from = min(values.regenerated[, 1]),
      to = values.regenerated[min(fit.range), 1],
      col="grey")

  ##show fitted curve GREY (after red curve)
  curve(fit.parameters.results["phi.0"]-
        (fit.parameters.results["delta.phi"]*
           ((1-exp(-fit.parameters.results["lambda"]*x))^fit.parameters.results["beta"])), 
      add=TRUE,
      from = values.regenerated[max(fit.range), 1],
      to = max(values.regenerated[, 1]),
      col="grey")

  ##PLOT NATURAL VALUES
  points(values.natural, pch=20, col="red")

  ##plot range choosen for fitting
  abline(v=values.regenerated[min(fit.range), 1], lty=2)
  abline(v=values.regenerated[max(fit.range), 1], lty=2)
  
  ##legend
  legend(legend.pos, legend=c("reg. measured","reg. used for fit", "natural"),  
          pch=c(3,3, 20), col=c("grey", col[18], "red"), 
          horiz=TRUE, bty="n", cex=.7)

  
  ##plot De if De was calculated 
  if(is.na(De.mean) == FALSE){
    
    lines(c(0,De.error.lower), c(values.natural.error.lower,values.natural.error.lower), lty=2, col="grey")
    lines(c(0,De.mean), c(values.natural.mean,values.natural.mean), lty=2, col="red")
    lines(c(0,De.error.upper), c(values.natural.error.upper,values.natural.error.upper), lty=2, col="grey")
    
    lines(c(De.error.lower, De.error.lower), 
          c(0,values.natural.error.lower), lty=2, col="grey")
    lines(c(De.mean,De.mean), c(0, values.natural.mean), lty=2, col="red")
    lines(c(De.error.upper, De.error.upper), 
          c(0,values.natural.error.upper), lty=2, col="grey")
    
  }
 
  ##Insert fit and result
  if(De.mean > max(values.regenrated.x) | De.error.upper > max(values.regenrated.x)){
    
    try(mtext(side=3, substitute(D[e] == De.mean, 
                                 list(De.mean=paste(
                                   De.mean," (",De.error.lower," ", De.error.upper,")", sep=""))),
              line=0, cex=0.8, col="red"), silent=TRUE)
    
    De.status <- "VALUE OUT OF BOUNDS"
    
  } else{
      
  try(mtext(side=3, substitute(D[e] == De.mean, 
                               list(De.mean=paste(
                                 De.mean," (",De.error.lower," ", De.error.upper,")", sep=""))),
            line=0, cex=0.8), silent=TRUE)
  
  De.status <- "OK"
  }

  ##==lower plot==##    
  par(mar=c(4.2,4,0,0))
  
  ##plot residuals	
  if(is.na(fit.parameters.results[1])==FALSE){
    
  plot(values.regenrated.x,residuals(fit), 
     xlim=c(0,max(temp.sequence.structure$x.max)),
     xlab="time [s]", 
     type="p", 
     pch=20,
     col="grey", 
     ylab="residual [a.u.]",
     #lwd=2,
     log="")

  ##add 0 line
  abline(h=0)
  }else{
    plot(NA,NA,
         xlim=c(0,max(temp.sequence.structure$x.max)),
         ylab="residual [a.u.]",
         xlab=xlab, 
         ylim=c(-1,1)
         )    
    text(x = max(temp.sequence.structure$x.max)/2,y=0, "Fitting Error!")   
  }
}#endif::output.plot
##=============================================================================#
## RETURN
##=============================================================================#
  
  ##combine values
  De.values <- data.frame(De = De.mean,
                          De.error.lower = De.error.lower,
                          De.error.upper = De.error.upper,
                          De.status = De.status,
                          row.names=NULL)
  
  newRLumResults.analyse_IRSAR.RF <- set_RLum.Results(
    data = list(
      De.values = De.values,
      fit = fit))

  return(newRLumResults.analyse_IRSAR.RF)

}##EOF

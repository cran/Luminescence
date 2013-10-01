##//////////////////////////////////////////////////////////////////////////////
##//plot_Rlum.Data.Curve.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen
##version: 0.1
##date: 2013-09-27
##==============================================================================

plot_RLum.Data.Curve <- function(object, 
                                 par.local = TRUE,
                                 ...){
  
  # Integrity check ----------------------------------------------------------------------------
  
  ##check if object is of class RLum.Data.Curve
  if(class(object) != "RLum.Data.Curve"){
    
    stop("[plot_RLum.Data.Curve]: Input object is not of type RLum.Data.Curve")
    
  }

  ##set labeling unit 
  lab.unit <- if(object@recordType=="OSL" | 
                   object@recordType=="IRSL" | 
                   object@recordType=="RL" | 
                   object@recordType=="RBR"){"s"} 
              else if (object@recordType == "TL"){"\u00B0C"}
              else {"unknown"}
  
  lab.xlab <- if(object@recordType=="OSL" | 
                   object@recordType=="IRSL" | 
                   object@recordType=="RL" | 
                   object@recordType=="RBR"){"t"} 
              else if (object@recordType == "TL"){"T"}
              else {"independent"}
  
  ##XSYG
  ##check for curveDescripter
  if("curveDescripter" %in% names(object@info) == TRUE){
    
    temp.lab <- strsplit(object@info$curveDescripter, split = ";")[[1]]
 
    xlab <- temp.lab[1]
    ylab <- temp.lab[2]
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {object@recordType}
  
  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else 
  {if(exists("xlab") == TRUE){xlab} else
    {paste(lab.xlab," [",lab.unit,"]", sep="")}}
  
  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} 
          else if (exists("ylab") == TRUE){ylab}
          else if (lab.xlab == "independent") {"dependent [unknown]"} 
          else {paste(object@recordType, 
                      " [cts/", round(max(object@data[,1])/length(object@data[,1]),digits=2)
         , " ", lab.unit,"]", sep="")}
  
  sub <-  if("sub" %in% names(extraArgs)) {extraArgs$sub} else
  { 
    if((object@recordType == "TL") & "RATE" %in% names(object@info)){
      paste("(",object@info$RATE," K/s)", sep = "")
    }                
  }
  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
  {1}
  
  type <- if("type" %in% names(extraArgs)) {extraArgs$type} else 
  {"l"}
  
  col <- if("col" %in% names(extraArgs)) {extraArgs$col} else 
  {1}
  
  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else 
  {c(min(object@data[,2]),max(object@data[,2]))}
  
  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else 
  {c(min(object@data[,1]),max(object@data[,1]))}
  
  log <- if("log" %in% names(extraArgs)) {extraArgs$log} else 
  {""}
  
  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
  {""}
  
  ##to avoid problems with plot method of RLum.Analysis
  plot.trigger <- if("plot.trigger" %in% names(extraArgs)) {extraArgs$plot.trigger} else 
  {FALSE}
  
  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local == TRUE){par(mfrow=c(1,1), cex = cex)}
  
  ##plot curve
  plot(object@data[,1], object@data[,2], 
       main = main,
       xlim = xlim,
       ylim = ylim,
       xlab = xlab, 
       ylab = ylab,
       sub = sub,
       type = type,
       log = log,
       col = col)
  
  ##plot additional mtext
  mtext(mtext, side = 3, cex = cex*0.8)          
}

apply_CosmicRayRemoval<- structure(function(#Function to remove cosmic rays from an RLum.Data.Spectrum S4 class objects
  ### The function provides several methods for cosmic ray removal and spectrum 
  ### smoothing for an RLum.Data.Spectrum S4 class objects 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  object,
  ### \code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): 
  ### S4 object of class \code{RLum.Data.Spectrum}
  
  method = "Pych",
  ### \code{\link{character}} (with default): Defines method that is applied for 
  ### cosmic ray removal. Allowed methods \code{smooth} (\code{\link{smooth}}),
  ### \code{smooth.spline} (\code{\link{smooth.spline}}) and 
  ### \code{Pych} (default). See details for further information.
  
  method.Pych.smoothing = 2,
  ### \code{\link{integer}} (with default): Smoothing parameter for 
  ### cosmic ray removal according to Pych (2003). The value defines how
  ### many neighboring values in each frame are used for smoothing 
  ### (e.g. \code{2} means that the two previous and two following values 
  ### are used)
  
  ...
  ### further arguments and graphical parameters that will be passed to the 
  ### \code{smooth} function.
  
){
  
  # Integrity check -----------------------------------------------------------
  
  ##check if object is of class RLum.Data.Spectrum
  if(class(object) != "RLum.Data.Spectrum"){
    
    stop("[apply_CosmicRayRemoval]: Input object is not of type RLum.Data.Spectrum")
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  kind <- if("kind" %in% names(extraArgs)) {extraArgs$kind} else 
  {"3RS3R"}
  
  twiceit <- if("twiceit" %in% names(extraArgs)) {extraArgs$twiceit} else 
  {TRUE}

  spar <- if("spar" %in% names(extraArgs)) {extraArgs$spar} else 
  {NULL}

  # Apply method ------------------------------------------------------------
  
  ## +++++++++++++++++++++++++++++++++++ (smooth) ++++++++++++++++++++++++++++##
  if(method == "smooth"){
  
    ## grep data matrix
    object.data.temp <- get_RLum.Data.Spectrum(object)
      
    ##apply smoothing 
    object.data.temp.smooth <- sapply(1:ncol(object.data.temp), function(x){
   
             smooth(object.data.temp[,x], 
                    kind = kind, 
                    twiceit = twiceit)

    })
  
  ## +++++++++++++++++++++++++++++++++++ (smooth.spline) +++++++++++++++++++++##
  }else if(method == "smooth.spline"){
    
    ## grep data matrix
    object.data.temp <- get_RLum.Data.Spectrum(object)
    
    #apply smoothing 
    object.data.temp.smooth <- sapply(1:ncol(object.data.temp), function(x){
      
      smooth.spline(object.data.temp[,x], 
                    spar = spar)$y
      
    })
  
  ## +++++++++++++++++++++++++++++++++++ (Pych) ++++++++++++++++++++++++++++++##
  }else if(method == "Pych"){

    ## grep data matrix
    object.data.temp <- get_RLum.Data.Spectrum(object)
  
    ## apply smoothing 
    object.data.temp.smooth <- sapply(1:ncol(object.data.temp), function(x){
         
      ##(1) - calculate sd for each subframe
      temp.sd <- sd(object.data.temp[,x])
  
      ##(2) - correct estimation of sd by 1-sigma clipping
      temp.sd.corr <- sd(object.data.temp[
        
        object.data.temp[,x] >= (mean(object.data.temp[,x]) - temp.sd) &
        object.data.temp[,x] <= (mean(object.data.temp[,x]) + temp.sd)
        
        , x])

      ##(3) - construct histogramm of count distribution
      temp.hist <- hist(object.data.temp[,x], 
                        breaks = length(object.data.temp[,x])/2, plot = FALSE)
      
      ##(4) - find mode of the histogram (e.g. peak)
      temp.hist.max <- which(temp.hist$counts == max(temp.hist$counts))
      
      ##(5) - find gaps in the histogram (bins with zero value)
      temp.hist.zerobin <- which(temp.hist$counts == 0)
   
        ##(5.1)
        ##select just values right from the peak
        temp.hist.zerobin <- temp.hist.zerobin[
          (temp.hist.max[1] + 1):length(temp.hist.zerobin)]
        
        ##(5.2)
        ##select non-zerobins
        temp.hist.nonzerobin <- which(temp.hist$counts != 0)
        temp.hist.nonzerobin <- temp.hist.nonzerobin[
          temp.hist.nonzerobin >=  (temp.hist.zerobin[1]-1)]
     
      ##(6) - find the first gap which is wider than the threshold
      temp.hist.nonzerobin.diff <- diff(
        temp.hist$breaks[temp.hist.nonzerobin])
        
          ## select the first value where the thershold is reached
          ## factor 3 is defined by Pych (2003)
          temp.hist.thres <- which(
            temp.hist.nonzerobin.diff >= 3 * temp.sd.corr)[1]

      ##(7) - use counts above the threshold and recalculate values
      ## on all further values 
      if(is.na(temp.hist.thres) == FALSE){
        
      object.data.temp[,x] <- sapply(1:nrow(object.data.temp), function(n){
                
        if(c(n + method.Pych.smoothing) <= nrow(object.data.temp) & 
             (n - method.Pych.smoothing) >= 0){
          
              ifelse(
                object.data.temp[n,x] >= temp.hist$breaks[temp.hist.thres],
                   median(object.data.temp[(n-method.Pych.smoothing):
                                           (n+method.Pych.smoothing),x]),   
                   object.data.temp[n,x])

        }else{
          
          object.data.temp[n,x]
        
        }
       
       })
    
      }
      
      ##return object
      return(object.data.temp[,x])
      
    })#end loop
      
    
  }else{
    
    stop("[apply_CosmicRayRemoval] Unkown method for cosmic ray removal.")
    
  }
  
  # Correct row and column names --------------------------------------------
  
  rownames(object.data.temp.smooth) <- rownames(object@data)
  colnames(object.data.temp.smooth) <- colnames(object@data)
  
  
  # Return Output------------------------------------------------------------
  
  temp.output <- set_RLum.Data.Spectrum(recordType = object@recordType,
                                        curveType = object@curveType,
                                        data = object.data.temp.smooth,
                                        info = object@info)
  
  invisible(temp.output)
  
  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## \bold{\code{method = "Pych"}} \cr
  ##
  ## This method applies the cosmic-ray removal algorithm described by 
  ## Pych (2003). Some aspects that are different to the publication:
  ## \itemize{
  ## \item{For interpolation between neighbouring values the median and not 
  ## the mean is used.}
  ## \item{The number of breaks to construct the histogram is set to:
  ## \code{length(number.of.input.values)/2}}
  ## }
  ## For further details see references below.
  ##
  ## \bold{\code{method = "smooth"}} \cr
  ##
  ## Method uses the function \code{\link{smooth}} to remove cosmic rays.\cr
  ## 
  ## Arguments that can be passed are: \code{kind}, \code{twiceit}\cr
  ## 
  ## \bold{\code{method = "smooth.spline"}} \cr
  ## Method uses the function \code{\link{smooth.spline}} 
  ## to remove cosmic rays.\cr
  ## Arguments that can be passed are: \code{spar}\cr
  ##
  ## \bold{How to combine methods?}\cr
  ##
  ## Different methods can combined by applying the method repeatedly on the 
  ## dataset (see example). 
                                   
  ##value<<
  ## Returns same object as input (\code{\linkS4class{RLum.Data.Spectrum}})
  
  ##references<<
  ## Pych, W., 2003. A Fast Algorithm for Cosmic-Ray Removal from Single Images.
  ## Astrophysics 116, 148-153.
  ## \url{http://arxiv.org/pdf/astro-ph/0311290.pdf?origin=publication_detail}
  
  ##note<<
  ## This function has BETA status
  
  ##seealso<<
  ## \code{\linkS4class{RLum.Data.Spectrum}}, \code{\link{smooth}}, 
  ## \code{\link{smooth.spline}}, \code{\link{apply_CosmicRayRemoval}}
  
  ##keyword<<
  ## manip
  
}, ex=function(){
  
 ##(1) - use with your own data and combine (uncomment for usage)
 ## run two times the default method and smooth with another method
 ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
 ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "Pych")
 ## your.spectrum <- apply_CosmicRayRemoval(your.spectrum, method = "smooth")
  
})#END OF STRUCTURE
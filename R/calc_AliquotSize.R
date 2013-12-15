calc_AliquotSize<- structure(function( # Estimate the amount of grains on an aliquot
  ### Estimate the number of grains on an aliquot. Alternatively, the packing
  ### density of an aliquot is computed.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  
  ##section<<
  ## version 0.2 [2013-11-25] 
  # ===========================================================================
  
  grain.size,
  ### \code{\link{numeric}} (\bold{required}): mean grain size (microns) or a 
  ### range of grain sizes from which the mean grain size is computed 
  ### (e.g. \code{c(100,200)}).
  sample.diameter,
  ### \code{\link{numeric}} (\bold{required}): diameter (mm) of the targeted
  ### area on the sample carrier.
  packing.density = 0.65,
  ### \code{\link{numeric}} (with default) empirical value for mean packing 
  ### density. \cr 
  ### If \code{packing.density = "inf"} a hexagonal structure on an
  ### infinite plane with a packing density of \eqn{0.906\ldots} is assumed.
  grains.counted
  ### \code{\link{numeric}} (optional) grains counted on a sample carrier. If
  ### a non-zero positive integer is provided this function will calculate the 
  ### packing density of the aliquot.
  ### If more than one value is provided the mean packing density and its 
  ### standard deviation is calculated. Note that this overrides
  ### \code{packing.density}.
  ){
  
  ##==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ##==========================================================================##
  
  if(length(grain.size) == 0 | length(grain.size) > 2) {
    cat(paste("Please provide the mean grain size or a range",
              "of grain sizes (in microns)."), fill = FALSE) 
    stop(domain=NA)
  }
  
  if(packing.density < 0 | packing.density > 1) {
    if(packing.density == "inf") {
    } else {
    cat(paste("Only values between 0 and 1 allowed for packing density!"))
    stop(domain=NA)
    }
  }
  
  if(sample.diameter < 0) {
    cat(paste("Please provide only positive integers."))
    stop(domain=NA)
  }
  
  ##==========================================================================##
  ## CALCULATIONS
  ##==========================================================================##
  
  # calculate the mean grain size
  if(length(grain.size) == 2) {
    grain.size = mean(grain.size)
  }
  
  if(packing.density == "inf") {
    packing.density = pi/sqrt(12)
  }
  
  # calculate the amount of grains on the aliquot
  if(missing(grains.counted) == TRUE) {
    n.grains<- ((pi*(sample.diameter/2)^2)/
                (pi*(grain.size/2000)^2))*packing.density
  }
  
  # calculate packing density
  if(missing(grains.counted) == FALSE) {
    
    area.container<- pi*sample.diameter^2
    
    if(length(grains.counted) == 1) {
      area.grains<- (pi*(grain.size/1000)^2)*grains.counted
      packing.density<- area.grains/area.container
    }
    else {
      packing.densities<- length(grains.counted)
      for(i in 1:length(grains.counted)) {
        area.grains<- (pi*(grain.size/1000)^2)*grains.counted[i]
        packing.densities[i]<- area.grains/area.container
      }
      std.d<- sd(packing.densities)
    }
  }
  
  ##==========================================================================##  
  ##TERMINAL OUTPUT
  ##==========================================================================##  
  
  cat("\n [calc_AliquotSize]")
  cat(paste("\n\n ---------------------------------------------------------"))
  cat(paste("\n mean grain size (microns)  :", grain.size))
  cat(paste("\n sample diameter (mm)       :", sample.diameter))
  if(missing(grains.counted) == FALSE) {
    if(length(grains.counted) == 1) {
      cat(paste("\n counted grains             :", grains.counted))
    } else {
      cat(paste("\n mean counted grains        :", round(mean(grains.counted))))
    }
  }
  if(missing(grains.counted) == TRUE) {
    cat(paste("\n packing density            :", round(packing.density,3)))
  } 
  if(missing(grains.counted) == FALSE) {
    if(length(grains.counted) == 1) {
      cat(paste("\n packing density            :", round(packing.density,3)))
    } else {
      cat(paste("\n mean packing density       :", round(mean(packing.densities),3)))
      cat(paste("\n standard deviation         :", round(std.d,3)))
    }
  }
  if(missing(grains.counted) == TRUE) {
    cat(paste("\n number of grains           :", round(n.grains,0)))
  }

  cat(paste("\n ---------------------------------------------------------\n"))
  
  ##==========================================================================##  
  ##RETURN VALUES
  ##==========================================================================##
  
   
  # prepare return values for mode: estimate grains
  if(missing(grains.counted) == TRUE) {
    results<- data.frame(grain.size = grain.size,
                         sample.diameter = sample.diameter,
                         packing.density = packing.density,
                         n.grains = round(n.grains,0),
                         grains.counted = NA)
  }
  
  # prepare return values for mode: estimate packing density/densities
  if(missing(grains.counted) == FALSE) {
      
    # return values if only one value for counted.grains is provided
    if(length(grains.counted) == 1) {
      results<- data.frame(grain.size = grain.size,
                           sample.diameter = sample.diameter,
                           packing.density = packing.density,
                           n.grains = NA,
                           grains.counted = grains.counted)
    } else { 
      # return values if more than one value for counted.grains is provided
      results<- data.frame(rbind(1:5))
      colnames(results)<- c("grain.size", "sample.diameter", "packing.density",
                            "n.grains","grains.counted")
      for(i in 1:length(grains.counted)) {
        results[i,]<- c(grain.size, sample.diameter, packing.densities[i],
                        n.grains = NA, grains.counted[i])
      }
    }
  }

  invisible(list(results=results))
  ### Returns terminal output. In addition a list is returned containing 
  ### the following element:
  ###
  ### \item{results}{data frame with calculation results.}
  
  ##details<<
  ## This function can be used to either estimate the number of grains
  ## on an aliquot or to compute the packing density depending on
  ## the arguments provided. \cr
  ## The following function is used to estimate the number of grains \code{n}:
  ## \cr
  ## \deqn{n = (\pi*x^2)/(\pi*y^2)*d}
  ## where \code{x} is the radius of the aliquot size (microns), \code{y}
  ## is the mean radius of the mineral grains (mm) and \code{d} is the packing
  ## density (value between 0 and 1). \cr\cr
  ## \bold{Packing density} \cr\cr
  ## The default value for \code{packing.density} is 0.65, which is the mean of
  ## empirical values determined by Heer et al. 2012 and unpublished data from
  ## the cologne luminescence laboratory. If \code{packing.density = "inf"}
  ## a maximum density of \eqn{\pi/\sqrt12 = 0.9068\ldots} is used. However,
  ## note that this value is not appropriate as the standard preparation
  ## procedure of aliquots resembles a PECC ("Packing Equal Circles in a 
  ## Circle") problem. 
  
  ##references<<
  ## Duller, G.A.T., 2008. Single-grain optical dating of Quaternary sediments: 
  ## why aliquot size matters in luminescence dating. Boreas 37, pp. 589-612. 
  ## \cr\cr
  ## Heer, A.J., Adamiec, G., Moska, P., 2012. How many grains are there on a 
  ## single aliquot?. Ancient TL, 30, pp. 9-16. \cr\cr
  ## \bold{Further reading} \cr\cr
  ## Chang, H.-C., Wang, L.-C., 2010. A simple proof of Thue's Theorem on 
  ## Circle Packing. \url{http://arxiv.org/pdf/1009.4322v1.pdf}, 2013-09-13.
  ## \cr\cr
  ## Graham, R.L., Lubachevsky, B.D., Nurmela, K.J., Oestergard, P.R.J., 1998. 
  ## Dense packings of congruent circles in a circle. Discrete Mathematics, 181, 
  ## pp. 139-154. \cr\cr
  ## Huang, W., Ye, T., 2011. Global optimization method for finding dense 
  ## packings of equal circles in a circle. European Journal of Operational 
  ## Research, 210, pp. 474-481. 
  
},ex=function(){  
  ## Estimate the amount of grains on a small aliquot
  calc_AliquotSize(grain.size = 125, sample.diameter = 1) 
  
  ## Calculate the mean packing density of large aliquots
  calc_AliquotSize(grain.size = 125, sample.diameter = 8, 
                   grains.counted = c(2525,2312,2880))
  
})#END OF STRUCTURE
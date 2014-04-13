plot_RLum<- structure(function(#General plot function for RLum S4 class objects
  ### Function calls object specific plot functions for RLum S4 class objects. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany), \cr
  
  ##section<<
  ## version 0.1
  # ===========================================================================

  object, 
  ### \code{\linkS4class{RLum}} (\bold{required}): S4 object of class \code{RLum}
  
  ...
  ### further arguments and graphical parameters that will be passed to the specific 
  ### plot functions
 
){
  
   # Integrity check ----------------------------------------------------------
  
   ##check if object is of class RLum.Data.Curve
   if(is(object, "RLum")==FALSE){
     
     stop("[plot_RLum]: Input object is not of class RLum or a derivative class")
     
   }
   
   
    ##grep object class
    object.class <- is(object)[1]
    
    ##select which plot function should be used
  
    switch (object.class,
            
            RLum.Data.Curve = plot_RLum.Data.Curve (object, ...),
            RLum.Data.Spectrum = plot_RLum.Data.Spectrum (object, ...),
            RLum.Analysis = plot_RLum.Analysis (object, ...)
  
           )


   # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
   
   ##details<<
   ## The function provides a generalised access point for plotting specific 
   ## \code{\linkS4class{RLum}} objects.\cr
   ## Depending on the input object, the corresponding plot function will be selected. 
   ## Allowed arguments can be found in the documentations of each plot function. 
   ## \tabular{lll}{
   ## \bold{object} \tab \tab \bold{corresponding plot function} \cr
   ##    
   ## \code{\linkS4class{RLum.Data.Curve}} \tab : \tab \code{\link{plot_RLum.Data.Curve}} \cr
   ## \code{\linkS4class{RLum.Analysis}} \tab : \tab \code{\link{plot_RLum.Analysis}}
   ## }
   
   ##value<<
   ## Returns a plot.
   
   ##references<<
   ## #
   
   ##note<<
   ## The provided plot output depends on the input object.
   
   ##seealso<<
   ## \code{\link{plot_RLum.Data.Curve}}, 
   ## \code{\linkS4class{RLum.Data.Curve}}
   
   ##keyword<<
   ## dplot
   
   
}, ex=function(){
  
  #load Example data
  data(ExampleData.CW_OSL_Curve, envir = environment())
  
  #transform data.frame to RLum.Data.Curve object
  temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")
  
  #plot RLum object 
  plot_RLum(temp)
  
})#END OF STRUCTURE

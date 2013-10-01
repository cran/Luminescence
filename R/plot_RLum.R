##//////////////////////////////////////////////////////////////////////////////
##//plot_Rlum.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#version: 0.1
#date: 2013-01-18
##==============================================================================
##major plot function for objects of the class RLum

plot_RLum <- function (object, ...){
  
   # Integrity check -----------------------------------------------------------------------------
  
   ##check if object is of class RLum.Data.Curve
   if(is(object, "RLum")==FALSE){
     
     stop("[plot_RLum]: Input object is not of class RLum or a derivative class")
     
   }
   
   
    ##grep object class
    object.class <- is(object)[1]
    
    ##select which plot function should be used
  
    switch (object.class,
            
            RLum.Data.Curve = plot_RLum.Data.Curve (object, ...),
            RLum.Analysis = plot_RLum.Analysis (object, ...)
  
           )


}

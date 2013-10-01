##//////////////////////////////////////////////////////////////////////////////
##//CW2pLM.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#version: 0.4
#date: 2013-03-27
##==============================================================================
##

CW2pLM <- function(values){
             
  # Integrity Checks -------------------------------------------------------- 
  
  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    
    stop("[CW2pLM] Error: 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!")
 
  }

  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){
 
    if(values@recordType != "OSL" & values@recordType != "IRSL"){
      
      stop(paste("[CW2pLM] Error: curve type ",values@recordType, "  is not allowed for the transformation!",
                 sep=""))
      
    }else{
      
      temp.values <- as(values, "data.frame")
      
    }
    
  }else{
    
    temp.values <- values
    
    
  }
  
  
  # Calculation -------------------------------------------------------------
  
  
            ##curve transformation
    					P<-2*max(temp.values[,1])
	  					u<-((2*temp.values[,1]*P)^0.5)
 								
	 					 ##cw >> plm conversion, according Bulur, 2000 
								temp.values[,2]<-temp.values[,2]*(u/P)
								temp.values<-data.frame(u,temp.values[,2])
            
  
  # Return values -----------------------------------------------------------
  
  ##returns the same data type as the input
  
  if(is(values, "data.frame") == TRUE){
    
    values <- temp.values
    return(values)
    
  }else{
    
    newRLumDataCurves.CW2pLM <- set_RLum.Data.Curve(recordType = values@recordType,
                                                    data = as.matrix(temp.values),
                                                    info = values@info)
    return(newRLumDataCurves.CW2pLM)                                                    
    
  }
  
}
##EOF

##//////////////////////////////////////////////////////////////////////////////
##//Second2Gray.R
##//////////////////////////////////////////////////////////////////////////////

##==============================================================================
#author: Sebastian Kreutzer*, Michael Dietze**, Margret C. Fuchs***
#organisation: *JLU Giessen, **TU Dresden, ***TU Bergakademie Freiberg
#vers.: 0.3
#date: 2013-02-15
##==============================================================================

Second2Gray<-function(values,
                      dose_rate,
                      method = "gaussian"
                      )
{
  
  De.seconds <- values[,1]
  De.error.seconds <- values[,2]
  
  De.gray <- NA
  De.error.gray <- NA
  
  De.gray <- round(De.seconds*dose_rate[1], digits=2) 
  
  if(method == "gaussian"){
    
    De.error.gray <- round(sqrt((De.seconds*dose_rate[2])^2+(dose_rate[1]*De.error.seconds)^2), digits=2)
    
  }else if (method == "absolute"){
        
    De.error.gray <- round(abs(dose_rate[1] * De.error.seconds) + abs(De.seconds * dose_rate[2]), digits=2)
    
  }else{
    
    stop("[Second2Gray] Error: unknown error calculation method!" )
    
  }
    
  values <- data.frame(De=De.gray, De.error=De.error.gray)
	return(values)
}

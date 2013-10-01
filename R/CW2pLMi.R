##//////////////////////////////////////////////////////////////////////////////
##//CW2pLMi.R with interpolation 
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#version: 0.3
#date: 2013-03-27
##==============================================================================
##Based on the paper of Bos and Wallinga, 2012 (Radiation Measurements) and the 
##personal comments and suggestions of Adrie Bos via e-mail

CW2pLMi<-function(values, P){

  # (0) Integrity checks -------------------------------------------------------
  
  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    
    stop("[CW2pLMi] Error: 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!")
    
  }
  
  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){
    
    if(values@recordType != "OSL" & values@recordType != "IRSL"){
      
      stop(paste("[CW2pLMi] Error: curve type ",values@recordType, "  is not allowed for the transformation!",
                 sep=""))
      
    }else{
      
      temp.values <- as(values, "data.frame")
    
    }
    
  }else{
    
    temp.values <- values
      
  }
  
  
  # (1) Transform values ------------------------------------------------------------------------                  								
    
    
    ##(a) log transformation of the CW-OSL count values
    CW_OSL.log<-log(temp.values[,2])
 
    ##(b) time transformation t >> t'
    t<-temp.values[,1]
    
      ##set P
      ##if no values for P is set selected a P value for a maximum of 
      ##two extrapolation points
      if(missing(P)==TRUE){
      
        i<-10
        P<-1/i
        t.transformed<-0.5*1/P*t^2

        while(length(t.transformed[t.transformed<min(t)])>2){
      
          P<-1/i
          t.transformed<-0.5*1/P*t^2
          i<-i+10
    
        }#end::while
      }else{
      
      if(P==0){stop("[CW2pLMi] Error: P has to be > 0!")}
      t.transformed<-0.5*1/P*t^2
      
     }
       #endif  
  
    # (2) Interpolation ---------------------------------------------------------------------------
    
    ##interpolate values, values beyond the range return NA values
    CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )
     
    ##combine t.transformed and CW_OSL.interpolated in a data.frame
    temp<-data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))
   
    ##Problem: I rare cases the interpolation is not working properely and Inf or NaN values are returned
    
       ##Fetch row number of the invalid values
       invalid_values.id<-c(which(is.infinite(temp[,2]) | is.nan(temp[,2])))
   
       ##interpolate between the lower and the upper value
       invalid_values.interpolated<-sapply(1:length(invalid_values.id), 
                    function(x) {
                       mean(c(temp[invalid_values.id[x]-1,2],temp[invalid_values.id[x]+1,2]))
                    }
       )
   
       ##replace invalid values in data.frame with newly interpolated values
       if(length(invalid_values.id)>0){
          temp[invalid_values.id,2]<-invalid_values.interpolated
       }
    
  # (3) Extrapolate first values of the curve ---------------------------------------------------
    
   
   ##(a) - find index of first rows which contain NA values (needed for extrapolation)
   temp.sel.id<-min(which(is.na(temp[,2])==FALSE))
      
   ##(b) - fit linear function
   fit.lm<-lm(y ~ x,data.frame(x=t[1:2],y=CW_OSL.log[1:2]))
    
   ##select values to extrapolate and predict (extrapolate) values based on the fitted function
   x.i<-data.frame(x=temp[1:(min(temp.sel.id)-1),1])
   y.i<-predict(fit.lm,x.i)
  
   ##replace NA values by extrapolated values
   temp[1:length(y.i),2]<-y.i
  
   ##set method values
   temp.method<-c(rep("extrapolation",length(y.i)),rep("interpolation",(length(temp[,2])-length(y.i))))
   
   ##print a warning message for more than two extrapolation points
   if(length(y.i)>2){warning("t' is beyond the time resolution and more than two data points have been extrapolated!")}
 
  # (4) Convert, transform and combine values --------------------------------------------------- 

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##transform CW-OSL values to pLM-OSL values
  pLM<-1/P*t*CW_OSL

  ##combine all values and exclude NA values
  temp.values <- data.frame(x=t,y.t=pLM,x.t=t.transformed, method=temp.method)
  temp.values <- na.exclude(temp.values)
 
  # (5) Return values ---------------------------------------------------------------------------  
  
  ##returns the same data type as the input
  if(is(values, "data.frame") == TRUE){
    
    values <- temp.values
    return(values)
    
  }else{
      
  
    ##add old info elements to new info elements
    temp.info <- c(values@info, 
                   CW2pLMi.x.t = list(temp.values$x.t),
                   CW2pLMi.method = list(temp.values$method)) 

    newRLumDataCurves.CW2pLMi <- set_RLum.Data.Curve(recordType = values@recordType,
                                                    data = as.matrix(temp.values[,1:2]),
                                                    info = temp.info)
    return(newRLumDataCurves.CW2pLMi)                                                    
    
  }

}
##EOF

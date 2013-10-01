##//////////////////////////////////////////////////////////////////////////////
##//CW2pPMi.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen
##version: 0.2
##date: 2013-03-27
##==============================================================================
##Based on the paper of Bos and Wallinga, 2012 (Radiation Measurements) and the 
##personal comments and suggestions of Adrie Bos via e-mail

CW2pPMi<-function(values, P){
  
  # (0) Integrity checks ------------------------------------------------------
  
  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    
    stop("[CW2pPMi] Error: 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!")
    
  }
  
  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){
    
    if(values@recordType != "OSL" & values@recordType != "IRSL"){
      
      stop(paste("[CW2pPMi] Error: curve type ",values@recordType, "  is not allowed for the transformation!",
                 sep=""))
      
    }else{
      
      temp.values <- as(values, "data.frame")
      
    }
    
  }else{
    
    temp.values <- values
    
  }
  

  # (3) Transform values ------------------------------------------------------                  								
    
    ##log transformation of the CW-OSL count values
    CW_OSL.log<-log(temp.values[,2])
  
    ##time transformation t >> t'
    t<-temp.values[,1]
    
    ##set P
    ##if no values for P is set selected a P value for a maximum of 
    ##two extrapolation points
    if(missing(P)==TRUE){
      
       i<-1
       P<-1/i
       t.transformed<-(1/3)*(1/P^2)*t^3
 
       while(length(t.transformed[t.transformed<min(t)])>2){
       
          P<-1/i
          t.transformed<-(1/3)*(1/P^2)*t^3
          i<-i+1
       
       }
       }else{
        
          t.transformed<-(1/3)*(1/P^2)*t^3
      
       }  

  # (4) Interpolation ---------------------------------------------------------

 
   ##interpolate values, values beyond the range return NA values
   CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )
    
   ##combine t.transformed and CW_OSL.interpolated in a data.frame
   temp<-data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))
  
    
  # (5) Extrapolate first values of the curve ---------------------------------
    
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
    if(temp.sel.id>2){warning("t' is beyond the time resolution. Only two data points have been extrapolated, the first ",temp.sel.id-3, " points have been set to 0!")}
  
  # (6) Convert, transform and combine values --------------------------------- 

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##transform CW-OSL values to pPM-OSL values
  
  pPM<-(t^2/P^2)*CW_OSL

  ##combine all values and exclude NA values
  temp.values <- data.frame(x=t, y.t=pPM, x.t=t.transformed, method=temp.method)
  temp.values <- na.exclude(temp.values)

  # (7) Return values ---------------------------------------------------------  

  ##returns the same data type as the input
  if(is(values, "data.frame") == TRUE){
    
    values <- temp.values
    return(values)
    
  }else{
    
    
    ##add old info elements to new info elements
    temp.info <- c(values@info, 
                   CW2pPMi.x.t = list(temp.values$x.t),
                   CW2pPMi.method = list(temp.values$method)) 
    
    newRLumDataCurves.CW2pPMi <- set_RLum.Data.Curve(recordType = values@recordType,
                                                     data = as.matrix(temp.values[,1:2]),
                                                     info = temp.info)
    return(newRLumDataCurves.CW2pPMi)                                                    
    
  }

}
# ##EOF

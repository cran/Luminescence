##//////////////////////////////////////////////////////////////////////////////
##//CW2pHMi.R 
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen
##version: 0.2
##date: 2013-03-27
##==============================================================================
##Based on the paper of Bos and Wallinga, 2012 (Radiation Measurements) and the 
##personal comments and suggestions of Adrie Bos via e-mail

CW2pHMi<-function(values, delta){

  ##(1) data.frame or RLum.Data.Curve object?
  if(is(values, "data.frame") == FALSE & is(values, "RLum.Data.Curve") == FALSE){
    
    stop("[CW2pHMi] Error: 'values' object has to be of type 'data.frame' or 'RLum.Data.Curve'!")
    
  }
  
  ##(2) if the input object is an 'RLum.Data.Curve' object check for allowed curves
  if(is(values, "RLum.Data.Curve") == TRUE){
    
    if(values@recordType != "OSL" & values@recordType != "IRSL"){
      
      stop(paste("[CW2pHMi] Error: curve type ",values@recordType, "  is not allowed for the transformation!",
                 sep=""))
      
    }else{
      
      temp.values <- as(values, "data.frame")
      
    }
    
  }else{
    
    temp.values <- values
    
  }
 
  
  # (1) Transform values ------------------------------------------------------                  								
    
    ##log transformation of the CW-OSL count values
    CW_OSL.log<-log(temp.values[,2])
  
    ##time transformation t >> t'
    t<-temp.values[,1]

    ##set delta
    ##if no values for delta is set selected a delta value for a maximum of 
    ##two extrapolation points
    if(missing(delta)==TRUE){
       
       i<-10
       delta<-i
       t.transformed<-t-(1/delta)*log(1+delta*t)
 
       while(length(t.transformed[t.transformed<min(t)])>2){
       
         delta<-i
         t.transformed<-t-(1/delta)*log(1+delta*t)
         i<-i+10
         
       }
     }else{
     
    t.transformed<-t-(1/delta)*log(1+delta*t)
    
   }  

  # (2) Interpolation ---------------------------------------------------------

   ##interpolate values, values beyond the range return NA values
   CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )
    
   ##combine t.transformed and CW_OSL.interpolated in a data.frame
   temp<-data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))
  
   ##Problem: I some cases the interpolation algorithm is not working properely and Inf or NaN values are returned
  
      ##fetch row number of the invalid values
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
    
  # (3) Extrapolate first values of the curve ---------------------------------
    
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
  
  # (4) Convert, transform and combine values --------------------------------- 

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##set values for c and P
  
    ##P is the stimulation period
    P<-max(temp.values[,1])

    ##c is a dimensionless constant
    c<-(1+(delta*P))/(delta*P)

  ##transform CW-OSL values to pLM-OSL values
  pHM<-((delta*t)/(1+(delta*t)))*c*CW_OSL

  ##combine all values and exclude NA values
  temp.values<-data.frame(x=t,y.t=pHM,x.t=t.transformed,method=temp.method)
  temp.values<-na.exclude(temp.values)

  # (5) Return values ---------------------------------------------------------  
  
  ##returns the same data type as the input
  if(is(values, "data.frame") == TRUE){
    
    values <- temp.values
    return(values)
    
  }else{
    
    
    ##add old info elements to new info elements
    temp.info <- c(values@info, 
                   CW2pHMi.x.t = list(temp.values$x.t),
                   CW2pHMi.method = list(temp.values$method)) 
    
    newRLumDataCurves.CW2pHMi <- set_RLum.Data.Curve(recordType = values@recordType,
                                                     data = as.matrix(temp.values[,1:2]),
                                                     info = temp.info)
    return(newRLumDataCurves.CW2pHMi)                                                    
    
  }
  
}
##EOF

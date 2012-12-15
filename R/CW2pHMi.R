##//////////////////////////////////////////////
##//pHM Transformation.R with interpolation 
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.2
#date: 15/12/2012
##======================================
##Based on the paper of Bos and Wallinga, 2012 (Radiation Measurements) and the 
##personal comments and suggestions of Adrie Bos via e-mail


CW2pHMi<-function(values, delta){

  # (0) Integrity checks ------------------------------------------------------------------------
  if(is.data.frame(values)==FALSE){stop("[CW2pHMi] >> Input object is not of type data.frame!")}

  # (1) Transform values ------------------------------------------------------------------------                  								
    
    ##log transformation of the CW-OSL count values
    CW_OSL.log<-log(values[,2])
  
    ##time transformation t >> t'
    t<-values[,1]

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

  # (2) Interpolation ---------------------------------------------------------------------------

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

  ##set values for c and P
  
    ##P is the stimulation period
    P<-max(values[,1])

    ##c is a dimensionless constant
    c<-(1+(delta*P))/(delta*P)

  ##transform CW-OSL values to pLM-OSL values
  pHM<-((delta*t)/(1+(delta*t)))*c*CW_OSL

  ##combine all values and exclude NA values
  values<-data.frame(x=t,y.t=pHM,x.t=t.transformed,method=temp.method)
  values<-na.exclude(values)

  # (5) Return values ---------------------------------------------------------------------------  
  
  return(values)
}
##EOF

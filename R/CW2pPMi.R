##//////////////////////////////////////////////
##//pPM Transformation.R with interpolation 
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.1
#date: 03/11/2012
##======================================
##Based on the paper of Bos and Wallinga, 2012 (Radiation Measurements) and the 
##personal comments and suggestions of Adrie Bos via e-mail

CW2pPMi<-function(values, P){

  # (0) Integrity checks ------------------------------------------------------------------------
  if(is.data.frame(values)==FALSE){stop("[CW2pPMi] >> Input object is not of type data.frame!")}

  # (1) Transform values ------------------------------------------------------------------------                  								
    
    ##log transformation of the CW-OSL count values
    CW_OSL.log<-log(values[,2])
  
    ##time transformation t >> t'
    t<-values[,1]
    
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

  # (2) Interpolation ---------------------------------------------------------------------------

 
   ##interpolate values, values beyond the range return NA values
   CW_OSL.interpolated<-approx(t,CW_OSL.log, xout=t.transformed, rule=1 )
    
   ##combine t.transformed and CW_OSL.interpolated in a data.frame
   temp<-data.frame(x=t.transformed, y=unlist(CW_OSL.interpolated$y))
  
    
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
    if(temp.sel.id>2){warning("t' is beyond the time resolution. Only two data points have been extrapolated, the first ",temp.sel.id-3, " points have been set to 0!")}
  
  # (4) Convert, transform and combine values --------------------------------------------------- 

  ##unlog CW-OSL count values, i.e. log(CW) >> CW
  CW_OSL<-exp(temp$y)

  ##transform CW-OSL values to pPM-OSL values
  
  pPM<-(t^2/P^2)*CW_OSL

  ##combine all values and exclude NA values
  values<-data.frame(x=t,y.t=pPM,x.t=t.transformed,method=temp.method)
  values<-na.exclude(values)

  # (5) Return values ---------------------------------------------------------------------------  

  return(values)
}
# ##EOF




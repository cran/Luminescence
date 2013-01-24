##//////////////////////////////////////////////
##//Calc_OSLLxTxRatio.R
##/////////////////////////////////////////////
##
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.2.1
#date: 24/01/2013
##======================================
##calculation of the LxTx ratio including error calculation

Calc_OSLLxTxRatio<-function(Lx.data,
                            Tx.data,
                            signal.integral,
                            background.integral  
  
){
   
  
  ##------------------------------------------------------------------------------------------------##
  ##(1) - a few integrity check 
  
   if(missing(Tx.data)==FALSE){
     
     ##(a) - check data type
     if(is(Lx.data)[1]!=is(Tx.data)[1]){
       stop("[Calc_OSLLxTxRatio.R] >> Error: Data type of Lx and Tx data differs!")}
   
     ##(b) - test if data.type is valid in general
     if(is(Lx.data)[1]!="data.frame" & is(Lx.data)[1]!="numeric"){
       stop("[Calc_OSLLxTxRatio.R] >> Error: Data type error! Required types are data.frame or numeric vector.")}
      
     ##(c) - convert vector to data.frame if nescessary
     if(is(Lx.data)[1]!="data.frame"){
       Lx.data<-data.frame(x=1:length(Lx.data),y=Lx.data)
       Tx.data<-data.frame(x=1:length(Tx.data),y=Tx.data)
     }
   
     ##(d) - check if Lx and Tx curves have the same channel length
     if(length(Lx.data[,2])!=length(Tx.data[,2])){
       stop("[Calc_OSLLxTxRatio.R] >> Error: Channel number of Lx and Tx data differs!")}
   
   }else{

    Tx.data<-data.frame(x=NA,y=NA)
    if(is(Lx.data)[1]!="data.frame"){Lx.data<-data.frame(x=1:length(Lx.data),y=Lx.data)}   
         
   }#endif::missing Tx.data
   
   ##(e) - check if signal integral is valid
   if(min(signal.integral)<1 | max(signal.integral>length(Lx.data[,2]))){
     stop("[Calc_OSLLxTxRatio.R] >> Error: signal.integral is not valid!")}

   ##(f) - check if background integral is valid
   if(min(background.integral)<1 | max(background.integral>length(Lx.data[,2]))){
     stop(paste("[Calc_OSLLxTxRatio.R] >> Error: background.integral is not valid! Max: ",length(Lx.data[,2]),sep=""))}
   
   ##(g) - check if signal and background integral overlapping
   if(min(background.integral)<=max(signal.integral)){
     stop("[Calc_OSLLxTxRatio.R] >> Error: Overlapping of signal.integral and background.integral is not permitted!")}
   ##
  ##------------------------------------------------------------------------------------------------##
  ##------------------------------------------------------------------------------------------------##
  ##(2) - read data and produce background subracted values 
   
   Lx.curve<-Lx.data[,2] 
   Lx.signal<-sum(Lx.curve[signal.integral])
   Lx.background<-sum(Lx.curve[background.integral])/(length(background.integral)/length(signal.integral))
   LnLx<-(Lx.signal-Lx.background)
   
   if(is.na(Tx.data[1,1])==FALSE){Tx.curve<-Tx.data[,2]}else{Tx.curve<-NA}
     Tx.signal<-sum(Tx.curve[signal.integral])
     Tx.background<-sum(Tx.curve[background.integral])/(length(background.integral)/length(signal.integral))
     TnTx<-(Tx.signal-Tx.background)                         
     
  ##------------------------------------------------------------------------------------------------##
  ##------------------------------------------------------------------------------------------------##
  ##(3) calulate Lx/Tx Errors according Galbraith (2002) follwing also the nomenclator for the ease of use
  ## -- results checked with the analyst an they are comparable! (29/10/2011)    
  
  ##(a)
  ##calculate Y.0 - which is the sum OSL signal including the background
  Y.0<-Lx.signal
  Y.0_TnTx<-Tx.signal
  
  ##(b)
  ##calculate k value - express the background as mutiple value from the number of signal channels 
  k<-length(background.integral)/length(signal.integral)
  
  
  ##(c)
  ##calculate background over n channels (k sets); note that m=n*k = background.integral
  Y.i<-sapply(0:round(k,digits=0), function(i){
    Y.i<-sum(Lx.curve[(min(background.integral)+length(signal.integral)*i):(min(background.integral)+length(signal.integral)+length(signal.integral)*i)])
  })      
  ##also for the test signal   
  Y.i_TnTx<-sapply(0:round(k,digits=0), function(i){
    Y.i<-sum(Tx.curve[(min(background.integral)+length(signal.integral)*i):(min(background.integral)+length(signal.integral)+length(signal.integral)*i)])
  })
  
  ##(d)
  ##calculate the variance of the background over the k sets; exlcude NA values
  Y.var<-var(Y.i,na.rm=TRUE)
  Y.var_TnTx<-var(Y.i_TnTx,na.rm=TRUE)
  
  ##(e)
  ##calculate average background for the k sets    
  Y.mean<-sum(na.exclude(Y.i))/round(k,digits=0)
  Y.mean_TnTx<-sum(na.exclude(Y.i_TnTx))/round(k,digits=0)
  
  ##(f)
  ##calculate difference from variance and mean background counts
  Y.sigma<-abs(Y.var-Y.mean)
  Y.sigma_TnTx<-abs(Y.var_TnTx-Y.mean_TnTx)
  
  ##(g)
  ##calculate error after equation 7 in Gablraith 2002        
  LnLx.relError<-sqrt(1+(Y.sigma/Y.mean))*(sqrt(Y.0+(Y.mean/k))/(Y.0-Y.mean))
  TnTx.relError<-sqrt(1+(Y.sigma_TnTx/Y.mean_TnTx))*(sqrt(Y.0_TnTx+(Y.mean_TnTx/k))/(Y.0_TnTx-Y.mean_TnTx))
  
  ##(h)
  ##calculate absolute standard error
  LnLx.Error<-abs(LnLx*LnLx.relError)
  TnTx.Error<-abs(TnTx*TnTx.relError)

  ##combine results
  LnLxTnTx<-cbind(Lx.signal,Lx.background,Tx.signal,Tx.background,LnLx,LnLx.Error,TnTx,TnTx.Error)
  ##------------------------------------------------------------------------------------------------##
  ##------------------------------------------------------------------------------------------------##
  ##(4) Calculate LxTx error with gaussian error propagation and combine results

  #transform results in a data.frame
  LnLxTnTx<-as.data.frame((LnLxTnTx))

  #add col names
  colnames(LnLxTnTx)<-c("LnLx","LnLx.BG","TnTx","TnTx.BG","Net_LnLx","Net_LnLx.Error","Net_TnTx","Net_TnTx.Error")
  
  ##calculate Ln/Tx
  LxTx<-LnLxTnTx$Net_LnLx/LnLxTnTx$Net_TnTx

  ##calculate Ln/Tx error
  LxTx.Error<-sqrt(((1/LnLxTnTx$Net_TnTx)*LnLxTnTx$Net_LnLx.Error)^2+((LnLxTnTx$Net_LnLx/-LnLxTnTx$Net_TnTx^2)*LnLxTnTx$Net_TnTx.Error)^2)

  ##return combined values
  return(cbind(LnLxTnTx,LxTx,LxTx.Error))
   
}#end function

calc_OSLLxTxRatio<- structure(function(#Calculate Lx/Tx ratio for CW-OSL curves.
  ### Calculate Lx/Tx ratios from two given OSL curves. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.3.1
  # ===========================================================================
  
  Lx.data, 
  ### \link{data.frame} (\bold{required}): requires a CW-OSL shine down curve 
  ### (x = time, y = counts)
  
  Tx.data,
  ### \link{data.frame} (optional): requires a CW-OSL shine down curve 
  ### (x = time, y = counts). If no input is given the Tx.data will 
  ### be treated as \code{NA} and no Lx/Tx ratio is calculated.
  
  signal.integral,
  ### \link{vector} (\bold{required}): vector with the limits 
  ### for the signal integral.
  
  background.integral  
  ### \link{vector} (\bold{required}): vector with the bounds for the 
  ### background integral.
  
){
   
  
  ##--------------------------------------------------------------------------##
  ##(1) - a few integrity checks 
  
   if(missing(Tx.data)==FALSE){
     
     ##(a) - check data type
     if(is(Lx.data)[1]!=is(Tx.data)[1]){
       stop("[calc_OSLLxTxRatio.R] >> Error: Data type of Lx and Tx data differs!")
     }
   
     ##(b) - test if data.type is valid in general
     if((is(Lx.data)[1] != "data.frame" & 
         is(Lx.data)[1] != "numeric") & 
         is(Lx.data)[1] != "matrix"){
       stop("[calc_OSLLxTxRatio.R] >> Error: Data type error! Required types are data.frame or numeric vector.")
     }
      
     ##(c) - convert vector to data.frame if nescessary
     if(is(Lx.data)[1] != "data.frame" & 
        is(Lx.data)[1] != "matrix"){
       Lx.data <- data.frame(x=1:length(Lx.data),y=Lx.data)
       Tx.data <- data.frame(x=1:length(Tx.data),y=Tx.data)
     }
   
     ##(d) - check if Lx and Tx curves have the same channel length
     if(length(Lx.data[,2]) != length(Tx.data[,2])){
       stop("[calc_OSLLxTxRatio.R] Error: Channel number of Lx and Tx data differs!")}
   
   }else{

    Tx.data<-data.frame(x=NA,y=NA)
    if(is(Lx.data)[1]!="data.frame"){Lx.data<-data.frame(x=1:length(Lx.data),y=Lx.data)}   
         
   }#endif::missing Tx.data
   
   ##(e) - check if signal integral is valid
   if(min(signal.integral)<1 | max(signal.integral>length(Lx.data[,2]))){
     stop("[calc_OSLLxTxRatio.R] Signal.integral is not valid!")}

   ##(f) - check if background integral is valid
   if(min(background.integral)<1 | max(background.integral>length(Lx.data[,2]))){
     stop(paste("[calc_OSLLxTxRatio.R] Background.integral is not valid! Max: ",length(Lx.data[,2]),sep=""))}
   
   ##(g) - check if signal and background integral overlapping
   if(min(background.integral)<=max(signal.integral)){
     stop("[calc_OSLLxTxRatio.R] Overlapping of 'signal.integral' and 'background.integral' is not permitted!")}
   
#    ##(h) - check if signal > background integral 
#    if(length(background.integral)<length(signal.integral)){
#      stop("[calc_OSLLxTxRatio.R] Signal integral > background integral. This is currently not supported.")}

  ##--------------------------------------------------------------------------##
  ##(2) - read data and produce background subtracted values 
   
   Lx.curve <- Lx.data[,2] 
   Lx.signal <- sum(Lx.curve[signal.integral])
   Lx.background <- sum(
     Lx.curve[background.integral])/(
       length(background.integral)/length(signal.integral))
   LnLx <- (Lx.signal-Lx.background)
   
   if(is.na(Tx.data[1,1])==FALSE){Tx.curve<-Tx.data[,2]}else{Tx.curve<-NA}
  
     Tx.signal <- sum(Tx.curve[signal.integral])
     Tx.background<-sum(
       Tx.curve[background.integral])/(
         length(background.integral)/length(signal.integral))
     TnTx <- (Tx.signal-Tx.background)                         
     
  ##--------------------------------------------------------------------------##
  ##(3) 
  ## calulate Lx/Tx Errors according Galbraith (2002) follwing also the 
  ## nomenclator for the ease of use
  ## -- results checked with the analyst and they are comparable! (29/10/2011)    
  
  ##(a)
  ##calculate Y.0 - which is the sum OSL signal including the background
  Y.0 <- Lx.signal
  Y.0_TnTx <- Tx.signal
  
  ##(b)
  ## calculate k value - express the background as mutiple value from the number 
  ## of signal channels 
  n <- length(signal.integral) 
  m <- length(background.integral)
  k <- m/n

  ##(c)(1)
  ## calculate background over n channels (k sets); 
  ## note that m=n*k = multiple background.integral from signal.integral
  Y.i <- sapply(0:round(k,digits=0), function(i){
    sum(Lx.curve[
      (min(background.integral)+length(signal.integral)*i):
      (min(background.integral)+length(signal.integral)+length(signal.integral)*i)])
  })      
  
   
  ##(c)(2)
  ## also for the test signal   
  Y.i_TnTx <- sapply(0:round(k,digits=0), function(i){
    sum(Tx.curve[
      (min(background.integral)+length(signal.integral)*i):
      (min(background.integral)+length(signal.integral)+length(signal.integral)*i)])
  })
  
  ##(d)
  ## calculate the variance of the background over the k sets; exlcude NA values
  Y.var <- var(Y.i, na.rm = TRUE)
  Y.var_TnTx <- var(Y.i_TnTx, na.rm = TRUE)
   
  ##(e)
  ## calculate average background for the k sets    
  Y.mean <- sum(na.exclude(Y.i))/round(k,digits=0)
  Y.mean_TnTx <- sum(na.exclude(Y.i_TnTx))/round(k,digits=0)
  
   
  ##(f)
  ##calculate difference from variance and mean background counts
  Y.sigma <- abs(Y.var-Y.mean)
  Y.sigma_TnTx <- abs(Y.var_TnTx-Y.mean_TnTx)
  
  ##(g)
  ##calculate error according equation (7) in Gablraith 2002        
  LnLx.relError <- sqrt(1+(Y.sigma/Y.mean))*(sqrt(Y.0+(Y.mean/k))/(Y.0-Y.mean))
  TnTx.relError <- sqrt(1+(Y.sigma_TnTx/Y.mean_TnTx))*(sqrt(Y.0_TnTx+(Y.mean_TnTx/k))/(Y.0_TnTx-Y.mean_TnTx))

   
  ##(h)
  ##calculate absolute standard error
  LnLx.Error <- abs(LnLx*LnLx.relError)
  TnTx.Error <- abs(TnTx*TnTx.relError)

  ##combine results
  LnLxTnTx<-cbind(Lx.signal,
                  Lx.background,
                  Tx.signal,
                  Tx.background,
                  LnLx, LnLx.Error,
                  TnTx, TnTx.Error)
   
  ##--------------------------------------------------------------------------##
  ##--------------------------------------------------------------------------##
  ##(4) Calculate LxTx error with gaussian error propagation and combine results

  #transform results in a data.frame
  LnLxTnTx <- as.data.frame((LnLxTnTx))


  #add col names
  colnames(LnLxTnTx)<-c("LnLx","LnLx.BG","TnTx","TnTx.BG","Net_LnLx","Net_LnLx.Error","Net_TnTx","Net_TnTx.Error")
  
  ##calculate Ln/Tx
  LxTx <- LnLxTnTx$Net_LnLx/LnLxTnTx$Net_TnTx

  ##calculate Ln/Tx error
  LxTx.Error<-sqrt(((1/LnLxTnTx$Net_TnTx)*LnLxTnTx$Net_LnLx.Error)^2+((LnLxTnTx$Net_LnLx/-LnLxTnTx$Net_TnTx^2)*LnLxTnTx$Net_TnTx.Error)^2)

  ##return combined values
  temp <- cbind(LnLxTnTx,LxTx,LxTx.Error)
   
  temp.return <- new("RLum.Results", 
                     originator = "calc_OSLLxTxRatio",
                     data = temp)

  return(temp.return)
  ### 
   
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## The integrity of the chosen values for the signal and background integral 
  ## is checked by the function; 
  ## the signal integral limits have to be lower than the background integral limits. 
  ## If a \link{vector} is given as input instead of a \link{data.frame}, 
  ## an artificial \code{data.frame}
  ## is produced. The error calculation is done according to Galbraith (2002).
   
  ##value<<
  ## Returns an S4 object of type \code{\linkS4class{RLum.Results}}. 
  ## Slot \code{data} contains a \link{list} with the following structure:\cr 
  ## $ LnLx  \cr        
  ## $ LnLx.BG   \cr     
  ## $ TnTx    \cr       
  ## $ TnTx.BG    \cr   
  ## $ Net_LnLx   \cr   
  ## $ Net_LnLx.Error\cr 
  ## $ Net_TnTx.Error\cr
  ## $ LxTx\cr
  ## $ LxTx.Error
   
  ##references<<
  ## Duller, G., 2007. Analyst. \url{http://www.nutech.dtu.dk/english/~/media/Andre_Universitetsenheder/Nutech/Produkter%20og%20services/Dosimetri/radiation_measurement_instruments/tl_osl_reader/Manuals/analyst_manual_v3_22b.ashx}\cr
  ##
  ## Galbraith, R.F., 2002. A note on the variance of a background-corrected 
  ## OSL count. Ancient TL, 20 (2), 49-51. 
  
  ##note<<
  ## The results of this function have been cross-checked with the Analyst (vers. 3.24b).   
  ## Access to the results object via \code{get_RLum.Results}.
   
  ##seealso<<
  ## \code{\link{Analyse_SAR.OSLdata}}, \code{\link{plot_GrowthCurve}}, 
  ## \code{\link{analyse_SAR.CWOSL}}
   
  ##keyword<<
  ## datagen
   
   
}, ex=function(){

  ##load data
  data(ExampleData.LxTxOSLData, envir = environment())
  
  ##calculate Lx/Tx ratio
  results <- calc_OSLLxTxRatio(Lx.data, Tx.data, signal.integral = c(1:2), 
                               background.integral = c(85:100))

})#END OF STRUCTURE
##//////////////////////////////////////////////////////////////////////////////
##//plot_Rlum.Analysis.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
##version: 0.1
##date: 2013-09-06
##==============================================================================

plot_RLum.Analysis <- function(object, 
                              nrows = 3,    
                              ncols = 2, 
                              ...){
  
  # Integrity check ----------------------------------------------------------------------------
  
  ##check if object is of class RLum.Data.Curve
  if(is(object,"RLum.Analysis") == FALSE){
    
    stop("[plot_RLum.Analysis]: Single input objects are not of type 'RLum.Data.Curve'")
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  ##main
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {""}

  ##mtext
  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
  {""}
  
  # Plotting ------------------------------------------------------------------------------------
  
      ##grep RLum.Data.Curve objects 
      temp <- lapply(1:length(object@records), function(x){
                
                if(is(object@records[[x]], "RLum.Data.Curve") == TRUE){
                  
                  object@records[[x]]
                  
                }})
              
     
      ##calculate number of pages for mtext
      if(length(temp)%%(nrows*ncols)>0){
        
        n.pages <- round(length(temp)/(nrows*ncols), digits=0)+1
        
      }else{
        
        n.pages <- length(temp)/(nrows*ncols)
        
      }
 
      ##set par
      par(mfrow=c(nrows,ncols))          
  
      ##plot curves
      for(i in 1:length(temp)){
                
            plot_RLum.Data.Curve(temp[[i]], 
                 col = if(grepl("IRSL", temp[[i]]@recordType) == TRUE){"red"} else 
                       if(grepl("OSL", temp[[i]]@recordType) == TRUE){"blue"} else
                       {"black"},
                     mtext = paste("#",i,sep=""),
                     par.local = FALSE,
                     main = if(main==""){temp[[i]]@recordType}else{main})           
               
           
           if(i%%(nrows*ncols)==0){
             mtext(mtext, outer = TRUE, side=3, line=-2)
           }
        }     
        
  
}

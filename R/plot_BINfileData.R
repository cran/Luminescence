##/////////////////////////////////////////////////////////////////////////////
##//plot_BINfileData.R
##/////////////////////////////////////////////////////////////////////////////
##=============================================================================#
##author: Sebastian Kreutzer*, Michael Dietze**
##organisation: *JLU Giessen, **TU Dresden
##version: 0.4
##date: 2013-03-05
##=============================================================================#
##
## plot luminescence curves from bin file

plot_BINfileData<-function(BINfileData, #input BIN file
                          position, 
                          run,
                          set,
                          sorter="POSITION", #sort by POSITION or RUN
                          ltype=c("IRSL","OSL","TL","RIR","RBR"),
                          curve.transformation,                            
                          dose_rate,
                          temp.lab="deg. C", 
                          cex.global=1
                        ){
  
     
    ##check if the object is of type Risoe.BINfileData
    if(class(BINfileData)!="Risoe.BINfileData"){stop("Wrong object! Object of type Risoe.BINfileData needed.")}
  
    temp<-BINfileData
    
# Missing check -------------------------------------------------------------------------------  

    ##set plot position if missing
    if(missing(position)==TRUE){position<-c(min(temp@METADATA[,"POSITION"]):max(temp@METADATA[,"POSITION"]))}    
    if(missing(run)==TRUE){run<-c(min(temp@METADATA[,"RUN"]):max(temp@METADATA[,"RUN"]))}    
    if(missing(set)==TRUE){set<-c(min(temp@METADATA[,"SET"]):max(temp@METADATA[,"SET"]))}
    

# Ordering ------------------------------------------------------------------------------------

    ##(1) order by RUN, SET OR BY POSITION
    if(sorter=="RUN"){
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"RUN"]),]
    }else if(sorter=="SET"){    
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"SET"]),]  
    }else {
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"POSITION"]),]      
    }



# Select values for plotting ------------------------------------------------------------------

    ##(2) set SEL for selected position
    
        ##set all to FALSE
        temp@METADATA[,"SEL"]<-FALSE
    
        ##set TRUE 
        temp@METADATA[(temp@METADATA[,"POSITION"] %in% position)==TRUE & 
                      (temp@METADATA[,"RUN"] %in% run)==TRUE &
                      (temp@METADATA[,"SET"] %in% set)==TRUE &
                      (temp@METADATA[,"LTYPE"] %in% ltype)==TRUE,"SEL"]<-TRUE
       
    ##---------------------------------------------------------------------------------------------##
    ##PLOTTING
    ##---------------------------------------------------------------------------------------------##
    ##(3) plot curves
    for(i in 1:length(temp@METADATA[,"ID"])){
   
     ##print only if SEL == TRUE
     if(temp@METADATA[i,"SEL"]==TRUE)
     {
      
      ##find measured unit
      measured_unit<-if(temp@METADATA[i,"LTYPE"]=="TL"){" deg. C"}else{"s"} 
      
      ##set x and y values
      values.x <- seq(temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"],
                      temp@METADATA[i,"HIGH"],by=temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"])
      values.y <- unlist(temp@DATA[temp@METADATA[i,"ID"]])      
      values.xy <- data.frame(values.x, values.y)
      
      ##set curve transformation if wanted
      if((temp@METADATA[i,"LTYPE"] == "OSL" | temp@METADATA[i,"LTYPE"] == "IRSL") &
          missing(curve.transformation) == FALSE){    
  
        if(curve.transformation=="CW2pLM"){
          
          values.xy <- CW2pLM(values.xy)
          
        }else if(curve.transformation=="CW2pLMi"){
          
          values.xy <- CW2pLMi(values.xy)[,1:2]
          
        }else if(curve.transformation=="CW2pHMi"){
          
          values.xy <- CW2pHMi(values.xy)[,1:2]
          
        }else if(curve.transformation=="CW2pPMi"){
        
         values.xy <- CW2pPMi(values.xy)[,1:2]
        
        }else{
          
         warning("Function for curve.transformation is unknown. No transformation is performed.")
          
        }  
        
      }
              
      ##plot graph 
      plot(values.xy,
           main=paste("pos=", temp@METADATA[i,"POSITION"],", run=", temp@METADATA[i,"RUN"],
                      ", set=", temp@METADATA[i,"SET"],sep=""
                      ),
           type="l",
           ylab=paste(temp@METADATA[i,"LTYPE"]," [cts/",round(temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"],digits=3)," ",
                      measured_unit,"]",sep=""),
           xlab=if(measured_unit==" deg. C"){paste("temp. [",temp.lab,"]",sep="")}else{"time [s]"},
           col=if(temp@METADATA[i,"LTYPE"]=="IRSL" | temp@METADATA[i,"LTYPE"]=="RIR"){"red"}
               else if(temp@METADATA[i,"LTYPE"]=="OSL" | temp@METADATA[i,"LTYPE"]=="RBR"){"blue"}
               else{"black"},
           sub=if(temp@METADATA[i,"LTYPE"]=="TL"){paste("(",temp@METADATA[i,"RATE"]," K/s)",sep="")}else{},           
           lwd=1.2*cex.global,
           cex=0.9*cex.global
      )
      
      ##add mtext for temperature
      
      ##grep temperature (different for different verions)
      
      temperature<-if(temp@METADATA[i,"VERSION"]=="03"){temp@METADATA[i,"AN_TEMP"]}
                   else{temp@METADATA[i,"TEMPERATURE"]}
      
      ##mtext
      mtext(side=3, 
            if(temp@METADATA[i,"LTYPE"]=="TL"){paste("TL to ",temp@METADATA[i,"HIGH"], " ",temp.lab,sep="")}
            else{paste(temp@METADATA[i,"LTYPE"],"@",temperature," ",temp.lab ,sep="")},          
            cex=0.9*cex.global)      
      
     ##add mtext for irradiation
     mtext(side=4,cex=0.8*cex.global, line=0.5,
           if(temp@METADATA[i, "IRR_TIME"]!=0){
             
             if(missing("dose_rate")==TRUE){
               paste("dose = ",temp@METADATA[i, "IRR_TIME"], " s", sep="") 
             }else{
               paste("dose = ",temp@METADATA[i, "IRR_TIME"]*dose_rate, " Gy", sep="") 
             }
           }
         )#end mtext
      
     }#endif::selection            
   }#endforloop
}#EOF

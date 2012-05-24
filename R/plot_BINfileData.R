##//////////////////////////////////////////////
##//plot_BINfileData.R
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.1.4
#date: 24/04/2012
##======================================
##
## plot luminescence curves from bin file

plot_BINfileData<-function(BINfileData, #input BIN file
                          position, #set 
                          sorter="POSITION", #sort by POSITION or RUN
                          ltype=c("IRSL","OSL","TL","RIR","RBR"),
                          cex.global=1
                        ){
  
     
    ##check if the object is of type Risoe.BINfileData
    if(class(BINfileData)!="Risoe.BINfileData"){stop("Wrong object! Object of type Risoe.BINfileData needed.")}
  
    temp<-BINfileData
    
    ##set plot position if missing
    if(missing(position)==TRUE){position<-c(min(temp@METADATA[,"POSITION"]):max(temp@METADATA[,"POSITION"]))}
    
    ##(1) order by RUN, SET OR BY POSITION
    if(sorter=="RUN"){
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"RUN"]),]
    }else if(sorter=="SET"){    
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"SET"]),]  
    }else {
      temp@METADATA<-temp@METADATA[order(temp@METADATA[,"POSITION"]),]      
    }

    ##(2) set SEL for selected position
    
        ##set all to FALSE
        temp@METADATA[,"SEL"]<-FALSE
    
        ##set TRUE 
        temp@METADATA[(temp@METADATA[,"POSITION"] %in% position)==TRUE & 
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
        
      
      ##plot graph 
      plot(seq(temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"],
               temp@METADATA[i,"HIGH"],by=temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"]),
           unlist(temp@DATA[temp@METADATA[i,"ID"]]),
           main=paste("pos=", temp@METADATA[i,"POSITION"],", run=", temp@METADATA[i,"RUN"],
                      ", set=", temp@METADATA[i,"SET"],sep=""
                      ),
           type="l",
           ylab=paste(temp@METADATA[i,"LTYPE"]," [cts/",round(temp@METADATA[i,"HIGH"]/temp@METADATA[i,"NPOINTS"],digits=3)," ",
                      measured_unit,"]",sep=""),
           xlab=if(measured_unit==" deg. C"){"temp. [deg. C]"}else{"time [s]"},
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
            if(temp@METADATA[i,"LTYPE"]=="TL"){paste("TL to ",temp@METADATA[i,"HIGH"], " deg. C",sep="")}
            else{paste(temp@METADATA[i,"LTYPE"],"@",temperature," deg. C",sep="")},          
            cex=0.9*cex.global)             
     }#endif::selection            
   }#endforloop
}#EOF
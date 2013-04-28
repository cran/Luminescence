##//////////////////////////////////////////////////////////////////////////////
##//fit_LMCurve.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen, Germany
##version: 0.2.9
##date: 2013-04-10
##==============================================================================
##+++++++++++++++++++++++Preface+++++++++++++++++++++++(START)
##  --LM fitting procedure for LM curves
##+++++++++++++++++++++++Preface+++++++++++++++++++++++(END)
##+++++++++++++++++Needed+Improvements++++++++++++++++(START)
## 1. Add: Energy of stimulation for component to sum contribution instead of time, if engery of 
##         of the diod is given!
## 2. Brute Force for all possibilites 
## 3. Penalise for additional components (including automatic Component Number recognition)
## 4. introduce x and y scale limiting
## 5. Add automatic component recognition with t-test
##+++++++++++++++++Needed+Improvements++++++++++++++++(END)


fit_LMCurve <- function(
	              
                  values, #values from bin file or what ever
                  values.bg, #background value
                  n.components=3, #number of components
                  start_values,
                  input.dataType="LM", #option for pLM curve
                  
                  sample_code="",
                  sample_ID="", #additional ID option
                  
                  LED.power=36, #in mW/cm^2
                  LED.wavelength=470, #in nm
                  
                  cex.global=0.8,                 
                  fit.trace=FALSE,
                  fit.advanced=FALSE,
                  fit.calcError=FALSE,
                  
                  bg.subtraction="polynomial",
                     
                  output.path, #outputname is produced automatically
                  
                  output.terminal=TRUE, #terminal output TRUE/FALSE
                  output.terminaladvanced=TRUE, #advanced terminal output TRUE/FALSE
                                                #only used if output.terminal is TRUE
                  
                  output.plot=TRUE,
                  output.plotBG=FALSE, #plot TRUE or FALSE,
                  ...
                  ) {
                                  			
  ## Set plot format parameters -----------------------------------------------
  extraArgs <- list(...) # read out additional arguments list
  
  log       <- if("log" %in% names(extraArgs)) {extraArgs$log} 
               else {""}
  
  xlim      <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} 
               else {c(min(values[,1]),max(values[,1]))}
  
  ylim      <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} 
               else {
                 
                 if(input.dataType=="pLM"){
                   c(0,max(values[,2]*1.1))
                 }else{
                   c(min(values[,2]),max(values[,2]*1.1))
                 }
                 
               }
  
  xlab      <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} 
               else {
                 
                 if(log=="x" | log=="xy"){
                     if(input.dataType=="LM"){"log time [s]"}else{"log u [s]"}
                 }
                 else{if(input.dataType=="LM"){"time [s]"}else{"u [s]"}
                 }
                     
               }
  
  ylab     <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} 
              else {
                
                if(log=="y" | log=="xy"){
                  if(input.dataType=="LM"){
                    paste("log LM-OSL [cts/",round(max(values[,1])/length(values[,1]),digits=2)," s]",sep="")
                  }else{"log pLM-OSL [a.u.]"}
                }
                else{ if(input.dataType=="LM"){
                      paste("LM-OSL [cts/",round(max(values[,1])/length(values[,1]),digits=2)," s]",sep="")
                      }
                      else{"pLM-OSL [a.u.]"}
                }

              }
  
  main      <- if("main" %in% names(extraArgs)) {extraArgs$main} 
               else {"Default"}
    
  
  
  
  fun       <- if("fun" %in% names(extraArgs)) {extraArgs$fun} else {FALSE}
 
  
##=================================================================================================##  	
##  BACKGROUND SUBTRACTION
##=================================================================================================##
 
#   ##perform background subtraction if background LM measurment exists
       
       if(missing(values.bg)==FALSE){
              
          #set graphical parameters
          par(mfrow=c(1,1), cex=1.5*cex.global)
         
          ##check if length of bg and signal is consistent
          if(length(values[,2])!=length(values.bg[,2])){stop("Error: Length of values and values.bg differs!")}
         
         if(bg.subtraction=="polynomial"){
         
           #fit polynom function to background
           glm.fit<-glm(values.bg[,2] ~ values.bg[,1]+I(values.bg[,1]^2)+I(values.bg[,1]^3))
           glm.coef<-coef(glm.fit)
           
           #subtract background with fitted function
           values[,2]<-values[,2]-
             (glm.coef[4]*values[,1]^3+glm.coef[3]*values[,1]^2+glm.coef[2]*values[,1]+glm.coef[1])
           writeLines("[fit_LMCurve.R] >> Background subtracted (method=\"polynomial\")!")
           
           ##plot Background measurement if needed
           if(output.plotBG==TRUE){
             
             plot(values.bg, ylab="LM-OSL [a.u.]", xlab="time [s]", main="Background")
             curve((glm.coef[4]*x^3+glm.coef[3]*x^2+glm.coef[2]*x+glm.coef[1]),add=TRUE,col="red",lwd=2)
             text(0,max(values.bg[,2]),paste("y = ", round(glm.coef[4],digits=2),
                                             "*x^3+",
                                             round(glm.coef[3],digits=2),
                                             "*x^2+",
                                             round(glm.coef[2],digits=2),
                                             "*x+",
                                             round(glm.coef[1],digits=2),
                                             sep=""),pos=4)
             mtext(side=3,sample_code,cex=.8*cex.global)
             }
               
         }else if(bg.subtraction=="linear"){
         
           #fit linear function to background
           glm.fit<-glm(values.bg[,2] ~ values.bg[,1])
           glm.coef<-coef(glm.fit)
           
           ##substract bg
           values[,2]<-values[,2]-(glm.coef[2]*values[,1]+glm.coef[1])
           writeLines("[fit_LMCurve.R] >> Background subtracted (method=\"linear\")!")
           
           ##plot Background measurement if needed
           if(output.plotBG==TRUE){
           
             plot(values.bg, ylab="LM-OSL [a.u.]", xlab="time [s]", main="Background")
             curve((glm.coef[2]*x+glm.coef[1]),add=TRUE,col="red",lwd=1.5)
             text(0,max(values.bg[,2]),paste("y = ",
                                             round(glm.coef[2],digits=2),
                                             "*x+",
                                             round(glm.coef[1],digits=2),
                                             sep=""),pos=4)
             mtext(side=3,sample_code,cex=.8*cex.global)
      
           }#endif::plot BG
           
         }else if(bg.subtraction=="channel"){
     
           values[,2]<-values[,2]-values.bg[,2]
           writeLines("[fit_LMCurve.R] >> Background subtracted (method=\"channel\")!")
        
           if(output.plotBG==TRUE){
          
           plot(values.bg, ylab="LM-OSL [a.u.]", xlab="time [s]", main="Background")
           mtext(side=3,sample_code,cex=.8*cex.global)
           }
           
         }else{stop("Error: Invalid method for background subtraction")}
       }
                        
	
##=================================================================================================##		
##  FITTING
##=================================================================================================##
    
    ##---------------------------------------------------------------------------------------------##   
     ##set function for fit equation
     ##////equation used for fitting////(start)
     fit.equation<-function(Im.i,xm.i){   
       equation<-parse(
         text=paste("exp(0.5)*Im[",Im.i,"]*(values[,1]/xm[",xm.i,"])*exp(-values[,1]^2/(2*xm[",xm.i,"]^2))",
                      collapse="+",sep=""))
         return(equation)
       }
     ##////equation used for fitting///(end)    
    ##---------------------------------------------------------------------------------------------##
       
    ##---------------------------------------------------------------------------------------------##    
    ##automatic start parameter estimation   
       
    ##set fit function
    fit.function<-fit.equation(Im.i=1:n.components,xm.i=1:n.components)   
  
    if(missing(start_values)==TRUE){
      
      ##set b (detrapping) values for a 7-component function taken from Jain et al. (2003)
      b.pseudo<-c(32,2.5,0.65,0.15,0.025,0.0025,0.00030)
      
      ##calculate xm parameters from values set based on the pseudo curves
      xm.pseudo<-sqrt(max(values[,1])/b.pseudo)
        
        ##the Im values obtaind by calculating residuals 
          xm.residual<-sapply(1:length(b.pseudo),function(x){abs(values[,1]-xm.pseudo[x])})
          xm.residual<-cbind(xm.residual,values[,1])
          Im.pseudo<-sapply(1:length(xm.pseudo),function(x){
                           min(xm.residual[which(xm.residual[,x]==min(xm.residual[,x])),8])#8 is time index
                           })
     
      ##set additional variables
      b.pseudo_start<-1
      b.pseudo_end<-0
      fit.trigger<-FALSE
           
     while(fit.trigger==FALSE){
         
             
             xm<-xm.pseudo[b.pseudo_start:(n.components+b.pseudo_end)]
             Im<-Im.pseudo[b.pseudo_start:(n.components+b.pseudo_end)]
            
             if(fit.advanced==TRUE){ 
             ##------------------------------------------------------------------------------------##
             ##MC for fitting parameter
             ##make the fitting more stable by small variations of the parameters 
           
             ##sample input parameters values from a normal distribution
             xm.MC<-sapply(1:length(xm),function(x){
               xm.MC<-sample(rnorm(25,mean=xm[x],sd=xm[x]/10), replace=TRUE)              
             })
            
             Im.MC<-sapply(1:length(xm),function(x){
                Im.MC<-sample(rnorm(25,mean=Im[x],sd=Im[x]/10), replace=TRUE)
                
             })
             ##------------------------------------------------------------------------------------##
     
             for(i in 1:length(xm.MC[,1])){       
                 
##NLS          ##try fit  
               fit<-try(nls(y~eval(fit.function), 
                      trace=fit.trace, 
                      data=data.frame(x=values[,1],y=values[,2]), 
                      algorithm="port",
                     start=list(Im=Im.MC[i,],xm=xm.MC[i,]),#end start values input
                     nls.control(
                       maxiter=500
                       ),#end nls control
                     lower=c(xm=min(values[,1]),Im=0),
                     upper=c(xm=max(values[,1]),Im=max(values[,2]*1.1))
                    ),# nls
                 silent=TRUE)# end try 
              ##graphical output
              if(i==1){cat(paste("[fit_LMCurve.R] >> advanced fitting attempt (#", 
                                 b.pseudo_start,"): ",sep=""))}
              cat("*")      
           
             if(inherits(fit,"try-error")==FALSE){break}
             }#end::forloop
             
             cat("\n")
             
            }else{
             
##NLS      ##try fit  
           fit<-try(nls(y~eval(fit.function), 
                           trace=fit.trace, 
                           data=data.frame(x=values[,1],y=values[,2]), 
                           algorithm="port",
                           start=list(Im=Im,xm=xm),#end start values input
                           nls.control(
                             maxiter=500
                             ),#end nls control
                           lower=c(xm=0,Im=0)
                           ),# nls
                       silent=TRUE)# end try
              
            }#endifelse::fit.advanced
              
         
        if(inherits(fit,"try-error")==FALSE){fit.trigger<-TRUE}
        else{
            
           if((n.components+b.pseudo_end)==7){fit.trigger<-TRUE             
           }else{
             b.pseudo_start<-b.pseudo_start+1
             b.pseudo_end<-b.pseudo_end+1
           }#endif::maximum loops  
        }#endif::try-error  
        }#end:whileloop fit trigger
  
    }else{#endif::missing start values
    ##---------------------------------------------------------------------------------------------##
     
      fit<-try(nls(y~eval(fit.function), trace=fit.trace, data.frame(x=values[,1],y=values[,2]), 
                   algorithm="port", start=list(Im=start_values[,1],xm=start_values[,2]),#end start values input
                   nls.control(maxiter=500),
                   lower=c(xm=0,Im=0),
                   #upper=c(xm=max(x),Im=max(y)*1.1)# set lower boundaries for components
                   )# nls
               )# end try
    }#endif::startparameter   
    
    ##---------------------------------------------------------------------------------------------##  
   
    ##grep parameters
    if(inherits(fit,"try-error")==FALSE){    
    parameters<-coef(fit)
 
    ##write parameters in vectors and order parameters         
    Im<-parameters[1:(length(parameters)/2)]
    xm<-parameters[(1+(length(parameters)/2)):length(parameters)]    
      
      ##order parameters
      o<-order(xm)
      xm<-xm[o]
      Im<-Im[o]
     
 
    if (output.terminal==TRUE){    
    ##print rough fitting information - use the nls() control for more information
    writeLines("\n[fit_LMCurve.R]")
    writeLines(paste("\nFitting was done using a ",n.components, "-component function:\n",sep=""))
        
    ##print parameters    
    print(parameters)
   
    #print some additional information    
    writeLines("\n(equation used for fitting according Kitis & Pagonis, 2008)")
    }#end if
  
##=================================================================================================##   
##  Additional Calculations
##=================================================================================================##
  
    ##calculate stimulation intensity Schmidt (2008)
         
      ##Energy - E = h*v
      h<-6.62606957e-34 #in W*s^2 - Planck constant
      ny<-299792458/(LED.wavelength/10^9) #frequency of light
      E<-h*ny
    
      ##transform LED.power in W/cm^2
      LED.power<-LED.power/1000
         
      stimulation_intensity<-LED.power/E
    
    
    ##calculate b and n from the equation of Bulur(1996) to compare results
    ##Using Equation 5 and 6 from Kitis (2008)
    b<-as.vector(max(values[,1])/xm^2) #detrapping probability
    n0<-as.vector((Im/exp(-0.5))*xm)
    
    
    ##CALCULATE 1- sigma CONFIDENCE INTERVALL
    ##---------------------------------------------------------------------------------------------##
    b.error<-rep(NA, n.components)
    n0.error<-rep(NA, n.components)
    
    if(fit.calcError==TRUE){
    ##option for confidence interval
    values.confint<-confint(fit, level=0.68) 
    Im.confint<-values.confint[1:(length(values.confint[,1])/2),]
    xm.confint<-values.confint[((length(values.confint[,1])/2)+1):length(values.confint[,1]),]
      
      ##error calculation
      b.error<-as.vector(abs((max(values[,1])/xm.confint[,1]^2)-(max(values[,1])/xm.confint[,2]^2)))
      n0.error<-as.vector(abs(((Im.confint[,1]/exp(-0.5))*xm.confint[,1]) - ((Im.confint[,2]/exp(-0.5))*xm.confint[,2])))
    }
    ##---------------------------------------------------------------------------------------------##
     
    
    ##calculate photoionisation cross section and print on terminal
    ##using EQ (5) in Kitis   
    cs<-as.vector((max(values[,1])/xm^2)/stimulation_intensity)
    rel_cs<-round(cs/cs[1],digits=4)
   
    ##coefficient of determination after law
    RSS <- sum(residuals(fit)^2) #residual sum of squares
    TSS <- sum((values[,2] - mean(values[,2]))^2) #total sum of squares
    pR<-round(1-RSS/TSS,digits=4)
   
##=================================================================================================##   
##  Terminal Output (advanced)
##=================================================================================================##   
if (output.terminaladvanced==TRUE && output.terminal==TRUE){    
    ##write fill lines
    writeLines("------------------------------------------------------------------------------")
  	writeLines("(1) Corresponding values according the equation in Bulur, 1996 for b and n0:\n")
    for (i in 1:length(b)){
      writeLines(paste("b",i," = ",format(b[i],scientific=TRUE)," +/- ",format(b.error[i],scientific=TRUE),sep=""))
      writeLines(paste("n0",i," = ",format(n0[i],scientific=TRUE)," +/- ",format(n0.error[i],scientific=TRUE),"\n",sep=""))    
    }#end for loop
     
    ##write photoionisation cross section on terminal
    for (i in 1:length(cs)){    
    writeLines(paste("cs from component",i," = ",format(cs[i],scientific=TRUE, digits=4), " cm^2",
                "\t >> relative: ",round(cs[i]/cs[1],digits=4),sep=""))
    
     }#end for loop 
    writeLines(paste(
    "\n(stimulation intensity value used for calculation: ",format(stimulation_intensity,scientific=TRUE)," 1/s 1/cm^2)",sep=""))
    writeLines("(errors quoted as 1-sigma uncertainties)")
    writeLines("------------------------------------------------------------------------------\n")
	
    #sum of squares
 		writeLines(paste("pseudo-R^2 = ",pR,sep=""))
}#end if
   
##=================================================================================================##   
##  COMPOSE RETURN VALUES (data.frame)
##=================================================================================================##                  

   ##write output table if values exists
     if (exists("fit")){
              
       ##set data.frame for a max value of 7 components
       output.table<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA,
                                NA,NA,NA,NA,NA,NA,NA,NA)
    
        output.tableColNames<-c("Im1","xm1",
                                "b1","b1.error","n01","n01.error",
                                "cs1","rel_cs1",
                               "Im2","xm2",
                                "b2","b2.error","n02","n02.error",
                                "cs2","rel_cs2",
                               "Im3","xm3",
                                "b3","b3.error","n03","n03.error",
                                "cs3","rel_cs3",
                               "Im4","xm4",
                                "b4","b4.error","n04","n04.error",
                                "cs4","rel_cs4",
                               "Im5","xm5",
                                "b5","b5.error","n05","n05.error",
                                "cs5","rel_cs5",
                               "Im6","xm6",
                                "b6","b6.error","n06","n06.error",
                                "cs6","rel_cs6",
                               "Im7","xm7",
                                "b7","b7.error","n07","n07.error",
                                "cs7","rel_cs7")
       
       
       ##write components in output table 
       i<-0
       k<-1
       while(i<=n.components*8){
         output.table[1,i+1]<-Im[k]
         output.table[1,i+2]<-xm[k]
         output.table[1,i+3]<-b[k]
         output.table[1,i+4]<-b.error[k]
         output.table[1,i+5]<-n0[k]
         output.table[1,i+6]<-n0.error[k]
         output.table[1,i+7]<-cs[k]
         output.table[1,i+8]<-rel_cs[k]         
         i<-i+8 
         k<-k+1
       }
     
       ##add pR and n.components
       output.table<-cbind(sample_ID,sample_code,n.components,output.table,pR)
     
       ###alter column names
       colnames(output.table)<-c("ID","sample_code","n.components",output.tableColNames,"pseudo-R^2")
  
##=================================================================================================##   
## TABLE OUTPUT (CVS)
##=================================================================================================##       
       
      if(missing(output.path)==FALSE){
       
      ##write file with just the header if the file not exists
       if(file.exists(paste(output.path,"Fit_Output_",sample_code,".csv",sep=""))==FALSE){
         write.table(output.table,file=paste(output.path,"Fit_Output_",sample_code,".csv",sep=""), sep=","
                     ,row.names=FALSE)          
       }else{
         write.table(output.table,file=paste(output.path,"Fit_Output_",sample_code,".csv",sep=""), sep=","
                     ,row.names=FALSE, append=TRUE, col.names=FALSE)
      }#endif :: for write option                 
     }#endif::CSV output
       
##-------------------------------------------------------------------------------------------------##       
}#endif::exists fit
}else{
  
  output.table<-NA
  writeLines("[fit_LMCurve.R] >> Fitting Error: Plot without fit produced!")
  
  }
##=================================================================================================##    
##  PLOTTING
##=================================================================================================##
if(output.plot==TRUE){    
 
    ##cheat the R check routine
    x<-NULL; rm(x)
  
    ##set colors gallery to provide more colors
    col<-unlist(colors())
    col<-col[c(261,552,51,62,76,151,451,474,654)]
   
    ##set plot frame
    layout(matrix(c(1,2,3),3,1,byrow=TRUE),c(1.6,1,1), c(1,0.3,0.4),TRUE)
    par(oma=c(1,1,1,1),mar=c(0,4,3,0),cex=cex.global)
 		
     ##==uppper plot==##
     ##open plot area
     plot(NA,NA,
          xlim=xlim,
          ylim=ylim,
          xlab="",
          xaxt="n",
          main=main,
          log=log,          
          ylab=ylab
         )#endplot
    
     mtext(side=3,sample_code,cex=0.8*cex.global)
    
     ##plotting measured signal 
     points(values[,1],values[,2],pch=20, col="grey")
    
    ##==pseudo curve==##---------------------------------------------------------------------------##
    ##curve for used pseudo values
    if(inherits(fit,"try-error")==TRUE & missing(start_values)==TRUE){ 
      fit.function<-fit.equation(Im.i=1:n.components,xm.i=1:n.components)
      Im<-Im.pseudo[1:n.components]
      xm<-xm.pseudo[1:n.components]

      ##draw pseudo curve
      lines(values[,1],eval(fit.function), lwd=2, col="red", lty=2)
    
      axis(side=1)
      mtext(side=1,xlab, cex=.9*cex.global,line=2)
     
      mtext(side=4,paste(n.components, " component pseduo function is shown",sep=""),cex=0.7, col="blue")
      
      ##draw information text on plot
      text(min(values[,1]),max(values[,2]),"FITTING ERROR!",pos=4)
      
      ##additional legend
      legend("topright",c("pseudo sum function"),lty=2,lwd=2,col="red",bty="n")
      
      par()
     }
    ##==pseudo curve==##---------------------------------------------------------------------------##
           
   	##plot sum function
    if(inherits(fit,"try-error")==FALSE){
     lines(values[,1],eval(fit.function), lwd=2, col="black")
     legend.caption<-"sum curve"
     curve.col<-1
                    
    ##plot signal curves                
 			                  
 			##plot curve for additional parameters
      for (i in 1:length(xm)) {
          curve(exp(0.5)*Im[i]*x/xm[i]*exp(-x^2/(2*xm[i]^2)),col=col[i+1], lwd=2,add=TRUE)
          legend.caption<-c(legend.caption,paste("component ",i,sep=""))
          curve.col<-c(curve.col,i+1)
      }              
 		##plot legend
 		legend(if(log=="x"| log=="xy"){
       if(input.dataType=="pLM"){"topright"}else{"topleft"}}else{"topright"},
          legend.caption,lty=1,lwd=2,col=col[curve.col], bty="n")
 		
   ##==lower plot==##    
 		##plot residuals	
    par(mar=c(4.2,4,0,0))
    plot(values[,1],residuals(fit), 
         xlim=xlim, 
         xlab=xlab, 
         type="l", 
         col="grey", 
         ylab="residual",
         lwd=2,
         log=log)
 		
     ##ad 0 line
     abline(h=0)
 		
    ##plot component contribution to the whole signal
    #open plot area
    par(mar=c(4,4,3.2,0))
    plot(NA,NA,
        xlim=xlim,
        ylim=c(0,100),
        ylab="contribution [%]",
        xlab=xlab,
        main="Component Contribution To Sum Curve",
        log=if(log=="xy"){"x"}else{log}
         )
    
    ##----------------------------------------------------------------------------------------------##
    ##++component contribution plot++##
    ##----------------------------------------------------------------------------------------------##
    ##1st polygon (calculation)
    y.contribution_first<-(exp(0.5)*Im[1]*values[,1]/xm[1]*exp(-values[,1]^2/(2*xm[1]^2))/(eval(fit.function))*100) 
    
    ##avoid NaN values (might happen with synthetic curves)
    y.contribution_first[is.nan(y.contribution_first)==TRUE]<-0      
    
    ##1st polygon (plot)
    polygon(c(values[,1],rev(values[,1])),c(rep(100,length(values[,1])),100-rev(y.contribution_first)),col=col[2])    
    
    ##polygons in between (calculate and plot)
    if (length(xm)>2){
      
      y.contribution_prev<-y.contribution_first
      i<-2
      
      while (i<=length(xm)-1) {
        y.contribution_next<-(exp(0.5)*Im[i]*values[,1]/xm[i]*exp(-values[,1]^2/(2*xm[i]^2))/(eval(fit.function))*100)
        
        ##avoid NaN values
        y.contribution_next[is.nan(y.contribution_next)==TRUE]<-0
        
        polygon(c(values[,1],rev(values[,1])),c(100-y.contribution_prev,
                              rev(100-y.contribution_prev-y.contribution_next)),col=col[i+1])
        y.contribution_prev<-y.contribution_prev+y.contribution_next
        i<-i+1        
      }#end while loop
    }#end if
    
    ##last polygon (calculation)  
    y.contribution_last<-(exp(0.5)*Im[length(xm)]*values[,1]/xm[length(xm)]*exp(-values[,1]^2/(2*xm[length(xm)]^2))/
      (eval(fit.function))*100)
    
    ##avoid NaN values
    y.contribution_last[is.nan(y.contribution_last)==TRUE]<-0
    
    ##last polygon (plot)
    polygon(c(values[,1],rev(values[,1])),c(y.contribution_last,rep(0,length(values[,1]))),col=col[length(xm)+1])
      
    
    ##--------------------------------------------------------------------------------------------##
    }#end if try-error for fit

    if(fun==TRUE){sTeve()}
}    
##--------------------------------------------------------------------------------------------------
    ##remove objects  
    try(unlist("parameters"))
    return(list(fit=fit,output.table=output.table))   
}#Endoffunction

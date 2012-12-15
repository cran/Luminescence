##//////////////////////////////////////////////////////
##//fit_CWCurve.R - for CW - curve fitting 
##//////////////////////////////////////////////////////

##======================================
#author: Sebastian Kreutzer
#organisation: JLU of Giessen
#vers.: 0.1
#date: 28/10/2012
##======================================

fit_CWCurve<- function(
                  values, #values from bin file or what ever
                           
                  n.components.max, #maximum number of components,
                  fit.failure_threshold=3, #threshold for fit failure
                  fit.trace=FALSE,
                  fit.calcError=FALSE,
                  
                  LED.power=36, #in mW/cm^2
                  LED.wavelength=470, #in nm

                  log="", #set log scale
                  cex.global=0.6,
                  main="CW-OSL Curve Fit",
                  sample_code="Default", #set sample code 
                  ylab, #provide more options for a brighter application
                  xlab, #provide more options for a brighter application
                  
                  output.path, #outputname is produced automatically
                  
                  output.terminal=TRUE, #terminal output TRUE/FALSE
                  output.terminalAdvanced=TRUE, #advanced terminal output TRUE/FALSE
                                                #only used if output.terminal is TRUE
                                               
                  output.plot=TRUE #plot TRUE or FALSE
                  ) {
                                  		
    
      ##set sys language to EN (to get the error messages as well in EN) 
      Sys.setenv(LANG="EN")  
       
      ##switch off warnings of to avoid confusions
      options(warn=-1)
 	
      ##set x and y values
      x<-values[,1]
      y<-values[,2]
        
##=================================================================================================##		
## FITTING
##=================================================================================================##
##             
##////equation used for fitting////(start)
  fit.equation<-function(I0.i,lambda.i){   
                equation<-parse(
                text=paste("I0[",I0.i,"]*exp(-lambda[",lambda.i,"]*x)",
                collapse="+",sep=""))
                return(equation)
  }
##////equation used for fitting///(end)    
       
  ##set variables     
  fit.trigger<-TRUE #triggers if the fitting should stopped
  n.components<-1 #number of components used for fitting - start with 1
  fit.failure_counter<-0 #counts the failed fitting attempts
 
  ##if n.components_max is missing, then it is Inf
  if(missing(n.components.max)==TRUE){n.components.max<-Inf}     

##       
##
##++++Fitting loop++++(start)       
while(fit.trigger==TRUE & n.components<=n.components.max){
        
    ##rough automatic start parameter estimation
    I0<-rep(values[1,2]/3,n.components)
    
    ##fit an linear function a first guess
    temp.values<-data.frame(log(y),x)
    
    temp<-lm(temp.values)
    lambda<-abs(temp$coefficient[2])
    
    k<-2;  
    while(k<=n.components){
      lambda[k]<-lambda[k-1]/10 
      k<-k+1
    }
  
    ##set fit equation as fit function
    I0.i<-1:n.components
    lambda.i<-1:n.components
    fit.function<-fit.equation(I0.i=I0.i,lambda.i=lambda.i)
         
  
    ##try fit    
    fit.try<-try(nls(y~eval(fit.function), 
                     trace=fit.trace, 
                     data=values, 
                     algorithm="port",
                start=list(
  						            I0=I0,
                          lambda=lambda
							            ),
						    nls.control(
						             maxiter=500,
                         warnOnly=FALSE,
                         minFactor=1/2048,
                         ),
						    lower=c(I0=0,lambda=0)# set lower boundaries for components
            )# nls
		)#end try
   
    ##count failed attempts for fitting 
    if(inherits(fit.try,"try-error")==FALSE){
      fit<-fit.try
      n.components<-n.components+1       
    }else{fit.failure_counter<-fit.failure_counter+1
         if(n.components==fit.failure_counter & exists("fit")==FALSE){fit<-fit.try}}
    
    ##stop fitting after a given number of wrong attempts
    if(fit.failure_counter>=fit.failure_threshold){fit.trigger<-FALSE}
   
}##end while 
##++++Fitting loop++++(end)    
      
##=================================================================================================##   
## FITTING OUTPUT
##=================================================================================================##       
    
    ##grep parameters
    if(inherits(fit,"try-error")==FALSE){
     
      parameters<-coef(fit)
    
    ##correct fit equation for the de facto used number of components
      I0.i<-1:(length(parameters)/2)
      lambda.i<-1:(length(parameters)/2)
      fit.function<-fit.equation(I0.i=I0.i,lambda.i=lambda.i)
      n.components<-length(I0.i)
         
      ##write parameters in vectors and order by decreasing lambda value       
      I0<-parameters[1:(length(parameters)/2)]
      lambda<-parameters[(1+(length(parameters)/2)):length(parameters)] 
        
          o<-order(lambda,decreasing=TRUE)
          I0<-I0[o]
          lambda<-lambda[o]
                      
##=================================================================================================##   
## Additional Calculation
##=================================================================================================##
    
      
    ## ---------------------------------------------  
    ##calculate stimulation intensity Schmidt (2008)
      
      ##Energy - E = h*v
      h<-6.62606957e-34 #in W*s^2 - Planck constant
      ny<-299792458/(LED.wavelength/10^9) #frequency of light
      E<-h*ny
      
      ##transform LED.power in W/cm^2
      LED.power<-LED.power/1000
      
      ##gets stimulation intensity
      stimulation_intensity<-LED.power/E  
     
    ## ---------------------------------------------   
    ##calculate photoionisation cross section and print on terminal
      
      ##using EQ (5) in Kitis   
      cs<-as.vector(lambda/stimulation_intensity)
      cs.rel<-round(cs/cs[1],digits=4)
    
    ## ---------------------------------------------   
    ##coefficient of determination after law
      
      RSS <- sum(residuals(fit)^2) #residual sum of squares
      TSS <- sum((y - mean(y))^2) #total sum of squares
 		  pR<-round(1-RSS/TSS,digits=4)
      
    ## ---------------------------------------------        
    ##calculate 1- sigma CONFIDENCE INTERVALL
    
      lambda.error<-rep(NA, n.components)
      I0.error<-rep(NA, n.components)
      
     if(fit.calcError==TRUE){
        ##option for confidence interval
        values.confint<-confint(fit, level=0.68) 
        I0.confint<-values.confint[1:(length(values.confint[,1])/2),]
        lambda.confint<-values.confint[((length(values.confint[,1])/2)+1):length(values.confint[,1]),]
        
      ##error calculation
      I0.error<-as.vector(abs(I0.confint[,1]-I0.confint[,2]))
      lambda.error<-as.vector(abs(lambda.confint[,1]-lambda.confint[,2]))  
        
      }#endif::fit.calcError
      
##=================================================================================================##   
## Terminal Output 
##=================================================================================================##   
      
      if (output.terminal==TRUE){  
        
        ##print rough fitting information - use the nls() control for more information
        writeLines("\n[fit_CWCurve.R]")
        writeLines(paste("\nFitting was finally done using a ",n.components, 
                         "-component function (max=",n.components.max,"):",sep=""))
        writeLines("------------------------------------------------------------------------------")
        writeLines(paste("y = ",as.character(fit.function),"\n",sep=""))
        
        ##combine values and change rows names
        fit.results<-cbind(I0,I0.error,lambda,lambda.error,cs, cs.rel)
        row.names(fit.results)<-paste("c", 1:(length(parameters)/2), sep="")
        
        ##print parameters    
        print(fit.results)
        
        #print some additional information    
        if(fit.calcError==TRUE){writeLines("(errors quoted as 1-sigma values)")}
        writeLines("------------------------------------------------------------------------------")
      }#end if    
   
##=================================================================================================##   
## Terminal Output (advanced)
##=================================================================================================##   
if (output.terminalAdvanced==TRUE && output.terminal==TRUE){    

    ##sum of squares
 		writeLines(paste("pseudo-R^2 = ",pR,sep=""))
}#end if
##=================================================================================================##   
## Table Output
##=================================================================================================##                  

    ##write output table if values exists
    if (exists("fit")){
    
    ##set data.frame for a max value of 7 components
    output.table<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                             NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                             NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    output.tableColNames<-c("I01","I01.error","lambda1", "lambda1.error",
                            "cs1","cs1.rel",
                            "I02","I02.error","lambda2", "lambda2.error",
                            "cs2","cs2.rel",
                            "I03","I03.error","lambda3", "lambda3.error",
                            "cs3","cs3.rel",
                            "I04","I04.error","lambda4", "lambda4.error",
                            "cs4","cs4.rel",
                            "I05","I05.error","lambda5", "lambda5.error",
                            "cs5","cs5.rel",
                            "I06","I06.error","lambda6", "lambda6.error",
                            "cs6","cs6.rel",
                            "I07","I07.error","lambda7", "lambda7.error",
                            "cs7","cs7.rel"
                            )
        
      ##write components in output table 
      i<-0
      k<-1
      while(i<=n.components*6){
        output.table[1,i+1]<-I0[k]
        output.table[1,i+2]<-I0.error[k]
        output.table[1,i+3]<-lambda[k]
        output.table[1,i+4]<-lambda.error[k]
        output.table[1,i+5]<-cs[k]
        output.table[1,i+6]<-cs.rel[k]
        i<-i+6
        k<-k+1
      }
 
      ##add pR and n.components
      output.table<-cbind(sample_code,n.components,output.table,pR)
     
      ##alter column names
      colnames(output.table)<-c("sample_code","n.components",
                                output.tableColNames,"pseudo-R^2")  
     
    if(missing(output.path)==FALSE){
      
      ##write file with just the header if the file not exists
      if(file.exists(paste(output.path,"fit_CWCurve_Output_",sample_code,".csv",sep=""))==FALSE){
        write.table(output.table,file=paste(output.path,"fit_CWCurve_Output_",
                                            sample_code,".csv",sep=""), sep=";"
                    ,row.names=FALSE)          
      }else{
        write.table(output.table,file=paste(output.path,"fit_CWCurve_Output_",
                                            sample_code,".csv",sep=""), sep=";"
                    ,row.names=FALSE, append=TRUE, col.names=FALSE)
        
      }#endif::for write option
      
     }#endif::table output
    
}#endif :: (exists("fit"))   
  
}else{writeLines("[simpleITLCurveFit.R] >> Fitting Error >> Plot without fit produced!")
      output.table<-NA
      }
   
##=================================================================================================##  
## PLOTTING
##=================================================================================================##
if(output.plot==TRUE){      
   
    ##set colors gallery to provide more colors
    col<-unlist(colors())
    col<-col[c(261,552,51,62,76,151,451,474,654)]
   
    ##set plot frame
    layout(matrix(c(1,2,3),3,1,byrow=TRUE),c(1.6,1,1), c(1,0.3,0.4),TRUE)
    par(oma=c(1,1,1,1),mar=c(0,4,3,0),cex=cex.global)
 		
     ##==uppper plot==##
     ##open plot area
    
     plot(NA,NA,
          xlim=c(min(x),max(x)),
          ylim=if(log=="xy"){c(1,max(y))}else{c(0,max(y))},
          xlab="",
          xaxt="n",
          ylab=if(missing(ylab)==TRUE){
            paste("OSL [cts/",length(x)/max(x)," s]",sep="")}else{
            ylab  
            },
          main=main, 
          log=log)
   
     ##plotting measured signal 
     points(x,y,pch=20, col="grey")

     ##add additional labeling (fitted function)
     mtext(side=3,sample_code,cex=0.7*cex.global)
    
   	##plot sum function
    if(inherits(fit,"try-error")==FALSE){
    lines(x,eval(fit.function), lwd=2, col="black")
    legend.caption<-"sum curve"
    curve.col<-1
                    
    ##plot signal curves                
 			            
 		##plot curve for additional parameters
     if(length(I0)>1){
      
       for (i in 1:length(I0)) {
            curve(I0[i]*exp(-lambda[i]*x),col=col[i+1], lwd=2,add=TRUE)
            legend.caption<-c(legend.caption,paste("component ",i,sep=""))
            curve.col<-c(curve.col,i+1)
        }
     }#end if
 		##plot legend
    #legend(y=max(y)*1,"measured values",pch=20, col="gray", bty="n")
 		legend("topright",legend.caption,lty=rep(1,n.components+1,NA),lwd=2,col=col[curve.col], bty="n")
  
    ##==lower plot==##    
 		##plot residuals	
    par(mar=c(4.2,4,0,0))
    plot(x,residuals(fit), 
         xlim=c(min(x),max(x)), 
         xlab=if(missing(xlab)==TRUE){if(log=="x" | log== "xy"){"log time [s]"}else{"time [s]"}}else{xlab}, 
         type="l", 
         col="grey", 
         ylab="residual [a.u.]",
         lwd=2,
         log=if(log=="x" | log=="xy"){log="x"}else{""}
         )
  
     ##add 0 line
     abline(h=0)
 		
    ##plot component contribution to the whole signal
    #open plot area
     par(mar=c(4,4,3.2,0))
     plot(NA,NA,
         xlim=c(min(x),max(x)),
         ylim=c(0,100),
         ylab="contribution [%]",
         xlab=if(missing(xlab)==TRUE){if(log=="x" | log=="xy"){"log time [s]"}else{"time [s]"}}else{xlab},
         main="Component Contribution To Sum Curve",
         log=if(log=="x" | log=="xy"){log="x"}else{""}
        )
    
     ##component contribution plot
       ##calculate first polygon    
       y.contribution_first<-(I0[1]*exp(-lambda[1]*x))/(eval(fit.function))*100
       polygon(c(x,rev(x)),c(rep(100,length(x)),100-rev(y.contribution_first)),col=col[2])    
 
    
       ##calculate polygons in between 
       if (length(I0)>2){
         y.contribution_prev<-y.contribution_first
         i<-2
         while (i<=length(I0)-1) {
           y.contribution_next<-I0[i]*exp(-lambda[i]*x)/(eval(fit.function))*100
           polygon(c(x,rev(x)),c(100-y.contribution_prev,rev(100-y.contribution_prev-y.contribution_next)),
                   col=col[i+1])
           y.contribution_prev<-y.contribution_prev+y.contribution_next
           i<-i+1          
         }#end while loop
       }#end if
         
     ##calculate last polygon    
     y.contribution_last<-I0[length(I0)]*exp(-lambda[length(lambda)]*x)/(eval(fit.function))*100
      polygon(c(x,rev(x)),c(y.contribution_last,rep(0,length(x))),col=col[length(I0)+1])
    
    }#end if try-error for fit
} 
          
##=================================================================================================##  
## Return Values
##=================================================================================================##   
      
     return(list(fit=fit,output.table=output.table))   
      
}#Endoffunction

##===========================================================================================
##STARTING FUNCTION##


#EOF


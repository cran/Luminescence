##//////////////////////////////////////////////
## plot_GrowthCurve.R
##//////////////////////////////////////////////
##======================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen, Germany
##version: 0.9.6
##date: 23/07/2012
##======================================

plot_GrowthCurve<-function(
									sample, 
									main="Growth Curve", 
                  mtext="",
									fit.method="EXP", 
                  fit.weights=TRUE, 
									fit.includingRepeatedRegPoints=TRUE, 
									fit.NumberRegPoints, 
									fit.NumberRegPointsReal, 
									NumberIterations.MC=100, 
									xlab="s", 
									output.plot=TRUE, 
                  output.plotExtended=TRUE, 
									cex.global=1 
								)
						{
##=================================================================================================##
##  
##  
##0. Error capturing
  
  ##NULL values in the data.frame are not allowed for the y-column
    if(length(sample[sample[,2]==0,2])>0){
     
      cat("\n[plot_GrowthCurve.R] >> Warning:",
          length(sample[sample[,2]==0,2]),"values with 0 for Lx/Tx detected; replaced by 0.0001.\n")
      sample[sample[,2]==0,2]<-0.0001 
    }
  
##1. INPUT
  
  #1.0.1 calculate number of reg points if not set
  if(missing(fit.NumberRegPoints)==TRUE){fit.NumberRegPoints<-length(sample[-1,1])}
  if(missing(fit.NumberRegPointsReal)==TRUE){
    fit.NumberRegPointsReal<-length(sample[-which(duplicated(sample[,1]) | sample[,1]==0),1])
  }
 
  #1.1 Produce dataframe from input values
  xy<-data.frame(x=sample[2:(fit.NumberRegPoints+1),1],y=sample[2:(fit.NumberRegPoints+1),2])
	y.Error<-sample[2:(fit.NumberRegPoints+1),3]
  
  ##1.1.1 produce weights for weighted fitting
  if(fit.weights==TRUE){
    fit.weights<-1/y.Error/(sum(1/y.Error))
  }else{
    fit.weights<-NULL
  }
  

	#1.2 Prepare datasets for Monte Carlo Simulation
	
		data.MC<-t(matrix(sapply(seq(2,fit.NumberRegPoints+1,by=1), 
									function(x){sample(rnorm(10000,mean=sample[x,2], sd=sample[x,3]), 
                            NumberIterations.MC, replace=TRUE)}), nrow=NumberIterations.MC
						     )#end matrix
					)#end transpose matrix
									
	#1.3 set x.natural
		x.natural<-as.vector(seq(1:NumberIterations.MC))
 
##=================================================================================================##
# FITTING ------------------------------------------------------------------------------------
##=================================================================================================##
##3. Fitting values with nonlinear least-squares estimation of the parameters
 
  ##set functions for fitting
 
  #EXP
	fit.functionEXP<-function(a,b,c,x) {a*(1-exp(-(x+c)/b))}

	#EXP+LIN
	fit.functionEXPLIN<-function(a,b,c,g,x) {a*(1-exp(-(x+c)/b)+(g*x))}
 
  #EXP+EXP
  fit.functionEXPEXP<-function(a1,a2,b1,b2,x){(a1*(1-exp(-(x)/b1)))+(a2*(1-exp(-(x)/b2)))}
  
	##input data for fitting; exclude repeated RegPoints 
  if(fit.includingRepeatedRegPoints==FALSE){
    data<-data.frame(x=xy[-which(duplicated(xy[,1])),1],y=xy[-which(duplicated(xy[,1])),2])
   }else{data<-data.frame(xy)}
  

  ##START PARAMETER ESTIMATION
  ##-----------------------------------------------------------------------------------------------##
  ##general setting of start parameters for fitting
  
	   ##a - estimation for a a the maxium of the y-values (Lx/Tx)
     a<-max(data[,2])

     ##b - get start parameters from a linear fit of the log(y) data
     fit.lm<-lm(log(data$y)~data$x)
     b<-as.numeric(1/fit.lm$coefficients[2])
     
	   ##c - get start parameters from a linear fit - offset on x-axis
	   fit.lm<-lm(data$y~data$x)
     c<-as.numeric(abs(fit.lm$coefficients[1]/fit.lm$coefficients[2]))
       
     #take slope from x - y scaling
		 g<-max(data[,2]/max(data[,1]))
  
     #set D01 and D02 (in case of EXp+EXP)
     D01<-NA; D02<-NA
  
  ##-----------------------------------------------------------------------------------------------##
  ##to be a little bit more flexible the start parameters varries within a normal distribution
  
      ##draw 50 start values from a normal distribution a start values
      a.MC<-rnorm(50,mean=a,sd=a/100)
      b.MC<-rnorm(50,mean=b,sd=b/100)
      c.MC<-rnorm(50,mean=c,sd=c/100)
      g.MC<-rnorm(50,mean=g,sd=g/1)
    
      ##set start vector (to avoid errors witin the loop)
      a.start<-NA; b.start<-NA; c.start<-NA; g.start<-NA
  

  
  ##-----------------------------------------------------------------------------------------------##
    
	#================================================================================================##
	#EXP#

	if (fit.method=="EXP" | fit.method=="EXP OR LIN" | fit.method=="LIN"){
      
        if(fit.method!="LIN"){
  
					##FITTING on GIVEN VALUES##
					#	--use classic R fitting routine to fit the curve
          
          ##try to create some start parameters from the input values to make the fitting more stable
          for(i in 1:50){
            
            a<-a.MC[i];b<-b.MC[i];c<-c.MC[i]
            
            fit<-try(nls(y~fit.functionEXP(a,b,c,x),
                         data=data,
                         start=c(a=a,b=b,c=c),
                         trace=FALSE,
                         algorithm="port",
                         lower=c(a=0,b>0,c=0),
                         nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
                         ),silent=TRUE)
            
            if(class(fit)!="try-error"){
              #get parameters out of it
              parameters<-(coef(fit)) 
              b.start[i]<-as.vector((parameters["b"]))
              a.start[i]<-as.vector((parameters["a"])) 
              c.start[i]<-as.vector((parameters["c"]))
            }
          }
          
          ##used mean as start parameters for the final fitting
          a<-median(na.exclude(a.start));b<-median(na.exclude(b.start));c<-median(na.exclude(c.start))
       							
          #FINAL Fit curve on given values
					fit<-try(nls(y~fit.functionEXP(a,b,c,x),
									data=data,
									start=c(a=a,b=b,c=c),
                  weights=fit.weights,
									trace=FALSE,
									algorithm="port",
                  lower=c(a=0,b=0,c=0),
									nls.control(maxiter=500)
          				))#end nls
              
							if (class(fit)=="try-error"){
                
							  writeLines("[plot_GrowthCurve.R] >> try-error for EXP fit")
                
							}else{
							  #get parameters out of it
							  parameters<-(coef(fit)) 
							  b<-as.vector((parameters["b"]))
							  a<-as.vector((parameters["a"])) 
							  c<-as.vector((parameters["c"]))
							
              #print D01 value
              writeLines(paste("[plot_GrowthCurve.R] >> D01 = ",round(b,digits=2),sep=""))
              D01<-round(b,digits=2)  
            
							#calculate De 
							De<-round(-c-b*log(1-sample[1,2]/a), digits=2)
							
						##Monte Carlo Simulation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error

						#set variables
						var.b<-vector(mode="numeric", length=NumberIterations.MC)
						var.a<-vector(mode="numeric", length=NumberIterations.MC)
						var.c<-vector(mode="numeric", length=NumberIterations.MC)
						
						#start loop
						for (i in 1:NumberIterations.MC) { 
							
							data<-data.frame(x=xy$x,y=data.MC[,i])

							fit.MC<-try(nls(y~fit.functionEXP(a,b,c,x),
								data=data,
								start=c(a=a,b=b,c=c),
                weights=fit.weights,
								trace=FALSE,
								algorithm="port",
								nls.control(maxiter=500) #increase max. iterations
							),silent=TRUE) #end nls

							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA

							}else {
								
								#get parameters out 
								parameters<-coef(fit.MC) 
								var.b[i]<-as.vector((parameters["b"]))
								var.a[i]<-as.vector((parameters["a"])) #Imax
								var.c[i]<-as.vector((parameters["c"]))
							
								#calculate x.natural
								x.natural[i]<-round(-var.c[i]-var.b[i]*log(1-sample[1,2]/var.a[i]), digits=2)
						  }
  
						}#end for loop             
					}#endif::try-error fit    
        }#endif:fit.method!="LIN"
       #=============================================================================================
	     #LIN#
			 ##two options: just linear fit or LIN fit after the EXP fit failed
                
         #set fit object, if fit objekt was not set before
         if(exists("fit")==FALSE){fit<-NA}
        
		   if ((fit.method=="EXP OR LIN" & class(fit)=="try-error") | fit.method=="LIN") {
		     
            #calculate De 
					  De<-round((sample[1,2]-fit.lm$coefficients[1])/fit.lm$coefficients[2], digits=2)

            #start loop for Monte Carlo Error estimation
            for (i in 1:NumberIterations.MC) { 
  
                data<-data.frame(x=xy$x,y=data.MC[,i])
                fit.lmMC<-lm(data$y~data$x, weights=fit.weights)
  
                #calculate x.natural
                x.natural[i]<-round((sample[1,2]-fit.lmMC$coefficients[1])/fit.lmMC$coefficients[2], digits=2)
            }#endfor::loop for MC
            
            #correct for fit.method
            fit.method<-"LIN"
            
            ##set fit object 
            if(fit.method=="LIN"){fit<-fit.lm}
                        
      }else{fit.method<-"EXP"}#endif::LIN
    }#end if EXP (this includes the LIN fit option)
		#================================================================================================
    #================================================================================================
		#EXP+LIN#
		else if (fit.method=="EXP+LIN") {
      
             
            ##try some start parameters from the input values to makes the fitting more stable
		        for(i in 1:length(a.MC)){
                    
                   a<-a.MC[i];b<-b.MC[i];c<-c.MC[i];g<-g.MC[i]
                   
                   ##------------------------------------------------------------------------------##
                   ##start: with EXP function
                   fit.EXP<-try(nls(y~fit.functionEXP(a,b,c,x),
                                data=data,
                                start=c(a=a,b=b,c=c),
                                trace=FALSE,
                                algorithm="port",
                                lower=c(a=0,b>10,c=0),
                                nls.control(maxiter=100,warnOnly=FALSE,minFactor=1/1048)
                   ),silent=TRUE)
                 
                   
                    if(class(fit.EXP)!="try-error"){
                       #get parameters out of it
                       parameters<-(coef(fit.EXP)) 
                       b<-as.vector((parameters["b"]))
                       a<-as.vector((parameters["a"])) 
                       c<-as.vector((parameters["c"]))
                      
                   ##end: with EXP function
                   ##------------------------------------------------------------------------------##
                   }
                    
                   
                    fit<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
		                  data=data,
		                  start=c(a=a,b=b,c=c,g=g),
		                  trace=FALSE,
		                  algorithm="port",
		                  lower=c(a=0,b>10,c=0,g=0),
		                  nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
		                 ),silent=TRUE)
                   
                     if(class(fit)!="try-error"){
                     #get parameters out of it
                     parameters<-(coef(fit)) 
                     b.start[i]<-as.vector((parameters["b"]))
                     a.start[i]<-as.vector((parameters["a"])) 
                     c.start[i]<-as.vector((parameters["c"]))
                     g.start[i]<-as.vector((parameters["g"])) 
                     }
                
    
                   
             }##end for loop
            
           
            ##used mean as start parameters for the final fitting
            a<-median(na.exclude(a.start))
            b<-median(na.exclude(b.start))
            c<-median(na.exclude(c.start))
            g<-median(na.exclude(g.start))
                    
            
            fit<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
  			  		data=data,
					  	start=c(a=a,b=b,c=c,g=g),
						  trace=FALSE,
              weights=fit.weights,
						  algorithm="port",
              lower=c(a=0,b>10,c=0,g=0),
						  nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
						  ))

        #if try error stop calculation        
        if(class(fit)!="try-error"){
              
						#get parameters out of it
						parameters<-(coef(fit)) 
						b<-as.vector((parameters["b"]))
						a<-as.vector((parameters["a"])) 
						c<-as.vector((parameters["c"]))
						g<-as.vector((parameters["g"]))
      
						#problem: analytic it is not easy to calculat x, here an simple approximation is made
						
              #calculate absolut diffrences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-
                (round(fit.functionEXPLIN(a,b,c,g,x=xy$x),digits=3))))
		         
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])
            
							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							i<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
					    
							#produce an iteration matrix 
							iteration.matrix<-matrix(c(i,(round(fit.functionEXPLIN(a,b,c,g,x=i),digits=3))),ncol=2)
					                 
							#select dose if Ln/Tn fits the values in the matrix
							De<-round(mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1]),digits=2)
					   
						##Monte Carlo Simulation for error estimation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error

						#set variables
						var.b<-vector(mode="numeric", length=NumberIterations.MC)
						var.a<-vector(mode="numeric", length=NumberIterations.MC)
						var.c<-vector(mode="numeric", length=NumberIterations.MC)
						var.g<-vector(mode="numeric", length=NumberIterations.MC)
					           
            ##terminal output fo MC
            cat("\n\t Execute Monte Carlo loops for error estimation of the EXP+LIN fit\n")
            
						##set progressbar
						pb<-txtProgressBar(min=0,max=NumberIterations.MC, char="=", style=3)
            
						#start Monto Carlo loops
						for (i in 1:NumberIterations.MC) { 
							
							data<-data.frame(x=xy$x,y=data.MC[,i])
							 
							fit.MC<-try(nls(y~fit.functionEXPLIN(a,b,c,g,x),
								data=data,
								start=c(a=a,b=b,c=c,g=g),
								trace=FALSE,
                weights=fit.weights,
								algorithm="port",
								nls.control(maxiter=500) #increase max. iterations
							),silent=TRUE)
              
							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA

							}else {

							parameters<-(coef(fit.MC)) 
							var.b[i]<-as.vector((parameters["b"]))
							var.a[i]<-as.vector((parameters["a"]))
							var.c[i]<-as.vector((parameters["c"]))
							var.g[i]<-as.vector((parameters["g"]))
					
							#problem: analytic it is not easy to calculat x, here an simple approximation is made
						
							#calculate absolut differences from LnTn
							differences <- data.frame(dose=xy$x,
                               differences=(sample[1,2]-(round(fit.functionEXPLIN(a=var.a[i],b=var.b[i],c=var.c[i],g=var.g[i],x=xy$x),digits=3))))
					
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])

							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							j<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
									
							#produce an iteration matrix 
							iteration.matrix<-matrix(c(j,(round(var.a[i]*(1-exp(-(j+var.c[i])/var.b[i])+var.g[i]*j),digits=3))),ncol=2)
					
							#select dose if Ln/Tn fits the values in the matrix
							x.natural[i]<-mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
							}

				  ##updata progress bar
				  setTxtProgressBar(pb, i)
              
          }#end for loop	
            
					##close
					close(pb)
            
      }#end if try-error 				
		} #End if EXP+LIN
		#================================================================================================
		#================================================================================================
		#EXP+EXP#
		else if (fit.method=="EXP+EXP") {
      
      a1.start<-NA
      a2.start<-NA
      b1.start<-NA
      b2.start<-NA
      
		  ## try to create some start parameters from the input values to make the fitting more stable
		  for(i in 1:50){
		    
		    a1<-a.MC[i];b1<-b.MC[i];
        a2<-a.MC[i]/2; b2<-b.MC[i]/2
		    
		    fit<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
		                 data=data,
		                 start=c(a1=a1,a2=a2,b1=b1,b2=b2),
		                 trace=FALSE,
		                 algorithm="port",
		                 lower=c(a1>0,a2>0,b1>0,b2>0),
		                 nls.control(maxiter=500,warnOnly=FALSE,minFactor=1/2048) #increase max. iterations
		    ),silent=TRUE)
		    
		     if(class(fit)!="try-error"){
		        #get parameters out of it
		        parameters<-(coef(fit)) 
		        a1.start[i]<-as.vector((parameters["a1"]))
		        b1.start[i]<-as.vector((parameters["b1"])) 
		        a2.start[i]<-as.vector((parameters["a2"]))
		        b2.start[i]<-as.vector((parameters["b2"]))
		     }        
        }
      
        ##use obtained parameters for fit input
		    a1<-median(na.exclude(a1.start))
        b1<-median(na.exclude(b1.start))
		    a2<-median(na.exclude(a2.start))
		    b2<-median(na.exclude(b2.start))
   
 
							#Fit curve on given values
							fit<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
									data=data,
									start=c(a1=a1,a2=a2,b1=b1,b2=b2),
									trace=FALSE,
                  weights=fit.weights,
									algorithm="port",
									nls.control(maxiter=500), #increase max. iterations
                  lower=c(a1>0,a2>0,b1>0,b2>0)
								))#end nls
             
              ##insert if for try-error      
              if (class(fit)!="try-error") {
                    
							#get parameters out of it
							parameters<-(coef(fit)) 
							b1<-as.vector((parameters["b1"]))
              b2<-as.vector((parameters["b2"]))
							a1<-as.vector((parameters["a1"])) 
              a2<-as.vector((parameters["a2"]))
              
              ##set D0 values
              D01<-round(b1,digits=2)
              D02<-round(b2,digits=2)
       
              #print D0 values
              writeLines(paste(">> D01 = ",D01,sep=""))
              writeLines(paste(">> D02 = ",D02,sep=""))
                            
        #problem: analytic it is not easy to calculat x, here an simple approximation is made
						
              #calculate absolut diffrences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-
                (round(fit.functionEXPEXP(a1,a2,b1,b2,x=xy$x),digits=3))))
		
              
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])
						 
							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							i<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
									
							#produce an iteration matrix 
							 iteration.matrix<-matrix(c(i,(round(fit.functionEXPEXP(a1,a2,b1,b2,x=i),digits=3))),ncol=2)
		              
							#select dose if Ln/Tn fits the values in the matrix
							De<-mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
							De<-round(De,digits=2)
              
            ##Monte Carlo Simulation for error estimation
						#	--Fit many curves and calculate a new De +/- De_Error
						#	--take De_Error from the simulation
            # --comparison of De from the MC and original fitted De gives a value for quality

						#set variables
						var.b1<-vector(mode="numeric", length=NumberIterations.MC)
            var.b2<-vector(mode="numeric", length=NumberIterations.MC)
						var.a1<-vector(mode="numeric", length=NumberIterations.MC)
            var.a2<-vector(mode="numeric", length=NumberIterations.MC)
					
													
						##terminal output fo MC
						cat("\n\t Execute Monte Carlo loops for error estimation of the EXP+EXP fit\n")  
            
						##progress bar
            pb<-txtProgressBar(min=0,max=NumberIterations.MC, initial=0, char="=", style=3)  
          
            #start Monto Carlo loops
						for (i in 1:NumberIterations.MC) { 
							
						  #update progress bar
						  setTxtProgressBar(pb,i)
              
							data<-data.frame(x=xy$x,y=data.MC[,i])
					
							fit.MC<-try(nls(y~fit.functionEXPEXP(a1,a2,b1,b2,x),
								data=data,
								start=c(a1=a1,a2=a2,b1=b1,b2=b2),
								trace=FALSE,
                weights=fit.weights,
								algorithm="port",
								nls.control(maxiter=500),
							  lower=c(a1>0,a2>0,b1>0,b2>0)#increase max. iterations
							),silent=TRUE)
              
							#get parameters out of it including error handling
							if (class(fit.MC)=="try-error") {
								
								x.natural[i]<-NA
                
							}else {

							parameters<-(coef(fit.MC)) 
							var.b1[i]<-as.vector((parameters["b1"]))
              var.b2[i]<-as.vector((parameters["b2"]))
       				var.a1[i]<-as.vector((parameters["a1"]))
              var.a2[i]<-as.vector((parameters["a2"]))
																	
							#problem: analytic it is not easy to calculat x, here an simple approximation is made
						
							#calculate absolut differences from LnTn
							differences <- data.frame(dose=xy$x,differences=(sample[1,2]-(round(fit.functionEXPEXP(
                a1=var.a1[i],
                a2=var.a2[i],
                b1=var.b1[i],
                b2=var.b2[i],
                x=xy$x),digits=3))))
					
							#set upper and lower boundary for searching (really timesaving)
							boundary.upper<-unique(differences[differences[,2]==max(differences[differences[,2]<=0,2]),])
							boundary.lower<-unique(differences[differences[,2]==min(differences[differences[,2]>=0,2]),])

							#case that there is no upper regeneration point...set a artificial point 20% above the highest reg Point 
							if (length(boundary.upper[,1])==0){
											artificialRegPoint<-max(xy$x)+max(xy$x)*0.2
											boundary.upper[1,1]<-artificialRegPoint									
											}

							#write all boundary values in a vector
							j<-seq(boundary.lower[,1],boundary.upper[,1],by=0.01)
									
							#produce an iteration matrix 
							iteration.matrix<-matrix(c(j,(round(
                                           (var.a1[i]*(1-exp(-(j)/var.b1[i])))+
                                           (var.a2[i]*(1-exp(-(j)/var.b2[i]))) ,
                                           digits=3))),ncol=2)
					
							#select dose if Ln/Tn fits the values in the matrix
							x.natural[i]<-mean(iteration.matrix[iteration.matrix[,2]==round(sample[1,2],digits=3),1])
              
              
							} #end if "try-error" MC simulation
                                       
						} #end for loop
      
          } #end if "try-error" Fit Method
      
        ##close
        close(pb)    
    #================================================================================================
		} #End if Fit Method  
      
      
      
		#Get De values from Monto Carlo simulation
		
			#calculate mean and sd (ignore NaN values)
			De.MonteCarlo<-round(mean(na.exclude(x.natural)),digits=2)
					
			#De.Error is Error of the whole De (ignore NaN values)
			De.Error<-round(sd(na.exclude(x.natural)),digits=2)
    

##=================================================================================================##
# PLOTTING ------------------------------------------------------------------------------------
##=================================================================================================##


##5. Plotting if plotOutput=TRUE
if(output.plot==TRUE) {
  
      ##cheat the R check
      x<-NULL; rm(x)

#PAR	#open plot area
      if(output.plot==TRUE & output.plotExtended==TRUE){
			layout(matrix(c(1,1,1,1,2,3), 3, 2, byrow=TRUE), respect=TRUE)
			par(cex=0.8*cex.global)
      }else{par(mfrow=c(1,1),cex=cex.global)}
  
#PLOT		#Plot input values
			plot(xy[1:fit.NumberRegPointsReal,1],xy[1:fit.NumberRegPointsReal,2],
				main=main,
				ylim=c(0,(max(xy$y)+if(max(xy$y)*0.1>1.5){1.5}else{max(xy$y)*0.2})),
				xlim=c(0,(max(xy$x)+if(max(xy$x)*0.4>50){50}else{max(xy$x)*0.4})),
				pch=19,
				xlab=if(xlab=="Gy"){"Dose [Gy]"}else{"Dose [s]"},
				ylab=expression(L[x]/T[x]))

#CURVE	#plot fitted curve
			if (fit.method=="EXP+LIN") {try(curve(a*(1-exp(-(x+c)/b)+(g*x)), lwd=1.5, add=TRUE))}
      else if (fit.method=="LIN") {curve(fit.lm$coefficients[2]*x+fit.lm$coefficients[1],lwd=1.5, add=TRUE)}
			else if (fit.method=="EXP") {try(curve(fit.functionEXP(a,b,c,x), lwd=1.5, add=TRUE))}
      else if (fit.method=="EXP+EXP") {try(curve(fit.functionEXPEXP(a1,a2,b1,b2,x),lwd=1.5,add=TRUE))}

##POINTS	#Plot Reg0 and Repeated Points
			#Repeated Point
      points(xy[which(duplicated(xy[,1])),1],xy[which(duplicated(xy[,1])),2], pch=2)
      
      #Reg Point 0
      points(xy[which(xy==0),1],xy[which(xy==0),2], pch=1)
   
##ARROWS	#y-error Bars

      segments(xy$x,xy$y-y.Error,xy$x,xy$y+y.Error)
	
##LINES	#Insert Ln/Tn
			try(lines(c(0,De),c(sample[1,2],sample[1,2]), col="red", lty=2,lwd=1.25),silent=TRUE)
			try(lines(c(De,De),c(0,sample[1,2]), col="red", lty=2, lwd=1.25),silent=TRUE)
			try(points(De,sample[1,2], col="red", pch=19),silent=TRUE)

##TEXT		#Insert fit and result
			try(mtext(side=3, substitute(D[e] == De, 
                                list(De=paste(De,"+/-",De.Error, xlab,
                                " | fit: ",fit.method))), line=0.3, cex=0.8*cex.global),silent=TRUE)
	
			#write error message in plot if De is NaN
			try(if (De=="NaN") {
				text(sample[2,1],0,"Error: Fit not valid. At least one parameter is negative!", 
         adj=c(0,0), cex=0.8, col="red")
			},silent=TRUE)
	
##LEGEND	#plot legend
			
			legend("topleft", c("REG Points", "REG Point repeated", "REG Point 0"),
          pch=c(19,2,1), cex=0.8*cex.global, bty="n")

##plot only if wanted
      if(output.plot==TRUE & output.plotExtended==TRUE){
##HIST		#try to plot histogramm of De values from the Monte Carlo simulation
			par(cex=0.7*cex.global)
      
			##plot histogram for frequency axis
      try(histogram<-hist(
          x.natural,
          freq=TRUE,
          col="white",
          border="white",
          xlab="",
          xaxt="n",
          main="" 
      ))
                  
        par(new=TRUE)
  			try(histogram<-hist(
  				x.natural,
  				xlab=if(xlab=="Gy"){"Dose [Gy]"}else{"Dose [s]"},
  				main=expression(paste(D[e], " from Monte Carlo simulation")),
          freq=FALSE,
  				sub=paste("n.iterations = ", NumberIterations.MC,", valid fits =",length(na.exclude(x.natural))),
  				col="grey",
          ylab="",
          yaxt="n",
  			))#end plot hist
			      
			#to avoid errors plot only if histogram exists
			if (exists("histogram")) {
        
			  ##add rug
			  rug(x.natural)
             
        ##add normal curve
			  curve(dnorm(x,mean=mean(na.exclude(x.natural)),sd=sd(na.exclude(x.natural))),col="red",add=TRUE)
                        
				#write De + Error from Monte Carlo simulation + write quality of error estimation
		  	try(mtext(side=3,substitute(D[e[MC]] == De, 
              list(De=paste(De.MonteCarlo,"+/-",De.Error,
              " | quality = ",round((1-abs(De-De.MonteCarlo)/De)*100,
              digits=1),"%"))),cex=0.6*cex.global),silent=TRUE)
          
			} else {plot(NA,NA,xlim=c(0,10), ylim=c(0,10), main=expression(paste(D[e], " from Monte Carlo simulation")))
				text(5,5,"not available")
			}#end ifelse

		
##PLOT		#PLOT test dose response curve if available if not plot not available
			#plot Tx/Tn value for sensitiviy change
		
			if ("TnTx" %in% colnames(sample)==TRUE) {
		
				plot(1:length(sample[,"TnTx"]),sample[1:(length(sample[,"TnTx"])),"TnTx"]/sample[1,"TnTx"],
				xlab="SAR cycle",
				ylab=expression(paste(T[n]/T[x])),
				main="Test Dose Response",
				type="o",
				pch=20,
				)
	
##LINES		#plot 1 line
				lines(c(1,length(sample[,"TnTx"])),c(1,1), lty=2, col="gray")
				} else {
			
			 	plot(NA,NA,xlim=c(0,10), ylim=c(0,10), main="Test Dose Response")
				text(5,5,"not available\n no TnTx column")
			}#end if else

##Additional mtext
mtext(side=4,mtext,outer=TRUE,line=-1.5,cex=0.6,col="blue")
		
	
##END lines
  }#endif::output.plotExtended
}#end if plotOutput	

    ##RETURN - return De values an parameter
	  output<-try(data.frame(De=De,De.Error=De.Error, D01=D01, D02=D02, Fit=fit.method),silent=TRUE)
    return(list(De=output,Fit=fit))	
  
}#EOF
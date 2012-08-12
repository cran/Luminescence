##//////////////////////////////////////////////
##//Calc_FuchsLang2001.R
##/////////////////////////////////////////////

##======================================
#author: Sebastian Kreutzer
#organisation: University of Bayreuth
#vers.: 0.3
#date: 16/04/2012
##======================================
##+++++++++++++++++++++++Preface+++++++++++++++++++++++(START)
## (A) Input is data.frame() with two columns De and De_Error
## 
##(B) 
## 1. Output is a PDF
## 2. If the cv of the first two D[e] values > cvThreshold, 
##		the frist D[e] is skipped. If you want to use only values
##		above the second or third values use the startDeValue parameter
##3. Function based on: 
##		Fuchs, M.;Lang, A. (2001): OSL dating of coarse-grain fluvial quartz using single-aliqout
##		protocols on sediments from NE Peloponnese, Greece. In: Quaternary Science Reviews (20), p. 783-787
##	
##		Short description: 
##		
##		(1) Estimate natural relative variation of the sample via DRT
##		(2) Orderd D[e] values of sample in increasing order
##		(3) Calculate a running mean, starting with the lowermost two values, than add 
##			 one value each step
##		(4) Stop if the value c[v] is exceed 
##		(5) plot figure 
##
##############################################################

Calc_FuchsLang2001 <- function(
                        sample,#data frame 
                        sample.mtext="unkown sample",
                        sample.id=sample.mtext,
                        cvThreshold=5, #variation coefficient in %
                        startDeValue=1, #D[e] value from which the calculation starts
                        
                        output.plot=TRUE,
                        output.terminal=TRUE,
											
												main="Fuchs & Lang (2001)",#alternative name for title 
												xlab=expression(paste(D[e]," [Gy]")),
                        cex.global=1
									) {

##=================================================================================================##
##PREPARE DATA
##=================================================================================================##

	##1. order values in acending order write used D[e] values in data.frame
	o <- order(sample[1]) # o is only an order parameter 
	sample_ordered <- sample[o,] # sort values after o and write them into a new variable

	##2. estimate D[e] 

		# set variables 
		usedDeValues<-data.frame(De=NA,De_Error=NA,cv=NA)
		endDeValue<-startDeValue

		# if the frist D[e] values are not used write this information in the data.frame
		if (startDeValue!=1) {
			
			n <- abs(1-startDeValue)

			#  write used D[e] values in data.frame
			usedDeValues[1:n,1]<-sample_ordered[1:n,1]
			usedDeValues[1:n,2]<-sample_ordered[1:n,2]
			usedDeValues[1:n,3]<-"skipped"
		}

##=================================================================================================##
##LOOP FOR MODEL
##=================================================================================================##  
  
	# repeat loop (run at least one time)
	repeat {
	
		#calculate mean, sd and cv
		mean<-round(mean(sample_ordered[startDeValue:endDeValue,1]),digits=2) #calculate mean from ordered D[e] values
		sd<-round(sd(sample_ordered[startDeValue:endDeValue,1]),digits=2)		#calculate sd from ordered D[e] values
		cv<-round(sd/mean*100, digits=2) #calculate coefficent of variation


			# break if cv > cvThreshold
			if (cv>cvThreshold & endDeValue>startDeValue){
			
				 # if the first two D[e] values give a cv > cvThreshold, than skip the first D[e] value	
				 if (endDeValue-startDeValue<2) {
						
						#  write used D[e] values in data.frame
						usedDeValues[endDeValue,1]<-sample_ordered[endDeValue,1]
						usedDeValues[endDeValue,2]<-sample_ordered[endDeValue,2]
						usedDeValues[endDeValue-1,3]<-"not used"

						# go to the next D[e] value
						startDeValue<-startDeValue+1

					} else {
					
					 	usedDeValues[endDeValue,1]<-sample_ordered[endDeValue,1]
						usedDeValues[endDeValue,2]<-sample_ordered[endDeValue,2]
						usedDeValues[endDeValue,3]<-paste("# ",cv," %",sep="")
					
						break #break loop
					}

				}#EndIf
				else {
			
					# write used D[e] values in data.frame
					usedDeValues[endDeValue,1]<-sample_ordered[endDeValue,1]
					usedDeValues[endDeValue,2]<-sample_ordered[endDeValue,2]
				
					# first cv values alway contains NA to ensure that NA% is not printed test    
      		if(is.na(cv)==TRUE) {
						usedDeValues[endDeValue,3]<-cv
					} else {
						usedDeValues[endDeValue,3]<-paste(cv," %",sep="")
					}
				}#EndElse

			# go the next D[e] value until the maximum number is reached
			if (endDeValue<length(sample_ordered[,1])) {
				endDeValue<-endDeValue+1
			} else {break}	

	}#EndRepeat

##=================================================================================================##
##ADDITIONAL CALCULATIONS and TERMINAL OUTPUT
##=================================================================================================##
  
	# additional calculate weighted mean
	w<-1/(sample_ordered[startDeValue:endDeValue,2])^2 #weights for weighted mean
	weighted_mean <- round(weighted.mean(sample_ordered[startDeValue:endDeValue,1], w), digits=2)
	weighted_sd<-round(sqrt(1/sum(w)),digits=2)
	n.usedDeValues<-endDeValue-startDeValue+1

	# standard error
	se <- round(sd/sqrt(endDeValue-startDeValue+1), digits=2)
  
  if(output.terminal==TRUE){
    cat("\n [Calc_FuchsLang2001]")
	  cat(paste("\n ---------------------------------"))
	  cat(paste("\n cvThreshold:            ",cvThreshold,"%"))
	  cat(paste("\n used values:            ",n.usedDeValues))
	  cat(paste("\n ---------------------------------"))
	  cat(paste("\n mean:                   ",mean))
	  cat(paste("\n sd:                     ",sd))
	  cat(paste("\n weighted mean:          ",weighted_mean))
	  cat(paste("\n weighted sd:            ",weighted_sd))
	  cat(paste("\n ---------------------------------"))
  }
##=================================================================================================##
##PLOTTING
##=================================================================================================##
 
if(output.plot==TRUE){
par(cex=cex.global,mfrow=c(1,1))
  
##PLOT  
	counter<-seq(1,max(o))
	plot(NA,NA,
		ylim=c(min(o)-1,max(o)+3),
		xlim=c((min(sample_ordered[,1])-sample_ordered[1,2]),(max(sample_ordered[,1])+sample_ordered[max(o),2])),
		xlab=xlab,
		ylab="# Aliquots",
		main=main
		)

##SEGMENTS
	segments(sample_ordered[,1]-sample_ordered[,2],1:length(sample_ordered[,1]),
	         sample_ordered[,1]+sample_ordered[,2],1:length(sample_ordered[,1]),
	         col="gray"
	         )
  
##POINTS
	points(sample_ordered[,1], counter,pch=19)
  
##LINES
    ##BOUNDARY INFORMATION 
    ##lower boundary
		lines(c(
			usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1], #boundary_counter for incorporate skipped values
			usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]),
			c(min(o)-0.5,max(o)+0.5),
			col="red",
			lty="dashed"
			)

		#upper boundary
		lines(c(max(usedDeValues[,1]),max(usedDeValues[,1])),c(min(o)-0.5,max(o)+0.5),col="red",lty="dashed")

		#plot some further informations into the grafik
		arrows(
			usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]+usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]*0.02, #x1
			max(o)+0.5, #y1
			max(usedDeValues[,1]-usedDeValues[,1]*0.02), #x2
			max(o)+0.5, #y2,
			code=3,
			length=0.03
		)
		text(
			c(
			usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1], 
			usedDeValues[length(usedDeValues[,1])-n.usedDeValues+1,1]),
			c(max(o)+2,max(o)+2),		
			labels=paste("used values = ",n.usedDeValues),
			cex=0.6*cex.global,
			adj=0			
			)

##MTEXT
  
  mtext(side=3,sample.mtext,cex=1.2*cex.global)
}#endif::output.plot 
##=================================================================================================##
##RETURN  VALUES
##=================================================================================================##  
  
  ##combine statistic parameters
  results<-data.frame(id=sample.id,
                      mean=mean,sd=sd,weighted_mean=weighted_mean,weighted_sd=weighted_sd,n.usedDeValues)
  
  return(list(results=results,usedDeValues=usedDeValues))  
}#EndOf function
#EOF

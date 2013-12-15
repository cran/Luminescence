calc_FuchsLang2001<- structure(function(#Apply the model after Fuchs & Lang (2001) to a given De distribution.
  ### This function applies the method according to Fuchs & Lang (2001) for 
  ### heterogeneously bleached samples with a given coefficient of variation threshold. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.3.1 [2012-12-10]
  # ===========================================================================

  sample,
  ### \link{data.frame} (\bold{required}): two column data frame, e.g. De and De error
  
  sample.mtext = "unknown sample",
  ### \link{character} (optional): mtext for optional plot (top)
  
  sample.id = sample.mtext,
  ### \link{character} (with default): sample id, with default the sample.mtext is used.
  
  cvThreshold = 5, 
  ### \link{numeric} (with default): coefficient of variation in percent, 
  ### as threshold for the method, e.g. \code{cvThreshold = 3}. See details.
  
  startDeValue = 1, 
  ### \link{numeric} (with default): number of the first aliquot that is used 
  ### for the calculations
                        
  output.plot = TRUE,
  ### \link{logical} (with default): plot output \code{TRUE}/\code{FALSE}
  
  output.terminal = TRUE,
  ### \link{logical} (with default): terminal output \code{TRUE}/\code{FALSE}
  
	main = "Fuchs & Lang (2001)",
  ### \link{character} (with default): title of the plot (works as in \link{plot})
  
  xlab = expression(paste(D[e]," [Gy]")),
  ### \link{character} (with default): xlab works as in \link{plot}
  
  cex.global = 1
  ### \link{numeric} (with default): global scaling factor

){

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
    cat("\n [calc_FuchsLang2001]")
	  cat(paste("\n ---------------------------------"))
	  cat(paste("\n cvThreshold:            ",cvThreshold,"%"))
	  cat(paste("\n used values:            ",n.usedDeValues))
	  cat(paste("\n ---------------------------------"))
	  cat(paste("\n mean:                   ",mean))
	  cat(paste("\n sd:                     ",sd))
	  cat(paste("\n weighted mean:          ",weighted_mean))
	  cat(paste("\n weighted sd:            ",weighted_sd))
	  cat(paste("\n ---------------------------------\n\n"))
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
##=================================================================================================#
##RETURN  VALUES
##=================================================================================================##  
  
  ##combine statistic parameters
  results<-data.frame(id=sample.id,
                      mean=mean,sd=sd,
                      weighted_mean=weighted_mean,
                      weighted_sd=weighted_sd,
                      n.usedDeValues)
  
  return(list(results=results,usedDeValues=usedDeValues))  
  
	# DOCUMENTATION - INLINEDOC LINES -----------------------------------------
	
	##details<<
  ## \bold{Used values} \cr
  ## If the coefficient of variation (c[v]) of the first two values is larger than 
  ## the threshold c[v_threshold], the first value is skipped. 
  ## Use the \code{startDeValue} argument to define a start value for 
  ## calculation (e.g. 2nd or 3rd value).\cr
	##
	## \bold{Basic steps of the approach} \cr
	##
	## (1) Estimate natural relative variation of the sample using a dose recovery test\cr
	## (2) Sort the input values ascendingly\cr
	## (3) Calculate a running mean, starting with the lowermost two values and 
  ## add values iteratively.\cr
  ## (4) Stop if the calculated c[v] exceeds the specified \code{cvThreshold}\cr
	
	##value<<
  ## A plot and terminal output is provided if desired. In addition, a 
  ## list is returned containing two elements:
  ## \item{results}{\link{data.frame} with stastical parameters, e.g. mean, sd, ...}
  ## \item{usedDeValues}{\link{data.frame} containing the used values for the calculation}
	
	##references<<
  ## Fuchs, M. & Lang, A., 2001. OSL dating of coarse-grain fluvial quartz using 
  ## single-aliqout 	protocols on sediments from NE Peloponnese, 
  ## Greece. In: Quaternary Science Reviews (20), pp. 783-787.
	##
	## Fuchs, M. & Wagner, G.A., 2003. Recognition of insufficient bleaching by 
  ## small aliquots of quartz for reconstructing soil erosion in Greece. 
  ## Quaternary Science Reviews, 22, pp. 1161-1167. 
	
	##note<<
	## Please consider the requirements and the constraints of this method 
  ## (see Fuchs & Lang, 2001)
  
	##seealso<<
	## \code{\link{plot}}, \code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}
  ## \code{\link{calc_FiniteMixture}}, \code{\link{calc_CentralDose}}, 
  ## \code{\link{calc_CommonDose}}
	
	##keyword<<
	## dplot
  
}, ex=function(){
  
  ##load example data
  data(ExampleData.DeValues, envir = environment())
  
  ##calculate De according to Fuchs & Lang (2001)
  calc_FuchsLang2001(ExampleData.DeValues, cvThreshold = 5)
  
})#END OF STRUCTURE

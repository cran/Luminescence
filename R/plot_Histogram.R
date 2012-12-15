##//////////////////////////////////////////////
## plot_Histogram.R
##//////////////////////////////////////////////
##
##======================================
##author: Sebastian Kreutzer
##version: 0.4
##date: 04/05/2012
##======================================
##description:
##		--	plots histogram with standard error
##			  idea from Rex Galbarith
##		-- input is: data.frame: value[,1] value.error[,2]

plot_Histogram <- function (
						values,
						main="Histogram",
						mtext="", #further material information
						xlab=expression(paste(D[e]," [Gy]")), #for plot xlab unit, possible "s",Gy" or "ka"
					  cex.global=1,
            breaks="Sturges",
            normal_curve=FALSE #add a normal curve on the plot
           
) {

##====================================================================
##DO NOT EDIT BELOW THIS POINT
##====================================================================

par(mfrow=c(2,1),cex=cex.global)

  ##get breaks for xlim and axTicks 
  breaks<-hist(values[,1],plot=FALSE)$breaks


  ##1. plot standard error
  plot(values[,1:2],
	  	xlim=range(breaks),
		  frame.plot=FALSE,
	  	xlab=xlab,
		  ylab="Standard Error",
		  main=main)

  ##add mtext (sample code etc.)
  mtext(side=3,mtext,cex=0.8*cex.global)  

  ##2. plot histogram
  hist(values[,1],
	  	xlim=range(breaks),
		  main="",
      xlab=xlab,
      breaks=breaks,
      freq=if(normal_curve==FALSE){TRUE}else{FALSE}
  )
  
  ##2.1 add number of values
  mtext(side=3,paste("n = ",length(values[,1]),sep=""), adj=0.05,padj=-4.5,cex=1*cex.global)

  ##3. Add a rug
  rug(values[,1],col="#00000088")

  ##4. Add a normal curve based on the data if wanted
  if(normal_curve==TRUE){
    x<-NULL;rm(x) #cheat the R check routine
    curve(dnorm(x,mean=mean(na.exclude(values[,1])),sd=sd(na.exclude(values[,1]))),col="red",add=TRUE,lwd=1.2*cex.global)
  }
}#End of function

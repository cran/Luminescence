##//////////////////////////////////////////////
##//plot_DeDistribution.R
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen, Germany
#vers.: 2.2.1
#date: 12/05/2012
##======================================
##improvements: see Maggi the Fox mail from 02.03.2012


plot_DeDistribution <- function(					
                        values, #De values for plotting (data.frame)
												main=expression(paste(D[e], " Distribution")), #alternative name for title 
                        distribution.parameter="",
												mtext="",#further material information
												bw="nrd0", #vary bin-width if necessary, choose a numeric value for manual setting
												xlab=expression(paste(D[e], " [Gy]")),
												cex.global=1 #global scale factor 																	
									) {
##====================================================================
##DO NOT EDIT BELOW THIS POINT
##====================================================================

	##1. Sort data set in increasing order
	o <- order(values[,1]) # o is order parameter
	values <- values[o,] # sort for order parameter

	##2. Add some calculations
	density<-density(values[,1],kernel="gaussian",bw=bw) #calculate density function
	n<-length(values[,1]) # numvber of values in the plot
	mean<-round(mean(values[,1]),digits=2) # mean
	sd<-round(sd(values[,1]),digits=2) # standard deviation
	
	par(oma=c(0,0,0,2),cex=1*cex.global, mfrow=c(1,1)) 
  
	##3. Density function
	plot(NA,NA, 
    xlim=range(density$x),
    ylim=range(density$y),
	  xaxt="n",
    xlab="",
		ylab="density")
    
    ##3.1 add sd, mean, meadian
    if(length(grep("sd",distribution.parameter))>0){
	    polygon(x=c(mean(values[,1])-sd(values[,1]), mean(values[,1])+sd(values[,1]),
	                mean(values[,1])+sd(values[,1]), mean(values[,1])-sd(values[,1])
                  ),
              y=c(-1,-1,max(density$y)+1,max(density$y)+1), 
              col="gray86",
              border=NA)
     }
  
   if(length(grep("mean",distribution.parameter))>0){
	   abline(v=mean(values[,1]))
	   text(mean(values[,1])+mean(values[,1]*0.005),max(density$y),"mean", srt=90, pos=1,cex=.7*cex.global)
	  }
	 if(length(grep("median",distribution.parameter))>0){
	   abline(v=median(values[,1]), lty=2)
	   text(median(values[,1])+median(values[,1]*0.005),max(density$y),"median", srt=90, pos=1,cex=.7*cex.global)
	 }

  	##3.2 add density function
	  lines(density, col="#3F489D",lty=1,lwd=1.25*cex.global)
    
  
  
  ##4. plot values	
	par(new=TRUE,oma=c(0,0,0,2),cex=1*cex.global)
    		 
	plot(values[,1],
		1:length(values[,1]), 
	   xlab=xlab, 
		yaxt="n", 
		ylab="", 
		pch=20, 
		xlim=c( 
				min(data.frame(density[1])), #minimum value density plot
				max(data.frame(density[1])) #maximum value density plot
		      ),
		main=main, # empty title
		frame.plot=FALSE
		 )
	
	##5. plot error bars
	arrows(values[,1]-values[,2]/2, #x1 Wert
			1:length(values[,1]), # y1 Wert
			values[,1]+values[,2]/2, # x2 Wert
			1:length(values[,1]), # y2 Wert
			code=3,
			angle=90, # Wert für die Ausrichtung der Pfeilenden in Verhaeltnis zum Pfeil (hier vertikal)
			length=.05) # Laenge der Endstuecken
			
	#5.1 2th y-axis
	axis(side=4)
	mtext("cumulative frequency",side=4, padj=4,cex=1*cex.global)

	##6. Additional statistical information
	
		#6.1 number of values in plot (plotted in left upper margin) 
		mtext (side=3,padj=2, substitute("n" == n, list(n=n)) , col="black", adj=0.025, cex=0.8*cex.global)
		mtext (side=3,padj=3.4, substitute("mean" == mean, list(mean=mean)) , col="black", adj=0.025, cex=0.8*cex.global)
		mtext (side=3,padj=5, substitute("sd" == sd, list(sd=sd)) , col="black", adj=0.025, cex=0.8*cex.global)
	
	##7. further information on the graphic
		
		#values.info
		mtext(side=3, mtext,cex=0.8*cex.global)		
  
}#EndOf function
##==================================================================================================##
##EOF##

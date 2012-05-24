##//////////////////////////////////////////////
##//plot_RadialPlot.R 
##/////////////////////////////////////////////
##
##======================================
#author: Sebastian Kreutzer 
#organisation: JLU Giessen
#vers.: 0.2.2
#date: 19/04/2012
#nota bene: based on a rewritten S script of Rex Galbraith, 2010
##======================================


##==================================================================
##start function
plot_RadialPlot <- function (						
						sample,        #sample input as.data.frame (x,y)
                    
            sample.groups, #allows grouping of sample data, e.g. sample.group=list(c(1:20),c(2:20))
            sample.legend,
            sample.lty=1,
            sample.pch=1,
            sample.col="black",
            sample.mtext="default",
           
            zscale.log=TRUE,
                   			                        
						zaxis.scale, #range of z-axis
            zaxis.group_circle=FALSE,
										
            yaxis.scale, #e.g. c(15,-15); if no value is chosen the yaxis.scale is roughly estimated
            plot.2sigmaRange=TRUE,
						plot.area_ratio=4.5/6, #set plot facto (width and height)
						
            zlab=expression(paste(D[e], " [Gy]")),
						main=expression(paste(D[e]," Distribution", sep="")), #title of plot
						
            cex.global=1,
            xscale_factor=1.01 #for scaling thickmarks
					

						) {

##=================================================================================================##
##CONSISTENCY CHECKS OF THE INPUT - suggested by Rex Galbraith
##=================================================================================================##  

    ##(1) - check if the input is a data.frame
    if(is.data.frame(sample)==FALSE){stop("Error: Sample has to be of type data.frame!")}
    
    ##(2) - check if the input contain more than two columns
    if(length(sample[1,])>2){
            
          ##try to find value by column name
         if(("x" %in% colnames(sample)==TRUE & "y" %in% colnames(sample)==TRUE) |
            ("de" %in% colnames(sample)==TRUE & "se" %in% colnames(sample)==TRUE) | 
            ("ED" %in% colnames(sample)==TRUE & "ED_Error" %in% colnames(sample)==TRUE)
            ){
           
           ##Output
           cat("\n [plot_RadialPlot]: Input has more than two columns, try to select columns by name...")
                      
           if(("x" %in% colnames(sample)==TRUE & "y" %in% colnames(sample)==TRUE)){
             
             sample<-data.frame(sample$x,sample$y)
             cat("\n [plot_RadialPlot]: Column 'x' and 'y' chosen as input.")
             
           }else if("de" %in% colnames(sample)==TRUE & "se" %in% colnames(sample)==TRUE){
             
             sample<-data.frame(sample$de,sample$se)
             cat("\n [plot_RadialPlot]: Column 'de' and 'se' chosen as input.")
             
           }else if(("ED" %in% colnames(sample)==TRUE & "ED_Error" %in% colnames(sample)==TRUE)){
             
             sample<-data.frame(sample$ED,sample$ED_Error)
             cat("\n [plot_RadialPlot]: Column 'ED' and 'ED_Error' chosen as input.")             
           }           
         }else{stop("Error: No matching columns found, please specify a data.frame with two columns!")} 
    }
    

##=================================================================================================##
##PREVIOUS CALCULATIONS
##=================================================================================================##
  
  
##1. Grouping

  ##set trigger 
  sample.groups.trigger=TRUE
  
  ##if no grouping has been done a single group is set
  if(missing(sample.groups)==TRUE){sample.groups<-list(c(1:length(sample[,1])))
                                   sample.groups.trigger=FALSE
                                   }          
  
  
##2. Do some calculations for the plot according Galbraith, 1988

  	##a)	n estimates z1,z2,...,zn - use log value - option to log the De (not the scale!)
		if(zscale.log==TRUE){z.i<-log(sample[,1])}else{z.i<-sample[,1]}					
  
		##b)	with correspondening relative standard errors s1,s2,...,sn
    if(zscale.log==TRUE){se.i<-sample[,2]/sample[,1]}else{se.i<-sample[,2]}
		
  	##c) calculate convenient central value as the weighted average
		z.0<-(sum(z.i/(se.i^2)))/(sum(1/(se.i^2)))
  
   
		##d) 	caculate values for scatter plot
		x.i<-1/se.i  #inverse standard error from every se (precision)
		y.i<-(z.i-z.0)/se.i #standard estimate

        
##3. Calculate some variables for the circle
	
		##a) 	calculate conversion factor for x,y values
		h <- plot.area_ratio*(max(x.i)-min(x.i))/((-min(y.i))+max(y.i))
			
		##b) 	calculate appropiate radius (taken from the S-script of G.Galbraith)
		r.0 <- max(sqrt((x.i)^2+(y.i*h)^2))
		
		##d) 	calculate values for semi circle scale
	    
      ##pretty() calculates breakpoints
      ##with correction for small scatter
  
      if(missing("zaxis.scale")==FALSE){
        mkest<-zaxis.scale
        }else{
          if(zscale.log==TRUE){
                        
                          mkest<-round(exp(pretty(z.i)),digits=0)
                         
                          
                          ##recalculate for small scatter
                          if(sd(z.i)/mean(z.i)<0.05){
                            mkest<-round(exp(pretty(c(min(z.i)-min(z.i)*0.02,max(z.i)+max(z.i)*0.02))),digits=0)
                          }
                          
                         }else{
                           
                           ##recalculate for small scatter
                           mkest<-round(pretty(z.i),digits=0)
                           if(sd(z.i)/mean(z.i)<0.05){
                             mkest<-pretty(c(min(z.i)-min(z.i)*0.1,max(z.i)+max(z.i)*0.1))
                           }
                         }        
        }##endif::missing zaxis.scale 
          
				  ## calculate the x,y coordinates of the semi circle
        	mkr<- range(mkest)                # default range
  		
  				  ## choose the range of the circluar scale on the next line,
  				  circ.x<- (0:300)/300
        		circ.x<- mkr[1]+circ.x*(mkr[2]-mkr[1]) 
           	if(zscale.log==TRUE){circ.z<-log(circ.x)}else{circ.z<-circ.x} # convert to z-values 
            circ.x<- r.0/(sqrt(1+h^2*(circ.z-z.0)^2))    # convert to x,y values
            circ.y<- (circ.z-z.0)*circ.x
        		
            ##write the values in a data.frame (used for the polygone etc.)
            circ.data_frame<-data.frame(x=circ.x,y=circ.y,z=circ.z)
     
        ##e) 	label for circle
        
      		  ## labels on the radial scale
       			if(zscale.log==TRUE){zmkest<-log(mkest)}else{zmkest<-mkest}
        		xmk1<- r.0/sqrt(1+h^2*(zmkest-z.0)^2)
        	 	ymk1<- (zmkest-z.0)*xmk1
       	 	 	xmk2<- xmk1*xscale_factor
           	ymk2<- (zmkest-z.0)*xmk2
           	xmk3<- (xscale_factor*1.04)*r.0/sqrt(1+h^2*(zmkest-z.0)^2)
           	ymk3<- (zmkest-z.0)*xmk3
				
##4. calculate y axis range - if spread > 12 then use real value
    
     yaxis.max<-if(max(abs(y.i))>12){
                  max(abs(y.i))
                 }else if(max(abs(y.i))<3){
                  8
                 }else{12}
        

##=================================================================================================##
##PLOT I: SEMI CIRCLE - z.scale
##=================================================================================================##  

##PAR
 
  par(mfrow=c(1,1),
       oma=c(2,0,0,0),
       mar=c(5,4,4,4.5),
       xpd=TRUE, #clipping of figure area
       las=1,
       cex=1*cex.global
    )

##PLOT
  
    plot(NA,NA,
    xlim=c(0,max(x.i)), 
    ylim=if(missing("yaxis.scale")==FALSE){yaxis.scale}else{c(-yaxis.max,yaxis.max)}, #note: y.i is a negative value
    xlab="",ylab="",
    yaxt="n",
    xaxt="n",
    main=main,
    bty="n",
    typ="n",
    xaxs="i", #intersection x axis and yaxis is 0 (without alignment)
    yaxs="i"
    )

##POLYGON
  
  if(plot.2sigmaRange==TRUE){
    
    pgx<-c(0,
           mean(circ.data_frame[round(circ.data_frame$y,digits=0)==2 ,"x"]),
           rev(circ.data_frame[circ.data_frame$y<2 & circ.data_frame$y>0 ,"x"]),
           rev(circ.data_frame[circ.data_frame$y>-2 & circ.data_frame$y<0 ,"x"]),
           mean(circ.data_frame[round(circ.data_frame$y,digits=0)==-2 ,"x"]),
           0)
    
    
    pgy<- c(2,2,
            rev(circ.data_frame[circ.data_frame$y<=2 & circ.data_frame$y>0 ,"y"]),
            rev(circ.data_frame[circ.data_frame$y>=-2 & circ.data_frame$y<0 ,"y"]),
            -2,-2)    
        
    polygon(pgx,pgy,border=F,col="grey84")    
  }
  
##CENTRAL LINES
 
  if(missing(sample.groups.trigger)==TRUE){
     
    ##insert central lines 
    for (i in 1:length(sample.groups)){
    
      ##(a) - central lines
      ##calculate point on z-scale of the line as weighted average      
      
      z.i.groups<-z.i[unlist(sample.groups[i])]
      se.i.groups<-se.i[unlist(sample.groups[i])]
      z.i_wmean <- (sum(z.i.groups/(se.i.groups^2)))/(sum(1/(se.i.groups^2)))
      
      ##calculate new y value (based on the slope which is in general z.i-z.0)
      y.i.groups<-(z.i_wmean-z.0)*max(x.i)*1.05
  
      ##weightes mean lines - lines
      lines(c(0,round(max(x.i),digits=0)),c(0,y.i.groups),lty=sample.lty[i], col=sample.col[i], lwd=1.5)
    
      if(zaxis.group_circle==TRUE){
      
        ##calculate z values for the 95% confidence interval
        if(zscale.log==TRUE){y.i.groups_lower<-(exp(((y.i.groups-2)/(max(x.i)*1.05))+z.0))
                        y.i.groups_upper<-(exp(((y.i.groups+2)/(max(x.i)*1.05))+z.0))
        }else{
           y.i.groups_lower<-((y.i.groups-2)/(max(x.i)*1.05))+z.0
           y.i.groups_upper<-((y.i.groups+2)/(max(x.i)*1.05))+z.0
        }
      
        ##small semi circle for the values       
        circ.x.groups<-seq(y.i.groups_lower,y.i.groups_upper,by=0.1)
        if(zscale.log==TRUE){circ.z.groups<-log(circ.x.groups)}else{circ.z.groups<-circ.x.groups}
      
        circ.x.groups<- r.0/(sqrt(1+h^2*(circ.z.groups-z.0)^2))# convert to x,y values
        circ.y.groups<-(circ.z.groups-z.0)*circ.x.groups
      
        ##plot 2-sigma range on the semi circle
        lines(circ.x.groups*1,circ.y.groups, col=sample.col[i],lwd=2,lty=sample.lty[i])
      }#end if semi circle   
    }#end for loop
 }else{
      
   ##weightes mean line (central value)
   lines(c(0,r.0),c(0,0),lty=2, col="black", lwd=1.3)
   
 }#endif::for groups or no groups
  
##-------------------------------------------------------------------------------------------------##
##SEMI CIRCLE  
  
    ##LINES
		##semi circle with thickmarks and labels
		lines(circ.x,circ.y,lwd=1.3)
		segments(xmk1,ymk1,xmk2,ymk2)
    
    ##TEXT - labels z-scale
    text(xmk3,ymk3,round(mkest,digits=2),cex=1.0*cex.global)
	 
    ##TEXT
		##label z-axis
    text(max(x.i)*1.15,0,zlab,cex=0.9, srt=90)
	
##-------------------------------------------------------------------------------------------------##
    
##LEGEND
  
    ##plot legend
    if(missing(sample.legend)==FALSE){
      legend("topleft",sample.legend,lty=sample.lty,pch=sample.pch,col=sample.col, lwd=1.5*cex.global,bty="n")      
    }

##=================================================================================================##
##PLOT II: VALUES WITHOUT z-scale
##=================================================================================================##  			
  
##PAR
  ##set values for plot area
  par(
    mfrow=c(1,1),
    oma=c(2,0,0,0.5),
    mar=c(5,4,4,4.5),
    xpd=TRUE, #clipping of figure area
    las=1,
    cex=1*cex.global,
    new=TRUE
    )
  
##PLOT

  ##open Plot area
  plot(NA,NA,
       xlim=c(0,max(x.i)), 
       ylim=if(missing("yaxis.scale")==FALSE){yaxis.scale}else{c(-yaxis.max,yaxis.max)}, #note: y.i is a negative value
       yaxt="n",
       xaxs="i", #intersection x axis and yaxis is 0 (without alignment)
       yaxs="i",
       xaxt="n",
       ylab="standardised estimate",
       xlab="",
       typ="n",
       bty="n",	# no frame
       )
  
##POINTS
  
  ##enter data points (separat for loop to avoid overplotting)
  for (i in 1:length(sample.groups)){   
    ##plot data points
    points(x.i[unlist(sample.groups[i])],y.i[unlist(sample.groups[i])],pch=sample.pch[i], col=sample.col[i], cex=1.4)
  }
  
##AXES
  
  ##seperate x-axis  
  ##1)    
  ##lower axis is precision which means 1/se	
  axis(side=1,line=4,cex.axis=1*cex.global)
  mtext(side=1,line=6,"precision",cex=1.0*cex.global) 
  
  ##2)
  ##upper axis is relative standard error which means 1/x.i
  if(zscale.log==TRUE){
    reticks.labels<-round(1/axTicks(side=1)*100,digits=1)
  }else{
    reticks.labels<-round(1/axTicks(side=1),digits=1)  
  }

  reticks.values<-axTicks(side=1)
  
  ##print se axis and exclude the first value (it may Inf)
  axis(side=1,at=reticks.values[-1],labels=reticks.labels[-1],line=4,cex.axis=1,tck=0.02,padj=-4)
  mtext(side=1,line=1.5,if(zscale.log==TRUE){"relative error [%]"}else{"absolute error"},cex=1.0) 
  
  ##y-axis
  axis(side=2,at=c(-2,-1,0,1,2),labels=c("-2",""," 0",""," 2"),cex.axis=1)  
  
##TEXT
  ##HIGHEST POINT
  ##mark highest point, also for the log De option (use max to ensure that really only the highest point has been chosen)
  if(zscale.log==TRUE){
    text(1/(max(sample[sample[,1]==max(sample[,1]),2])/max(sample[,1])),
         (max(log(sample[,1]))-z.0)/max((sample[sample[,1]==max(sample[,1]),2])/max(sample[,1])),
         round(max(exp(z.i)),digits=1),
         pos=3,
         cex=0.6*cex.global
         
         )#end text
  }else{
    text(1/(max(sample[sample[,1]==max(sample[,1]),2])),
         (max(sample[,1])-z.0)/(max(sample[sample[,1]==max(sample[,1]),2])),
         round(max(z.i),digits=1),
         pos=3,
         cex=0.6*cex.global
         )#end text 
  }
  
##MTEXT
  if(sample.groups.trigger==FALSE){
  #plot central value and mark values inside 2-sigma
  mtext(side=3,paste("n = ",length(z.i),
                     " | ","central value = ",if(zscale.log==TRUE){round(exp(z.0),digits=1)}else{round(z.0,digits=1)},
                     " | ","within 2-sigma = ",round(length(y.i[y.i>=-2 & y.i<=2])/length(y.i)*100,digits=1),
                     "%",sep=""),cex=0.8, line=0.5)
  }else{
    
    ##plot mtext for sample
    mtext(side=3,sample.mtext,cex=1*cex.global)

  }
##=================================================================================================##  
} #End of function

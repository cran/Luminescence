##//////////////////////////////////////////////
##//Calc_CentralDose.R 
##/////////////////////////////////////////////
##
##======================================
#author: Christoph Burow 
#organisation: University of Cologne
#vers.: 0.1
#date: 12/10/2012
#nota bene: Based on a rewritten S script of Rex Galbraith, 2010.
##          The original script annotation by Rex Galbraith is given
##          below, only slightly changed to match the scripts current
##          shape.
##======================================

# Program to calculate central dose and dispersion 
# and their standard errors and the profile log likelihood
# function for sigma
#
# The model and notation are the the same as in the central age
# model of Galbraith et al (1999) Archaeometry.41, 339--364.
# The parameter delta is the natural log of the central dose, and
# its standard error correspondes to the relative standard error
# of the central dose. The parameter sigma is the standard deviation
# -- see the above reference for interpretation.
#	
# The data are assumed to be in a data.frame  with two columns
# headed ED and ED_Error.  The data.frame may contain other columns also.
# Of course the program can be edited to read data in other ways.	 
#	
# The program may be edited to include a known value of sigma_b
# before fitting the central age model. Then the dispersion
# will be that over and above sigma_b (and sigma_wi).	
#
# Programmed by Rex Galbraith, last modified 3 September 2010

##=============================================================================================##
## start function

Calc_CentralDose<- function(input.data,
                            sigmab=0, #sigma default 0
                            sample.id="unknown sample", #sample name
                            print.iterations=FALSE, #printing of calculation iterations
                            output.plot=TRUE #plotting of profile log likelihood for sigma
                            ) {                     
                              

##=============================================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##=============================================================================================##
  
  if(is.data.frame(input.data)==FALSE) { print("Input data needs to be of type data.frame",quote=F) 
                              stop(domain=NA) }
  try(colnames(input.data)<- c("ED","ED_Error"),silent=TRUE)
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error")
                                             { print("Columns must be named 'ED' and 'ED_Error'",quote=F)
                              stop(domain=NA)}
  if(sigmab <0 | sigmab >1) { print("sigmab needs to be given as a fraction between 0 and 1 (e.g. 0.2)",quote=F)
                              stop(domain=NA)}
  
##=============================================================================================##
## CALCULATIONS
##=============================================================================================##
		          
# calculate  yu = log(ED) and su = se(logED)
	yu<- log(input.data$ED)
	su<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )

# calculate starting values and weights 
  sigma<- 0.15
	wu<- 1/(sigma^2 + su^2)
	delta<- sum(wu*yu)/sum(wu)
	n<- length(yu)

# compute mle's
  for(j in 1:200){
    
  	delta<- sum(wu*yu)/sum(wu)
  	sigma<- sigma*sqrt(sum( (wu^2)*(yu-delta)^2/sum(wu) ))
  	wu<- 1/(sigma^2 + su^2)
  
# print iterations
      if(print.iterations==TRUE) {
        
      	print(round(c(delta, sigma),4))
      	
      }
  }

# save parameters for terminal output
out.delta<- exp(delta)
out.sigma<- sigma
	
# log likelihood	
	llik<-  0.5*sum(log(wu)) - 0.5*sum(wu*(yu-delta)^2)
    # save parameter for terminal output
    out.llik<- round(llik,4)
  Lmax<- llik

# standard errors
	sedelta<- 1/sqrt(sum(wu))
	sesigma<- 1/sqrt(2*sigma*sum(wu^2))

# save parameters for terminal output	
  out.sedelta<- sedelta
  out.sesigma<- sesigma

# profile log likelihood
  sigmax<- sigma
	llik<- 0
	sig0<- max(0,sigmax-8*sesigma)
	sig1<- sigmax + 9.5*sesigma
	sig<- seq(sig0,sig1,0.0001)
  
	for(sigma in sig) {
    
   	wu<- 1/(sigma^2 + su^2)
  	mu<- sum(wu*yu)/sum(wu)
  	ll<-  0.5*sum(log(wu)) - 0.5*sum(wu*(yu-mu)^2)
  	llik<- c(llik,ll)

  }
  
	llik<- llik[-1] - Lmax

##=============================================================================================##  
##TERMINAL OUTPUT
##=============================================================================================##  
  
  cat("\n [Calc_CentralDose]")
  cat(paste("\n\n ---------------------------------"))
  cat(paste("\n sample ID:              ",sample.id))
  cat(paste("\n n:                      ",n))
  cat(paste("\n log ED:                 ","TRUE"))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n central dose (delta):   ",round(out.delta,4)))
  cat(paste("\n rse (delta):            ",round(out.sedelta,4)))
  cat(paste("\n se (delta):             ",round(out.delta*out.sedelta,4)))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n overdispersion (sigma): ",round(out.sigma,4)))
  cat(paste("\n se (sigma):             ",round(out.sesigma,4)))
  cat(paste("\n ---------------------------------\n\n"))  

  results<- data.frame(id=sample.id,n=n,log_ED="TRUE",central_dose=out.delta,rse_delta=out.sedelta,
                       se_delta=out.delta*out.sedelta,OD=out.sigma,se_sigma=out.sesigma)
  
##=============================================================================================##  
##PLOTTING
##=============================================================================================##
  
if(output.plot==TRUE) {
  
# plot the profile log likeihood
	par(oma=c(2,1,2,1),las=1,cex.axis=1.2, cex.lab=1.2)
	plot(sig,llik,type="l",xlab="sigma",ylab="Log likelihood",lwd=1.5)
	abline(h=0,lty=3)
	abline(h=-1.92,lty=3)
	title("Profile log likelihood for sigma")
	
# find upper and lower confidence limits for sigma
	tf<- abs(llik+1.92) < 0.005
	sig95<- sig[tf]
	ntf<- length(sig95)
	sigL<- sig95[1]
	sigU<- sig95[ntf]

# put them on the graph
	abline(v=sigL)	
	abline(v=sigmax)	
	abline(v=sigU)
	dx<- 0.006
	dy<- 0.2
	ytext<- min(llik) + dy
	res<- c(sigL,sigmax,sigU)
	text(res+dx,rep(ytext,3),round(res,2),adj=0)
}#endif::output.plot
  
#return value
  invisible(list(results=results))  

}#EndOf function
#EOF

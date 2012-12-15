##//////////////////////////////////////////////
##//Calc_MinDose4.R 
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

# R program to fit the four parameter minimum dose model to
#	 OSL palaeodose data
#
# The data are assumed to be in an ascii file, to be read as a
# data frame with column headings and with at least two columns:
#   including one headed  ED,  the calculated dose (Gy) for each grain
#   and one headed  ER_Error, the corresponding standard error
#	
# You will be prompted for the name of the data file and for a
# value of sigma_b. If this is already included in the value
# of se then put sigma_b = 0 at the prompt		 	
#	
# The program fits a truncated-normal distribution to the log dose
# estimates with an atom of probability at the truncation point
#		
# The model has four parameters:
#
#          gamma: this is the minimum dose on the log scale
#	          --- so exp(gamma) is the minimum dose
#          mu:	  this is the mean of the non-truncated normal distribution
#            	  --- if the distribution were not truncated, exp(mu)
#	          would be the central dose	
#          p0:    this is the proportion of grains at gamma
#                 --- if there are many grains at the minimum, 
#                     p0 should be substantially > 0
#          sigma: this captures the spread in ages above the 
#                 minimum. If the distribution of log doses
#                 were truncated, sigma would be the standard deviation,
#	          and would correspond to the dispersion parameter in
#	          the central age model.
#
# Further details of this model are given in my book Statistics for
# Fission Track Analysis, page 107-108, CRC Press, 2005,  and also
# in Galbraith et al 1999, Archaeometry, 41, 339-364.
#	
#   Estimation is by maximum likelihood	using the function nlminb.
#   This requires the user to specify starting values of the parameters,
#   given by gamma.init, mu.init, sigma.init and p0.init. The values
#   given below are suitable for the data sim.dat, but may need to be
#   changed for other data.
#	
#   Also upper and lower bounds for each parameter (gamma, mu, sigma, p0)
#   need to be specified in the xlb and xub below.  Again, depending on
#   the data, it may be necessary to change these, especially the bounds
#   for gamma and mu.  When the program has finished, you should check
#   that the final estimates of gamma and mu are within the bounds --
#   i.e., not on the boundary.					  	
#
#   Numerical results are printed to the screen and are also stored in a
#   file called filnam-4R.res, where filnam is the name of the input file.
#
#   You will be promted to see if you want to calculate profile log
#   likelihood functions, and if so, which ones.  The indices of the
#   paramters are: 1 gamma, 2 mu, 3 sigma and 4 p0.  For example, if you
#   want all four profiles type 4 at the prompt, if you want just gamma
#   and mu, type 2, and if you want just gamma, type 1.  If the profiles 
#   are calculated, they will be plotted in a postscript file called 
#   filnam-4R.ps
#
#   Rex Galbraith, September, 2010
			
            #------------------------------------------------#

######################################################################
# Functions lnnprob.f and neglik.f	

##=============================================================================================##
## start function

Calc_MinDose4<- function(input.data, #data.frame containing two columns named "ED" and "ED_Error"
                         sigmab, #spread in ages above the minimum
                         log=TRUE, #calculate mindose with (un-)logged De values
                         sample.id="unknown sample", #sample ID
                         gamma.xlb=0.1, #lower boundary for gamma
                         gamma.xub=100, #upper boundary for gamma
                         mu.xlb=1, #lower boundary for mu
                         mu.xub=100, #upper boundary for mu
                         sigma.xlb= 0.001, #lower boundary sigma
                         sigma.xub= 5.00, #upper boundary for sigma
                         init.gamma=5, #starting value for gamma
                         init.mu=10, #starting value for mu
                         init.sigma=0.6, #starting value for sigma
                         init.p0=0.01, #starting value for p0
                         calc.ProfileLikelihoods=TRUE, #calculate the profile likelihoods
                         console.ProfileLikelihoods=FALSE, #print terminal output of profile log likelihoods
                         console.extendedOutput=FALSE, #if TRUE calculations will be printed
                         output.file=FALSE, #write results in filename.res file
                         output.filename="default", #set the desired filename, else the output file will be name "default"
                         output.plot=FALSE, #plotting of profile log likelihoods for sigma
                         output.indices=4 #calculate the profile log likelihood functions for gamma, sigma, mu, p0
                         ) {                     
  
  
##=============================================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##=============================================================================================##
  
  if(is.data.frame(input.data)==FALSE) { print("Input data needs to be of type data.frame",quote=F) 
                                         stop(domain=NA) } 
  try(colnames(input.data)<- c("ED","ED_Error"),silent=TRUE)
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error") { print("Columns must be named 'ED' and 'ED_Error'",quote=F)
                                         stop(domain=NA)}
  if(sigmab <0 | sigmab >1) { print("sigmab needs to be given as a fraction between 0 and 1 (e.g. 0.2)",quote=F)
                                         stop(domain=NA)}
  if(output.indices >4 | output.indices <1) { print("Invalid number of indices. Only 1, 2, 3 or 4 (gamma, gamma/mu, gamma/mu/sigma, gamma/mu/sigma/p0) allowed.",quote=F)
                                              stop(domain=NA)}
  
##=============================================================================================##
## CALCULATIONS
##=============================================================================================##
  
# this calculates ln(1-Phi(x)) robustly, where 
# Phi(x) is the standard normal c.d.f  
  
lnnprob.f<- function(x)
{ 
  logsqrt2pi<- 0.5*log(2*pi)

  b1  <-  3.8052e-8
  b2  <-  1.00000615302
  b3  <-  3.98064794e-4
  b4  <-  1.98615381364
  b5  <-  0.151679116635
  b6  <-  5.29330324926 
  b7  <-  4.8385912808
  b8  <- 15.1508972451
  b9  <-  0.742380924027
  b10 <- 30.789933034
  b11 <-  3.99019417010 
  
  lnnp<- x
  tf<-  (x<1.28)
  if(sum(tf)>0) 
     {z<- x[tf]
      lnnp[tf]<- log(1-pnorm(z))
     }
  tf<- (!tf)
  if(sum(tf)>0)
     {z<- x[tf]
      fz <- 1/(z+b3+b4/(z-b5+b6/(z+b7-b8/(z+b9+b10/(z+b11)))))
      hz <- 1.0/(1.0-b1/z+b2*fz/z)
      lnnp[tf] <-  log(hz)-log(z)-0.5*z*z-logsqrt2pi
     }
  return(lnnp)
}

# neglik.f calculates minus the log-likelihood of the data from 
# the parameters and the data

neglik.f<- function(param,dat,rep){

# this calculates the negative of the log likelihood of the  
# data (dat,rep) for a given set of parameters (param)
# param is the vector of model parameters: 
#        (gamma, mu, sigma, p0)
# datmat is a nx2 matrix of data: ld, seld (including sigma_b)
# recover the data
	zi<- dat[,1]
	si<- dat[,2]
	n<- length(zi)
	
# recover the parameters
	gamma<- param[1]
	mu<- param[2]
	sigma<-  param[3]
	p0<-  param[4]

# calculate sigma^2 + seld^2, mu0 and sigma0
	s2<- sigma^2 + si^2
	sigma0<- 1/sqrt(1/sigma^2 + 1/si^2)
	mu0<- (mu/sigma^2 + zi/si^2)/(1/sigma^2 + 1/si^2) 

# calculate the log-likelihood
	logsqrt2pi<- 0.5*log(2*pi)
	res0<- (gamma - mu0)/sigma0
	res1<- (gamma - mu)/sigma
	lf1i<- log(p0) - log(si) - 0.5*((zi-gamma)/si)^2   - logsqrt2pi
	lf2i<- log(1-p0) - 0.5*log(s2) - 0.5*(zi-mu)^2/s2  - logsqrt2pi
	lf2i<- lf2i + lnnprob.f(res0) - lnnprob.f(res1)
	llik<- log( exp(lf1i) + exp(lf2i) )
	negll<- -sum(llik)

# print the results to screen
	get("prstat",mode="numeric")
	ncalls<- prstat[1]
	ncmod<-  prstat[2]
	ipr<-    prstat[3]
	ncalls<- ncalls+1
	ncmod<- ncmod+1
	if(console.extendedOutput==TRUE) {
	if( ncmod==ipr | (ncalls==1 & ipr>0) )
	   {
    	  cat(paste("\n\n\n ------ # function calls:", ncalls,"------"))
    	  cat(paste("\n neg. log likelihood: ",round(negll,3)))
    	  cat(paste("\n parameters"))
      if(log==TRUE) {
        cat(paste("\n  gamma: ",round(gamma,3),"   mindose:",round(exp(gamma),3)))
        cat(paste("\n     mu: ",round(mu,3),"  centdose:",round(exp(mu),3)))
      } else {
        cat(paste("\n  gamma: ",round(gamma,3),"   mindose:",round(gamma,3)))
        cat(paste("\n     mu: ",round(mu,3),"  centdose:",round(mu,3)))
      }
    	  cat(paste("\n  sigma: ",round(sigma,4)))
    	  cat(paste("\n     p0: ",round(p0,3)))
    	  cat(paste("\n      n: ",length(zi)))
	      ncmod<- 0
	   }
	}
	assign("prstat",c(ncalls,ncmod,ipr))

  return(negll)
}
	
##=============================================================================================##
## MAIN PROGRAM
##=============================================================================================##
  

# read in the data and print the number of grains
  if(log==TRUE) {
    lcd<- log(input.data$ED)
    lse<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )
  }
  else {
    lcd<- input.data$ED
    lse<- sqrt( (input.data$ED_Error)^2 + sigmab^2 )
  }
  datmat<- cbind(lcd,lse)
  
  cat("\n [Calc_MinDose4] \n")
  
# supply the starting values and bounds for the parameters
	if(log==TRUE) {
    gamma.init<- log(init.gamma)
  	mu.init<- log(init.mu)
    }
  else {
    gamma.init<- init.gamma
    mu.init<- init.mu
    }
	sigma.init<- init.sigma
	p0.init<- init.p0

	x0<- c(gamma.init, mu.init, sigma.init, p0.init)
  if(log==TRUE) {
  	xlb<- c( log(gamma.xlb), log(mu.xlb), sigma.xlb, 0.0001 )
  	xub<- c( log(gamma.xub), log(mu.xub), sigma.xub, 0.9999 )
    }
  else {
    xlb<- c( gamma.xlb, mu.xlb, sigma.xlb, 0.0001 )
    xub<- c( gamma.xub, mu.xub, sigma.xub, 0.9999 )
  }
	if(xub[1]>=xlb[2])
    { 
      cat("\n ---------------------------------------------------------------")
      cat("\n # Warning: upper bound for gamma must be < lower bound for mu #")
      cat("\n ---------------------------------------------------------------")
      stop
    }

# parameters to control printing from nlminb
	ncalls<- 0
	ncmod<- 0
	ipr<- 50
	prstat<- c(ncalls,ncmod,ipr)
	prfile<- F
	Bmess<- 0


# maximise the likelihood
	opt.param<- nlminb(start=x0,objective=neglik.f,scale=1,
	                   lower=xlb,upper=xub,dat=datmat,control=list(iter.max = 1000, eval.max = 1000))

# print out the maxmimum likelihood estimates
	
	mlest<- opt.param$par
	gamma<- mlest[1]
	mu<- mlest[2]
	sigma<-  mlest[3]
	p0<-  mlest[4]

	maxlik<- -opt.param$objective
	bic<- -2*maxlik + 4*log(length(lcd))
	get("prstat",mode="numeric")
	ncalls<- prstat[1]
	get("Bmess")

# write the parameter estimates to output file filnam-4R.res 
# where filnam is the input file name

  filnam<- output.filename
  
 if(output.file==TRUE)
  {
  
	lbout<- paste(filnam,if(log==TRUE){"-4R.res"}else{"-4R-UL.res"},sep="")
  
	write(c("Sample: ", filnam, "\nSigma_b: ", sigmab), file=lbout, ncolumns=2,append=F)
	write(" ",file=lbout,append=T)
  write(paste("Final estimate of model parameters"),lbout,append=T)
  write(paste("gamma:", round(gamma,3), "sigma:", round(sigma,3), "p0:", round(p0,3),"mu: ",round(mu,3)),lbout,append=T)
  write(" ",file=lbout,append=T)
  write("\nmaximum likelihood estimate of minimum age",lbout, append=T)
  write(" ",file=lbout,append=T)
  if(log==TRUE) {
    write(paste(" mindose  [exp(gamma)]: ",round(exp(gamma),3)),lbout,append=T)
    write(paste(" centdose    [exp(mu)]: ",round(exp(mu),3)),lbout,append=T)
  }
  else {
    write(paste(" mindose       [gamma]: ",round(gamma,3)),lbout,append=T)
    write(paste(" centdose         [mu]: ",round(mu,3)),lbout,append=T)
  }
  write(" ",file=lbout,append=T)
  write(c(" number of times mu<=gamma:",Bmess),file=lbout,ncolumns=2,append=T)
	write(" ",file=lbout,append=T)

# write fitted values to output file filnam-4R.res 
# where filnam is the input file name
	prfile<- T
	neglik.f(mlest,datmat)
	prfile<- F

 }
  
##=============================================================================================##
## PROFILE LOG LIKELIHOODS
##=============================================================================================##

  if(calc.ProfileLikelihoods==TRUE)
    {

# read in the indices of the parameters for which profile
# log likelihoods are to be calculated
    
  profind<- as.integer(output.indices)

# open a postscript file called filnam-4R.ps to plot the profiles
# and set graphical parameters
  if(output.plot==TRUE) {
	postscript(file=paste(filnam,if(log==TRUE){"-4R.ps"}else{"-4R-UL.ps"},sep=""),horizontal=F)
	par(mfrow=c(2,2),oma=c(9,2,9,1),mar=c(3.7,3.1,1.1,0.2),
	       mgp=c(1.75,0.5,0),las=1,cex.axis=1.1,cex.lab=1.3)
  }

# calculate the required profiles
	lbpar<- c("gamma","mu","sigma","p0")
	npar<- length(x0)
	for(j in 1:profind)
   {

##=============================================================================================##    
## first pass at the profile likelihood
    
# this does a broad sweep of the possible parameter values and
# estimates where more intensive calculations should be made
	lowpar<- xlb[j]+(mlest[j]-xlb[j])*c(0.01,0.5,0.8,0.9,0.95,0.99)
	highpar<- mlest[j]+(xub[j]-mlest[j])*c(0.01,0.05,0.1,0.2,0.5,0.99)
	prfpar<- c(lowpar,mlest[j],highpar)
	plk<- numeric(0)

	if(console.ProfileLikelihoods==TRUE){
	  cat("\n\n ------calculating profiles: broad sweep-------")
	  cat(paste("\n trial parameter value","|","profile log likelihood \n"))
	}
  
	for(par in prfpar )
   { 

# set up the initial values and the bounds so that the 
# profile parameter is fixed i.e. lower bd = upper bd
	if(j==1) {
	   px0<- c(par,mlest[2:npar])
           pxlb<- c(par,xlb[2:npar])
           pxub<- c(par,xub[2:npar])
        }
        if(j>1 & j<npar) {
           j1<- j-1
           j2<- j+1
           px0<- c(mlest[1:j1],par,mlest[j2:npar])
           pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
           pxub<- c(xub[1:j1],par,xub[j2:npar])
        }
        if(j==npar) {
           npar1<- npar-1
           px0<- c(mlest[1:npar1],par)
           pxlb<- c(xlb[1:npar1],par)
           pxub<- c(xub[1:npar1],par)
        }
  
# parameters to control printing from nlminb
	ncalls<-0
	ncmod<- 0
	ipr<- 0
	prstat<- c(ncalls,ncmod,ipr)


# maximise the likelihood
        opt.param<- nlminb(start=px0,objective=neglik.f,scale=1,
	                   lower=pxlb,upper=pxub,dat=datmat,control=list(iter.max = 1000, eval.max = 1000))

# save and print the results
	proflik<- -maxlik-opt.param$objective
	plk<- c(plk,proflik)
  
	if(console.ProfileLikelihoods==TRUE){
	  cat(c("",format(round(j,0)),"   ",format(round(c(par,proflik),3),nsmall=3),"\n"))
	}
}#END OF LOOP

##=============================================================================================##
## second pass
  
# this does a fine sweep of the parameter values between
# limits derived from the broad sweep
	n<- length(prfpar)
	cnt<- (1:n)
	tf<- plk > -2.0
	n1<- min(cnt[tf])
	n2<- max(cnt[tf])
	if(n1>1) tf[n1-1]<- T
	if(n2<n) tf[n2+1]<- T
	minpar<- xlb[j]
	if(n1>1) minpar<- min(prfpar[tf])
	maxpar<- xub[j]
	if(n2<n) maxpar<- max(prfpar[tf])
	prfpar2<- seq(minpar,maxpar,length=20)
	plk2<- numeric(0)


	if(console.ProfileLikelihoods==TRUE){
	  cat("\n ------calculating profiles: fine sweep-------")
	  cat(paste("\n trial parameter value","|","profile log likelihood \n"))
	}
        for(par in prfpar2 )
   { 

# set up the initial values and the bounds so that the 
# profile parameter is fixed i.e. lower bd = upper bd
	if(j==1) {
           px0<- c(par,mlest[2:npar])
           pxlb<- c(par,xlb[2:npar])
           pxub<- c(par,xub[2:npar])
        }
        if(j>1 & j<npar) {
           j1<- j-1
           j2<- j+1
           px0<- c(mlest[1:j1],par,mlest[j2:npar])
           pxlb<- c(xlb[1:j1],par,xlb[j2:npar])
           pxub<- c(xub[1:j1],par,xub[j2:npar])
        }
        if(j==npar) {
           npar1<- npar-1
           px0<- c(mlest[1:npar1],par)
           pxlb<- c(xlb[1:npar1],par)
           pxub<- c(xub[1:npar1],par)
        }

# parameters to control printing from nlminb
	ncalls<-0
	ncmod<- 0
	ipr<- 0
	prstat<- c(ncalls,ncmod,ipr)

# maximise the likelihood
  opt.param<- nlminb(start=px0,objective=neglik.f,scale=1,lower=pxlb,upper=pxub,
	                  dat=datmat,control=list(iter.max = 1000, eval.max = 1000))

# save and print the results
	proflik<- -maxlik-opt.param$objective
	plk2<- c(plk2,proflik)
	
  if(console.ProfileLikelihoods==TRUE){
	  cat(c("",format(round(j,0)),"   ",format(round(c(par,proflik),3),nsmall=3),"\n"))
	}
}#END OF LOOP

##=============================================================================================##
## POOLING & PLOTTING
##=============================================================================================##  
  
# the results from the broad sweep and the fine sweep
# are pooled, and only the data values near the  maximum likelihood
# solution are kept and the results are plotted in filnam-4R.ps
	prfpar<- c(prfpar,prfpar2)
	plk<- c(plk,plk2)
	ord<- order(prfpar)
	prfpar<- prfpar[ord]
	plk<- plk[ord]
	n<- length(prfpar)
	cnt<- (1:n)
	tf<- plk>=-1.92
	n1<- min(cnt[tf])
	n2<- max(cnt[tf])
	if(n1>1) tf[n1-1]<- T
	if(n2<n) tf[n2+1]<- T
	prfpar<- prfpar[tf]
	plk<- plk[tf]
	
# plotting
	if(output.plot==TRUE) {
    
	plot(prfpar,plk,type="n",xlab=lbpar[j],ylab="",ylim=c(-2.5,0),cex=0.8)
	lines(prfpar,plk)
	abline(h=-1.92,lty=3)
	abline(h=-0.5,lty=3)
	}

# the 95% upper and lower confidence limits are calculated from the
# profile results and the results are added to the plot
	n<- length(prfpar)
	cnt<- (1:n)
	tf<- plk>=-1.92
	n1<- min(cnt[tf])
	n2<- max(cnt[tf])
	delu<- (maxpar-minpar)/100
  
# the lower confidence limit	
    if(n1>1){ 
          uci<-prfpar[(n1-1):n1]
          lci<- plk[(n1-1):n1]
          u1<- approx(lci,uci,-1.92)$y
          if(output.plot==TRUE) {
          text(u1+delu,-2.12,format(round(u1,2)),adj=0,cex=1)
          if(c(j==1 | j==2)&log==TRUE)
            {
              text(u1+delu,-2.32,format(round(exp(u1),2)),adj=0,cex=1)
            }
          }
    }

# the upper confidence limit
    if(n2<n){
          uci<-prfpar[n2:(n2+1)]
          lci<- plk[n2:(n2+1)]
          u2<- approx(lci,uci,-1.92)$y
          if(output.plot==TRUE) {
          text(u2-delu,-2.12,format(round(u2,2)),adj=1,cex=1)
          if(c(j==1 | j==2)&log==TRUE)
            {
              text(u2-delu,-2.32,format(round(exp(u2),2)),adj=1,cex=1)
            }
          }
    }
	if(j==1) {try(gul<-u1, silent=TRUE)
	          try(guu<-u2, silent=TRUE)}
	if(j==2) {try(mul<-u1, silent=TRUE)
	          try(muu<-u2, silent=TRUE)}
	if(output.file==TRUE&&c(j==1 | j==2)) {
	  if(log==TRUE) {
	    write(paste("\n95% confidence interval",if(j==1){" [exp(gamma)]"}else{" [exp(mu)]"}),lbout,append=T)
      try(write(c(exp(u1),exp(u2)),lbout,append=T),silent=TRUE)
  	  try(write(paste("-",round(exp(if(j==1){gamma}else{mu})-exp(u1),2),"+",round(exp(u2)-exp(if(j==1){gamma}else{mu}),2)),lbout,append=T),silent=TRUE) 
	  }
    else {
      write(paste("\n95% confidence interval",if(j==1){" [gamma]"}else{" [mu]"}),lbout,append=T)
      try(write(c(u1,u2),lbout,append=T),silent=TRUE)
      try(write(paste("-",round(if(j==1){gamma}else{mu}-u1,2),"+",round(u2-if(j==1){gamma}else{mu},2)),lbout,append=T),silent=TRUE)
    }
	}

# the 68% upper and lower confidence limits are calculated from the
# profile results and the results are added to the plot
	n<- length(prfpar)
	cnt<- (1:n)
	tf<- plk>=-0.5
	n1<- min(cnt[tf])
	n2<- max(cnt[tf])
	delu<- (maxpar-minpar)/100

# the lower confidence limit
    if(n1>1){
          uci<-prfpar[(n1-1):n1]
          lci<- plk[(n1-1):n1]
          u1<- approx(lci,uci,-0.5)$y
          if(output.plot==TRUE) {
          text(u1+delu,-0.7,format(round(u1,2)),adj=0,cex=1)
          if(c(j==1 | j==2)&log==TRUE)
            {
              text(u1+delu,-0.9,format(round(exp(u1),2)),adj=0,cex=1)
            }
          }
    }

# the upper confidence limit
    if(n2<n){
          uci<-prfpar[n2:(n2+1)]
          lci<- plk[n2:(n2+1)]
          u2<- approx(lci,uci,-0.5)$y
          if(output.plot==TRUE) {
          text(u2-delu,-0.7,format(round(u2,2)),adj=1,cex=1)
          if(c(j==1 | j==2)&log==TRUE)
            {
              text(u2-delu,-0.9,format(round(exp(u2),2)),adj=1,cex=1)
            }
          }
    }
  
	if(j==1) {try(gll<-u1, silent=TRUE)
	          try(glu<-u2, silent=TRUE)}
	if(j==2) {try(mll<-u1, silent=TRUE)
	          try(mlu<-u2, silent=TRUE)}
	if(output.file==TRUE&&c(j==1 | j==2)) {
    if(log==TRUE) {
  	  write(paste("\n68% confidence interval",if(j==1){" [exp(gamma)]"}else{" [exp(mu)]"}),lbout,append=T)
  	  try(write(c(exp(u1),exp(u2)),lbout,append=T), silent=TRUE)
  	  try(write(paste("-",round(exp(if(j==1){gamma}else{mu})-exp(u1),2),"+",round(exp(u2)-exp(if(j==1){gamma}else{mu}),2)),lbout,append=T), silent=TRUE)
    }
    else {
      write(paste("\n68% confidence interval",if(j==1){" [gamma]"}else{" [mu]"}),lbout,append=T)
      try(write(c(u1,u2),lbout,append=T), silent=TRUE)
      try(write(paste("-",round(if(j==1){gamma}else{mu}-u1,2),"+",round(u2-if(j==1){gamma}else{mu},2)),lbout,append=T), silent=TRUE)
    }
	}
  
# printing the maximum likelihood estimate on the graph
	if(output.plot==TRUE) {
  text(mlest[j],-0.17,format(round(mlest[j],2)),cex=1)
    if(c(j==1 | j==2)&log==TRUE)
         {
    	    text(mlest[j],-0.35,format(round(exp(mlest[j]),2)),cex=1)
         }
      }
	}
    
  if(output.plot==TRUE) {
  mtext(side=2,line=0,"relative profile log likelihood",cex=1.2,outer=T,las=0) 
  mtext(side=3,line=0,paste(filnam,if(log==TRUE){"   MAM 4"}else{"   MAM 4-UL"}),cex=1.4,outer=T)
  }

# Bmess is a message: it is the number of times 
# B1<=B2 
	get("Bmess")
  cat("\n\n -----------------------------")
  cat(paste("\n # number of times B1<=B2:",Bmess,"#"))
  cat("\n -----------------------------")
}#EndOf IF (PROFILE LOG LIKELIHOODS)

  if(output.file==TRUE) {
  write("",lbout,append=T)
  write(paste("Likelihood: ", round(maxlik, 5)),lbout, append=T)
  write(paste("       BIC: ", round(bic, 3)),lbout, append=T)
  }

# prepare return values
  results<- data.frame(id=sample.id,n=length(lcd),log=log,Lmax=maxlik,BIC=bic,
                       gamma=gamma, mu=mu, sigma=sigma, p0=p0,
                       mindose=if(log==TRUE){exp(gamma)}else{gamma},
                       centdose=if(log==TRUE){exp(mu)}else{mu},
                       "gamma_68ci_lower"=NA,"gamma_68ci_upper"=NA,
                       "gamma_95ci_lower"=NA,"gamma_95ci_upper"=NA,
                       "mu_68ci_lower"=NA,"mu_68ci_upper"=NA,
                       "mu_95ci_lower"=NA,"mu_95ci_upper"=NA)
  
  
  try(results$"gamma_68ci_lower"<- if(log==TRUE){exp(gll)}else{gll},silent=TRUE)
  try(results$"gamma_68ci_upper"<- if(log==TRUE){exp(glu)}else{glu},silent=TRUE)
  try(results$"gamma_95ci_lower"<- if(log==TRUE){exp(gul)}else{gul},silent=TRUE)
  try(results$"gamma_95ci_upper"<- if(log==TRUE){exp(guu)}else{guu},silent=TRUE)
  
  try(results$"mu_68ci_lower"<- if(log==TRUE){exp(mll)}else{mll},silent=TRUE)
  try(results$"mu_68ci_upper"<- if(log==TRUE){exp(mlu)}else{mlu},silent=TRUE)
  try(results$"mu_95ci_lower"<- if(log==TRUE){exp(mul)}else{mul},silent=TRUE)
  try(results$"mu_95ci_upper"<- if(log==TRUE){exp(muu)}else{muu},silent=TRUE)
  
# print out the maxmimum likelihood estimates  
  
  cat(paste("\n\n----------- meta data ------------"))                                 
  cat(paste("\n Sample ID:      ",sample.id))
  cat(paste("\n n:              ",length(lcd)))
  cat(paste("\n sigmab:         ",sigmab))
  cat(paste("\n log ED:         ",log))
  cat(paste("\n Lmax:           ",round(maxlik,3)))
  cat(paste("\n BIC:            ",round(bic,3)))
  
  cat(paste("\n\n--------- final parameter estimates ---------"))
  cat(paste("\n gamma:    ",round(gamma,4)),
      "\t\t minimum dose: ",if(log==TRUE){round(exp(gamma),3)}else{round(gamma,3)})
  cat(paste("\n mu:       ",round(mu,4)),
      "\t\t cent dose:    ",if(log==TRUE){round(exp(mu),3)}else{round(mu,3)})
  cat(paste("\n sigma:    ",round(sigma,4))) 
  cat(paste("\n p0:       ",round(p0,4)))
  
  if(log==TRUE) {
    cat(paste("\n\n------- confidence intervals for gamma -------"))
    try(cat(paste("\n  95% ci: ", round(exp(gul),3), "-", round(exp(guu),3), " (-",
                    round(exp(gamma)-exp(gul),2), " +",round(exp(guu)-exp(gamma),2),")")
              ,sep=""),silent=TRUE)
    try(cat(paste("\n  68% ci: ", round(exp(gll),3), "-", round(exp(glu),3), " (-",
                    round(exp(gamma)-exp(gll),2), " +",round(exp(glu)-exp(gamma),2),")")
              ,sep=""),silent=TRUE)
    
    if(any(is.na(results[12:15]))==TRUE){
      cat("\n # Couldn't calculate confidence intervals.")
    }
    
    cat(paste("\n\n------- confidence intervals for mu ----------"))
    try(cat(paste("\n  95% ci: ", round(exp(mul),3), "-", round(exp(muu),3), " (-",
                    round(exp(mu)-exp(mul),2), " +",round(exp(muu)-exp(mu),2),")")
              ,sep=""),silent=TRUE)
    try(cat(paste("\n  68% ci: ", round(exp(mll),3), "-", round(exp(mlu),3), " (-",
                    round(exp(mu)-exp(mll),2), " +",round(exp(mlu)-exp(mu),2),")")
              ,sep=""),silent=TRUE)
    
    if(any(is.na(results[16:19]))==TRUE){
      cat("\n # Couldn't calculate confidence intervals.")
    }
    
  }
  else {
    cat(paste("\n\n------- confidence intervals for gamma -------"))
    try(cat(paste("\n  95% ci: ", round(gul,3), "-", round(guu,3), " (-",
                    round(gamma-gul,2), " +",round(guu-gamma,2),")")
              ,sep=""),silent=TRUE)
    try(cat(paste("\n  68% ci: ", round(gll,3), "-", round(glu,3), " (-",
                    round(gamma-gll,2), " +",round(glu-gamma,2),")")
              ,sep=""),silent=TRUE)
    
    if(any(is.na(results[12:15]))==TRUE){
      cat("\n # Couldn't calculate confidence intervals.")
    }
    
    cat(paste("\n\n------- confidence intervals for mu ----------"))
    try(cat(paste("\n  95% ci: ", round(mul,3), "-", round(muu,3), " (-",
                    round(mu-mul,2), " +",round(muu-mu,2),")")
              ,sep=""),silent=TRUE)
    try(cat(paste("\n  68% ci: ", round(mll,3), "-", round(mlu,3), " (-",
                    round(mu-mll,2), " +",round(mlu-mu,2),")")
              ,sep=""),silent=TRUE)
    
    if(any(is.na(results[16:19]))==TRUE){
      cat("\n # Couldn't calculate confidence intervals.")
    }
  }

  
  cat(paste("\n----------------------------------------------"))

  
# close the postscript file
  if(output.plot==TRUE) {
    dev.off()
  }
  
  # Print out warnings with regard to parameter boundaries
  
  bcheck<- data.frame(gamma=c(all.equal(gamma.xub,if(log==TRUE){exp(gamma)}else{gamma})==TRUE,
                              all.equal(gamma.xlb,if(log==TRUE){exp(gamma)}else{gamma})==TRUE),
                      mu=c(all.equal(mu.xub,if(log==TRUE){exp(mu)}else{mu})==TRUE,
                              all.equal(mu.xlb,if(log==TRUE){exp(mu)}else{mu})==TRUE),
                      sigma=c(all.equal(sigma.xub,sigma)==TRUE,
                              all.equal(sigma.xlb,sigma)==TRUE))
  rownames(bcheck)<- c(".xub",".xlb")
  
  
  
  if(any(bcheck==TRUE)) {
    cat(paste("\n\n #---------------------------------#"))
    cat(paste("\n # Warning! One or more parameters #
 # are on the boundary. Check the  #
 # logical matrix below to see     #
 # which ones and where.           #"))
    cat(paste("\n #---------------------------------#\n\n"))
    print(bcheck)
  }
  
  
# return values
  invisible(list(results=results))
  
}#EndOf function
#EOF

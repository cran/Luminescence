##//////////////////////////////////////////////
##//Calc_FiniteMixture.R 
##/////////////////////////////////////////////
##
##======================================
#author: Christoph Burow 
#organisation: University of Cologne
#vers.: 0.1
#date: 12/10/2012
#nota bene: Based on a rewritten S script of Rex Galbraith, 2006.
##          The original script annotation by Rex Galbraith is given
##          below, only slightly changed to match the scripts current
##          shape.
##======================================

# This program fits a k component mixture to OSL palaeodoses with
# differing known standard errors. Parameters (doses and mixing proportions)
# are estimated by maximum likelihood assuming that the log dose estimates
# are from a mixture of normal distributions.	

# The data are assumed to be in a data.frame with two columns
# headed ED and ED_Error.  The file may contain other columns also.
# Of course the program can be edited to read data in other ways.	 

# The user will be prompted to specify
#	the name of the file containing the data
#       the number of components to be fitted, k (2 or greater)
#       the overdispersion parameter sigmab (zero or greater)
	
# a simple approach is to start with k=2 and increase k successively
# and use the maximum log likelihood and BIC values to choose the
# final k. BIC should decrease for an improved fit and then increase
# again when there are signs of over fitting
	
# Other signs of overfitting are:	
#        repeated dose estimates, 
#        covariance matrix not positive definite, and
#        convergence problems.
				
# The program uses the maximum likelihood formulae and algorithm in Appendix 1
# of Galbraith (1988), Technometrics, 30, 271-281, but expressed in terms
# of the actual observations rather than the standardised ones.
# See also Galbraith (2005) Statistics for Fission Track Analysis pp 88-90.	

# Programmed by Rex Galbraith.  Last modified on 20 Sept 2006. The changes
# from the 31/08/2005 version are to add in prompts for the data file,
# k and sigmab and to suppress printing after each iteration. I have also
# increased the default number of iterations to 200, which is plenty for
# most data sets.  This can be changed of course. And I have changed the
# file fmix.var.s to give nicer output.			

##=============================================================================================##
## start function

Calc_FiniteMixture<- function(input.data, #(REQUIRED) data.frame containing two columns named "ED" and "ED_Error"
                              sigmab, #(REQUIRED) spread in ages above the minimum
                              n.components, #(REQUIRED) amount of components to be fitted
                              sample.id="unknown sample", #sample ID
                              n.iterations = 200, #number of iterations for estimating the maximum likelihoods
                              grain.probability=FALSE, #prints (to 2 decimal places) the estimated probabilities of which component each grain is in
                              output.file=FALSE, #write results in filename.res file
                              output.filename="default" #set the desired filename, else the output file will be name "default"
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
  if(n.components<2) { print("Atleast two components need to be fitted",quote=F)
                                         stop(domain=NA)}
  if(n.iterations<1 | n.iterations>10000) { print("Only integers between 1:10000 allowed for n.iterations",quote=F)
                                         stop(domain=NA)}

##=============================================================================================##
## CALCULATIONS
##=============================================================================================##
  
  k<- n.components  

# calculate yu = log(ED),  su = se(logED),  n = number of grains	
	yu<- log(input.data$ED)
	su<- input.data$ED_Error/input.data$ED
	n<- length(yu)

# compute starting values
	fui<- matrix(0,n,k)
  pui<- matrix(0,n,k)
  nui<- matrix(0,n,k)
	pii<- rep(1/k,k)
	mu<- min(yu) + (max(yu)-min(yu))*(1:k)/(k+1)
		
# remove the # in the line below to get alternative starting values
# (useful to check that the algorithm converges to the same values)
#	mu<- quantile(yu,(1:k)/(k+1))
	
# compute mle's
	nit<- n.iterations
	wu<- 1/(sigmab^2 + su^2)
	rwu<- sqrt(wu)
	for(j in 1:nit){
	for(i in 1:k)
            {
             fui[,i]<-  rwu*exp(-0.5*wu*(yu-mu[i])^2)
             nui[,i]<-  pii[i]*fui[,i]
            }            
	pui<- nui/apply(nui,1,sum)
  mu<- apply(wu*yu*pui,2,sum)/apply(wu*pui,2,sum)
	pii<- apply(pui,2,mean)
	}
	
# calculate the log likelihood and BIC	
	llik<- sum( log( (1/sqrt(2*pi))*apply(nui,1,sum) ))
	bic<- -2*llik + (2*k - 1)*log(n)

# calculate the covariance matrix and standard errors of the estimates
# i.e., the dose estimtes in Gy and relative standard errors, and
# the mixing proportions and standard errors.		
# run this after fmix.s to get the var matrix of the estimates
  aui<- matrix(0,n,k)
  bui<- matrix(0,n,k)
  for(i in 1:k)
    {
     aui[,i]<- wu*(yu-mu[i])
     bui[,i]<- -wu + (wu*(yu-mu[i]))^2
    }
  delta<- diag(rep(1,k))
  
  Au<- matrix(0,k-1,k-1)
  Bu<- matrix(0,k-1,k)
  Cu<- matrix(0,k,k)
  
  for(i in 1:(k-1)){ for(j in 1:(k-1)){
  Au[i,j]<- sum( (pui[,i]/pii[i] - pui[,k]/pii[k])*(pui[,j]/pii[j] - pui[,k]/pii[k]) )}} 
  
  for(i in 1:(k-1)){ for(j in 1:k){
  Bu[i,j]<- sum( pui[,j]*aui[,j]*(pui[,i]/pii[i] - pui[,k]/pii[k] - delta[i,j]/pii[i] + delta[k,j]/pii[k] ) )}}
  
  for(i in 1:k){ for(j in 1:k){
  Cu[i,j]<- sum( pui[,i]*pui[,j]*aui[,i]*aui[,j] - delta[i,j]*bui[,i]*pui[,i] ) }}
  
  invvmat<- rbind(cbind(Au,Bu),cbind(t(Bu),Cu))
  vmat<- solve(invvmat)
  rek<- sqrt(sum(vmat[1:(k-1),1:(k-1)]))
  
  
  cat("\n [Calc_FiniteMixture]")
  cat(paste("\n\n--- covariance matrix of mle's ---\n\n"))
  print(round(vmat,6))
  cat(paste("\n----------- meta data ------------"))                                 
  cat(paste("\n Sample ID:            ",sample.id))
  cat(paste("\n n:                    ",n))
  cat(paste("\n sigmab:               ",sigmab))
  cat(paste("\n number of components: ",k))
  cat(paste("\n llik:                 ",round(llik,4)))
  cat(paste("\n BIC:                 	",round(bic,3)))
  cat(paste("\n\n----------- components -----------\n\n"))
                                                 
  dose<- exp(mu)
  re<- sqrt(diag(vmat))[-c(1:(k-1))]
  sed<- dose*re
  estd<- rbind(dose,re,sed)

  prop<- pii
  sep<-  c(sqrt(diag(vmat))[c(1:(k-1))],rek)
  estp<- rbind(prop,sep)

  blk<- rep("    ",k)
  comp<- rbind(blk,round(estd,4),blk,round(estp,4))
  comp<- data.frame(comp,row.names=c("","dose (Gy)    ","rse(dose)    ","se(dose)(Gy)"," ","proportion   ","se(prop)    "))

  cp<- rep("comp",k)
  cn<- c(1:k)
  names(comp)<- paste(cp,cn,sep="")
  print(comp)
  
#Write output file
  if(output.file==TRUE) {
  lbout<- paste(output.filename,"-FM"," k",k,"s",sigmab, ".res",sep="")

  write(c(paste("sample: ", output.filename, "\nsigma: ", sigmab)),lbout)
  
  options(warn=-1)
  write(c(paste("\nnumber of components:",k,"    Sigma:",sigmab,"    llik:",round(llik,4),"     BIC:  ",round(bic,3))),lbout,append=T)
  write("",lbout,append=T)      
  write.table(comp,lbout,append=T,quote=F,sep="\t\t",na="--NA--")
  options(warn=0)
  }

# print (to 2 decimal places) the 
# estimated probabilities of which component each grain is in
# -- sometimes useful for diagnostic purposes			
  if(grain.probability==TRUE) {	
    cat(paste("\n-------- grain probability -------\n\n"))
    print(round(pui,2))                            
  }

# calculate the log likelihood and BIC for a single component -- can
# be useful to see if there is evidence of more than one component
  mu0<- sum(wu*yu)/sum(wu)
	fu0<-  rwu*exp(-0.5*wu*(yu-mu0)^2)
  L0<- sum( log((1/sqrt(2*pi))*fu0 ) )
  bic0<- -2*L0 + log(n)
	comp0<- round(c(exp(mu0),sigmab,L0,bic0),4)
	
  cat(paste("\n-------- single component --------"))                   
  cat(paste("\n mu:                    ", comp0[1]))
  cat(paste("\n sigmab:                ", comp0[2]))
  cat(paste("\n llik:                  ", comp0[3]))
  cat(paste("\n BIC:                   ", comp0[4]))
  cat(paste("\n----------------------------------\n\n"))
  
  if(output.file==TRUE) {
  write(c("\nsingle component mu, sigmab, llik and BIC:  "),lbout,append=T)
  write(c(comp0),lbout,append=T)
  }
  
  # Prepare return values
  meta<- data.frame(id=sample.id,n=n,sigmab=sigmab,n.components=k,llik=llik,bic=bic)
  single.comp<- data.frame(id=sample.id,mu=comp0[1],sigmab=comp0[2],llik=comp0[3],BIC=comp0[4])
  comp.re<- rbind(round(estd,4),round(estp,4))
  comp.re<- data.frame(comp.re,row.names=c("dose (Gy)    ","rse(dose)    ","se(dose)(Gy)","proportion   ","se(prop)    "))
  names(comp.re)<- paste(cp,cn,sep="")
  
  # Return values
  invisible(list(mle.matrix=vmat,
                 grain.probability=round(pui,2),
                 meta=meta,
                 components=comp.re,
                 single.comp=single.comp))
}#EndOf function
#EOF

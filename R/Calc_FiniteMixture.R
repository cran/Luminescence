calc_FiniteMixture<- structure(function( # Apply the finite mixture model (FMM) after Galbraith (2005) to a given De distribution
  ### This function fits a k-component mixture to a De distribution with 
  ### differing known standard errors. Parameters (doses and mixing proportions)
  ### are estimated by maximum likelihood assuming that the log dose estimates 
  ### are from a mixture of normal distributions.
  
  # ===========================================================================
  ##author<< 
  ## Christoph Burow, University of Cologne (Germany) \cr
  ## Based on a rewritten S script of Rex Galbraith, 2006. \cr\cr
  
  ##section<<
  ## version 0.21 [2013-11-04] 
  # ===========================================================================
  
  input.data,
  ### \code{\linkS4class{RLum.Results}} or \link{data.frame} (\bold{required}):
  ### for \code{data.frame}: two columns with De \code{(input.data[,1])} and
  ### De error \code{(values[,2])}
  sigmab,
  ### \code{\link{numeric}}  (\bold{required}): spread in De values given as a
  ### fraction (e.g. 0.2). This value represents the expected overdispersion in
  ### the data should the sample be well-bleached (Cunningham & Walling 2012, 
  ### p. 100).
  n.components, 
  ### \code{\link{numeric}}  (\bold{required}): number of components to be 
  ### fitted
  sample.id="unknown sample",
  ### \code{\link{character}} (with default): sample id
  n.iterations = 200, 
  ### \code{\link{numeric}}  (with default): number of iterations for maximum 
  ### likelihood estimates
  grain.probability=FALSE, 
  ### \code{\link{logical}} (with default): prints the estimated probabilities
  ### of which component each grain is in
  output.file=FALSE, 
  ### \code{\link{logical}} (with default): save results to file. See 
  ### \code{output.filename}.
  output.filename="default"
  ### \code{\link{character}} (with default): desired filename, else results 
  ### are saved to default.res
  ){

##============================================================================##
## CONSISTENCY CHECK OF INPUT DATA
##============================================================================##

  if(missing(input.data)==FALSE){
    
    if(is(input.data, "data.frame") == FALSE & is(input.data,
                                                  "RLum.Results") == FALSE){
      
      stop("[calc_FiniteMixture] Error: 'input.data' object has to be of type 
           'data.frame' or 'RLum.Results'!")
      
    }else{
      
      if(is(input.data, "RLum.Results") == TRUE){
        
        input.data <- get_RLum.Results(input.data, 
                                       signature(object = "De.values"))
        
      }
    }
  }  
  
  try(colnames(input.data)<- c("ED","ED_Error"),silent=TRUE)
  
  if(colnames(input.data[1])!="ED"||colnames(input.data[2])!="ED_Error") { 
    cat(paste("Columns must be named 'ED' and 'ED_Error'"), fill = FALSE)
    stop(domain=NA) 
  }
  
  if(sigmab <0 | sigmab >1) { 
    cat(paste("sigmab needs to be given as a fraction between", 
              "0 and 1 (e.g. 0.2)"), fill = FALSE)
    stop(domain=NA)
  }
  
  if(n.components<2) { 
    cat(paste("Atleast two components need to be fitted"), fill = FALSE)
    stop(domain=NA)
  }
  
  if(n.iterations<1 | n.iterations>10000) { 
    cat(paste("Only integers between 1:10000 allowed for n.iterations"),
        fill = FALSE)
    stop(domain=NA)
  }

##============================================================================##
## CALCULATIONS
##============================================================================##
  
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
  Au[i,j]<- sum( (pui[,i]/pii[i] - pui[,k]/pii[k])*(pui[,j]/pii[j] - 
                                                      pui[,k]/pii[k]) )}} 
  
  for(i in 1:(k-1)){ for(j in 1:k){
  Bu[i,j]<- sum( pui[,j]*aui[,j]*(pui[,i]/pii[i] - pui[,k]/pii[k] - 
                                    delta[i,j]/pii[i] + delta[k,j]/pii[k] ) )}}
  
  for(i in 1:k){ for(j in 1:k){
  Cu[i,j]<- sum( pui[,i]*pui[,j]*aui[,i]*aui[,j] - delta[i,j]*bui[,i]*
                   pui[,i] ) }}
  
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
  
  # this calculates the proportional standard error of the proportion of grains
  # in the fitted components. However, the calculation is most likely erroneous.
  # sep<-  c(sqrt(diag(vmat))[c(1:(k-1))],rek)
  
  estp<- prop

  blk<- rep("    ",k)
  comp<- rbind(blk,round(estd,4),blk,round(estp,4))
  comp<- data.frame(comp,row.names=c("","dose (Gy)    ","rse(dose)    ",
                                     "se(dose)(Gy)"," ","proportion   "))

  cp<- rep("comp",k)
  cn<- c(1:k)
  names(comp)<- paste(cp,cn,sep="")
  print(comp)
  
#Write output file
  if(output.file==TRUE) {
  lbout<- paste(output.filename,"-FM"," k",k,"s",sigmab, ".res",sep="")

  write(c(paste("sample: ", output.filename, "\nsigma: ", sigmab)),lbout)
  
  options(warn=-1)
  write(c(paste("\nnumber of components:",k,"    Sigma:",sigmab,
                "    llik:",round(llik,4),"     BIC:  ",round(bic,3))),
        lbout,append=T)
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
  meta<- data.frame(id=sample.id,n=n,sigmab=sigmab,n.components=k,
                    llik=llik,bic=bic)
  single.comp<- data.frame(id=sample.id,mu=comp0[1],sigmab=comp0[2],
                           llik=comp0[3],BIC=comp0[4])
  comp.re<- rbind(round(estd,4),round(estp,4))
  comp.re<- data.frame(comp.re,row.names=c("dose (Gy)    ","rse(dose)    ",
                                           "se(dose)(Gy)","proportion   "))
  names(comp.re)<- paste(cp,cn,sep="")
  
  grain.probability<- round(pui, 2)
  
  newRLumResults.calc_FiniteMixture <- set_RLum.Results(
    data = list(
      mle.matrix=vmat,
      grain.probability=grain.probability,
      meta=meta,
      components=comp.re,
      single.comp=single.comp))
  
# Return values
  invisible(newRLumResults.calc_FiniteMixture)
  ### Returns a terminal output and a file containing statistical results if 
  ### wanted. In addition a list is returned containing the following elements:
  ### \item{mle.matrix}{\link{matrix} covariance matrix of maximum likelihood
  ### estimates.}
  ### \item{grain.probability}{\link{matrix} with estimated probabilities 
  ### of which component each grain is in.}
  ### \item{meta}{\link{data.frame} containing model parameters 
  ### (sample.id, sigmab, n.components, llik, bic).}
  ### \item{components}{\link{data.frame} containing fitted components.}
  ### \item{single.comp}{\link{data.frame} containing log likelihood and 
  ### BIC for a single component.}
  ###
  ### The output should be accessed using the function 
  ### \code{\link{get_RLum.Results}}
  
  
  ##details<<
  ## This model uses the maximum likelihood and Bayesian Information Criterion 
  ## (BIC) approaches. \cr\cr
  ## Indications of overfitting are: \cr\cr
  ## - increasing BIC \cr
  ## - repeated dose estimates \cr
  ## - covariance matrix not positive definite \cr
  ## - convergence problems
  
  ##references<<
  ## Galbraith, R.F. & Green, P.F., 1990. Estimating the component ages in a 
  ## finite mixture. Nuclear Tracks and Radiation Measurements, 17, pp. 197-206.
  ## \cr\cr
  ## Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
  ## track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470.\cr\cr
  ## Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
  ## dose and error calculation and display in OSL dating: An overview and some
  ## recommendations. Quaternary Geochronology, 11, pp. 1-27.\cr\cr
  ## Roberts, R.G., Galbraith, R.F., Yoshida, H., Laslett, G.M. & Olley, J.M., 
  ## 2000. Distinguishing dose populations in sediment mixtures: a test of 
  ## single-grain optical dating procedures using mixtures of laboratory-dosed
  ## quartz. Radiation Measurements, 32, pp. 459-465.\cr\cr
  ## Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
  ## Hall/CRC, Boca Raton.\cr\cr
  ## \bold{Further reading}\cr\cr
  ## Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
  ## equivalent dose (De) distributions: Implications for OSL dating of sediment
  ## mixtures. Quaternary Geochronology, 4, pp. 204-230.\cr\cr
  ## Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
  ## archives using robust OSL chronologies. Quaternary Geochronology, 12, 
  ## pp. 98-106.\cr\cr
  ## Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
  ## reproducibility and accuracy of optical dating of fluvial deposits. 
  ## Quaternary Geochronology, 1, pp. 109-120.\cr\cr
  ## Rodnight, H. 2008. How many equivalent dose values are needed to obtain a 
  ## reproducible distribution?. Ancient TL, 26, pp. 3-10.
  
  ##seealso<<
  ## \code{\link{calc_CentralDose}},
  ## \code{\link{calc_CommonDose}}, \code{\link{calc_FuchsLang2001}},
  ## \code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}    
}, ex=function(){
  ## load example data
  data(ExampleData.DeValues, envir = environment())
  
  ## apply the finite mixture model
  calc_FiniteMixture(ExampleData.DeValues,
                     sigmab = 0.08, n.components = 2,
                     grain.probability = TRUE, output.file = FALSE)
})#END OF STRUCTURE
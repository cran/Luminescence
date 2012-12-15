##//////////////////////////////////////////////
##//Calc_CommonDose.R 
##/////////////////////////////////////////////
##
##======================================
#author: Christoph Burow 
#organisation: University of Cologne
#vers.: 0.1
#date: 12/10/2012
##======================================

# Program to calculate the common dose of a De distribution

##=============================================================================================##
## start function

Calc_CommonDose<- function(input.data,
                            sigmab=0, #sigma default 0
                            log=TRUE, #calculate central dose with (un-)logged De values
                            sample.id="unknown sample" #sample name
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
	if(log==TRUE) {
    yu<- log(input.data$ED)
  	su<- sqrt( (input.data$ED_Error/input.data$ED)^2 + sigmab^2 )
  }
  else {
    yu<- input.data$ED
    su<- sqrt((input.data$ED_Error)^2 + sigmab^2)
  }

# calculate weights 
	wu<- 1/su^2
	delta<- sum(wu*yu)/sum(wu)
	n<- length(yu)
  
#standard error
  sedelta<- 1/sqrt(sum(wu))
  if(log==FALSE) {
    sedelta<- sedelta/delta
  }  
  
  cat("\n [Calc_CommonDose]")
  cat(paste("\n\n ---------------------------------"))
  cat(paste("\n sample ID:              ",sample.id))
  cat(paste("\n n:                      ",n))
  cat(paste("\n log ED:                 ",if(log==TRUE){"TRUE"}else{"FALSE"}))
  cat(paste("\n ---------------------------------"))
  cat(paste("\n common dose:            ",if(log==TRUE){round(exp(delta),4)}else{round(delta,4)}))
  cat(paste("\n rse:                    ",round(sedelta,4)))
  cat(paste("\n se:                     ",round(if(log==TRUE){exp(delta)*sedelta}else{delta*sedelta},4)))
  cat(paste("\n ---------------------------------\n\n"))  
  
#return value
  results<- data.frame(id=sample.id,n=n,log_ED=log,common_dose=if(log==TRUE){round(exp(delta),4)}else{round(delta,4)},
                       rse=round(sedelta,4),se=round(if(log==TRUE){exp(delta)*sedelta}else{delta*sedelta},4))
  
  invisible(list(results=results))
}#EndOf function
#EOF

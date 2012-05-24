##//////////////////////////////////////////////
##//Calc_FadingCorr.R
##/////////////////////////////////////////////

##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.1.1
#date: 15/04/2012
##======================================
##
##  --fading correction according Huntely and Lamothe 2001
##  --Error estimation by Monte Carlo simulation
##
##
Calc_FadingCorr<-function(g_value,    #g-value in $
                  tc,         #tc in seconds
                  age.faded)  #Age.faded in ka
{
  
##=================================================================================================##
##CALCULATION
##=================================================================================================##
  
  ##set values for iteration
  z<-seq(1,500,by=0.01) #limit for the age range
      
  ##calculate kappa
  kappa<-g_value[1]/log(10)/100
   
  ##transform tc in ka years
  tc<-tc/60/60/24/365/1000
  
  ##calculate all values z
  temp<-which(round(age.faded[1]/z,digits=2)==round(1-kappa*(log(z/tc)-1),digits=2))
 
  ##-----------------------------------------------------------------------------------------------##
  ##Monte Carlo simulation for error estimation
      
      g_valueMC<-rnorm(500,mean=g_value[1],sd=g_value[2])
      age.fadedMC<-rnorm(500,mean=age.faded[1],sd=age.faded[2])
      kappaMC<-g_valueMC/log(10)/100
  
      ##calculate all values for 1:500 in a step of 0.01
      tempMC<-sapply(1:500,function(x){
            which(round(age.fadedMC[x]/z,digits=2)==round(1-kappaMC[x]*(log(z/tc)-1),digits=2))})
  ##-----------------------------------------------------------------------------------------------##
  
  ##obtain corrected age
  age.corr<-data.frame(Age=median(z[temp]),Age.Error=round(sd(z[unlist(tempMC)]),digits=2))
  
##=================================================================================================##
##OUTPUT
##=================================================================================================##

  cat("\n[Calc_FadingCorr]")
  cat("\n\t Fading correction according to Huntley & Lamothe (2001):\n")
  cat(paste("\n\t Age (faded): ",age.faded[1]," +/- ",age.faded[2]," ka",sep=""))
  cat(paste("\n\t g-value: ",g_value[1], " +/- ",g_value[2]," %/decade",sep=""))
  cat(paste("\n\t tc: ",tc, " ka",sep=""))
  cat(paste("\n\t kappa: ",mean(kappa),sep=""))
  cat(paste("\n\t observations: ",length(unlist(tempMC))),sep="")
  cat("\n\n\t ----------------------------------")
  cat(paste("\n\t Age (corr.): ",age.corr[1]," +/- ",age.corr[2]," ka",sep=""))
  cat("\n\t ----------------------------------\n") 
  
  return(age.corr)      
}#EOF
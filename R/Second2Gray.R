##//////////////////////////////////////////////
##//Second2Gray.R
##/////////////////////////////////////////////

##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.2
#date: 03/02/2012
##======================================

Second2Gray<-function(values,dose_rate)
{
	values[,1]<-round(values[,1]*dose_rate[1], digits=2) #values[,1] should be D[e] values (first column)
	values[,2]<-round(sqrt((values[,1]*dose_rate[2])^2+(dose_rate[1]*values[,2])^2), digits=2) #values[,2] should be D[e] error
	return(values)
}
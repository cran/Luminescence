##//////////////////////////////////////////////
##//plmTransformation.R
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.3
#date: 18/02/2012
##======================================
##
##+++++++++++++++++++++++Preface+++++++++++++++++++++++(START)
##Input: data.frame(time,cts)
##Output: data.frame(time,cts)Converted cts
##+++++++++++++++++++++++Preface+++++++++++++++++++++++(END)
CW2pLM<-function(values){
             
            ##curve transformation
    					P<-2*max(values[,1])
	  					u<-((2*values[,1]*P)^0.5)
 								
	 					 ##cw >> plm conversion, according Bulur 
								values[,2]<-values[,2]*(u/P)
								values<-data.frame(u,values[,2])
                                      
            return(values)
}
##EOF

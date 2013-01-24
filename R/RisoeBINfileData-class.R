##//////////////////////////////////////////////////////////////
##//RisoeBINfileData-class.R
##/////////////////////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.1.1
#date: 16/03/2012
##======================================
##
## S4 class definition
##=================================================================================================##
##Risoe.BINdata##
##=================================================================================================##
setClass("Risoe.BINfileData",
         representation(
           METADATA="data.frame",
           DATA = "list"           
           ),
         S3methods=TRUE
         )

##set generic S4 function for object
setMethod("show", signature(object = "Risoe.BINfileData"),
          function(object){
            
            version<-object@METADATA[1,"VERSION"]
            systemID<-object@METADATA[1,"SYSTEMID"]
            records.overall<-length(object@DATA)
            records.type<-summary(object@METADATA[,"LTYPE"])
            user<-as.character(object@METADATA[1,"USER"])
            date<-as.character(object@METADATA[1,"DATE"])      
            run.range<-range(object@METADATA[,"RUN"])
            set.range<-range(object@METADATA[,"SET"])
            pos.range<-range(object@METADATA[,"POSITION"])
            
            ##print
            cat("\nRisoe.BINfileData Object")
            cat("\n\tVersion:             ", version)
            cat("\n\tObject Date:         ", date) 
            cat("\n\tUser:                ", user)
            cat("\n\tSystem ID:           ", systemID)
            cat("\n\tOverall Records:     ", records.overall)
            cat("\n\tRecords Type:        ", sapply(1:length(records.type),function(x){paste(
              names(records.type)[x],
              "=",records.type[x],";",
              sep=""
              )
                                                                                       
            }
                                                    )
                )
            cat("\n\tPosition Range:      ",pos.range[1],":",pos.range[2])
            cat("\n\tRun Range:           ",run.range[1],":",run.range[2])
            cat("\n\tSet Range:           ",set.range[1],":",set.range[2])
          }#end function          
          )#end setMethod

        
##-------------------------------------------------------------------------------------------------##
##=================================================================================================##

##//////////////////////////////////////////////////////////////////////////////
##//RLum.Analysis-class.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen/Freiberg Instruments
##version: 0.1
##date: 2013-03-27
##==============================================================================

##class definition
setClass("RLum.Analysis",
         representation(
           records = "list",
           protocol = "character"
         ),
         contains = "RLum",
         prototype = list (
           records = list(),
           protocol = character()
         ),                
         S3methods=FALSE
)


# show method for object -------------------------------------------------------

  setMethod("show", 
            signature(object = "RLum.Analysis"),
            function(object){
                          
              ##print
              cat("\n [RLum.Analysis Object]")
              cat("\n\t protocol:", object@protocol)
              cat("\n\t number of records:", length(object@records))  
              
              ##get object class types
              temp<-sapply(1:length(object@records), function(x){
                             
                            is(object@records[[x]])[1]
                            
                           }
                          )
             
              ##print object class types
              sapply(1:length(table(temp)), function(x){
                
                ##show RLum class type
                cat("\n\t .. :",names(table(temp)[x]),":", 
                    table(temp)[x])
                
                ##show structure
                cat("\n\t .. .. : ", 
                    sapply(1:length(object@records), 
                           function(x) {
                                        paste(object@records[[x]]@recordType,
                                        if(x%%10==0 & x!=length(object@records)){"\n\t .. .. : "})
                            }))
                      
              }
              )
              
            }                              
            )##end show method


# get object structure ----------------------------------------------------

##method to show the object structure
setGeneric("get_structure.RLum.Analysis",
           function(object) {standardGeneric("get_structure.RLum.Analysis")})

setMethod("get_structure.RLum.Analysis", 
          signature=signature(object = "RLum.Analysis"), 
          definition = function(object) {
          
            ##check if the object containing other elements than allowed
            if(length(grep(FALSE, sapply(object@records, is, class="RLum.Data.Curve")))!=0){
              
              stop("[get_structure.RLum.Analysis] Error: Only 'RLum.Data.Curve' objects are allowed!" )
              
            }
            
            ##get length object
            temp.object.length <- length(object@records)
            
            ##ID
            temp.id <- 1:temp.object.length
            
            ##OBJECT TYPE
            temp.recordType <- c(NA)
            length(temp.recordType) <- temp.object.length
            temp.recordType <- sapply(1:temp.object.length, function(x){object@records[[x]]@recordType})
            
            ##PROTOCOL STEP
            temp.protocol.step <- c(NA)
            length(temp.protocol.step) <- temp.object.length
            
            ##n.channels
            temp.n.channels <- sapply(1:temp.object.length, function(x){length(object@records[[x]]@data[,1])})
                
            ##X.MIN
            temp.x.min <- sapply(1:temp.object.length, function(x){min(object@records[[x]]@data[,1])})

            ##X.MAX
            temp.x.max <- sapply(1:temp.object.length, function(x){max(object@records[[x]]@data[,1])})

            ##y.MIN
            temp.y.min <- sapply(1:temp.object.length, function(x){min(object@records[[x]]@data[,2])})

            ##X.MAX
            temp.y.max <- sapply(1:temp.object.length, function(x){max(object@records[[x]]@data[,2])})
            
            ##info elements as character value
            temp.info.elements <- unlist(sapply(1:temp.object.length, function(x){
                  
                  if(length(object@records[[x]]@info)!=0){
                    do.call(paste, as.list(names(object@records[[x]]@info)))
                  }else{NA}
                  
                  }))
    
            ##combine output to a data.frame
            return(data.frame(id=temp.id, recordType=temp.recordType, protocol.step=temp.protocol.step,
                              n.channels=temp.n.channels,
                              x.min=temp.x.min, x.max=temp.x.max, y.min=temp.y.min, y.max=temp.y.max,
                              info.elements=temp.info.elements))
           
          })


# constructor (set) method for object class ------------------------------------------

setGeneric("set_RLum.Analysis",
           function(records, protocol) {standardGeneric("set_RLum.Analysis")})


setMethod("set_RLum.Analysis", 
          signature = c(records = "list", protocol= "ANY"), 
          
          function(records, protocol){             
            
            if(missing(protocol)){
              
              protocol <- "UNKNOWN"
              
            }else if (is(protocol, "character") == FALSE){
              
              stop("[set_RLum.Analysis] Error: 'protocol' has to be of type 'charcter'!")
              
            }
            
            new("RLum.Analysis", 
                records = records,
                protocol = protocol
               )
            
          })

# constructor (set) method for object class ------------------------------------------

setGeneric("get_RLum.Analysis",
           function(object, record.id, recordType) {standardGeneric("get_RLum.Analysis")})


setMethod("get_RLum.Analysis", 
          signature = c(object = "RLum.Analysis", record.id = "ANY", recordType = "ANY"), 
          
          function(object, record.id, recordType){             
            
            ##record.id
            if(missing(record.id) == TRUE){
              
              record.id <- c(1:length(object@records))
              
            }else if (is(record.id, "numeric") == FALSE){
              
              stop("[get_RLum.Analysis] Error: 'record.id' has to be of type 'numeric'!")
              
            }
            
            ##recordType
            if(missing(recordType) == TRUE){
              
              recordType <- unique(
                              unlist(
                                lapply(1:length(object@records), 
                                       function(x){object@records[[x]]@recordType})))
              
            }else if (is(recordType, "character") == FALSE){
              
              stop("[get_RLum.Analysis] Error: 'recordType' has to be of type 'character'!")
              
            }
            
            
           ##select curves according to the chosen paramter
           if(length(record.id)>1){  
             
            temp <- sapply(record.id, function(x){
                    
                                if(object@records[[x]]@recordType%in%recordType){
                                  object@records[[x]]
                                }
                              })
            
            ##remove empty list element
            temp[!sapply(temp, is.null)]
            
            
            
           }else{
             
            
             object@records[[record.id]]   
             
           }
            
          })



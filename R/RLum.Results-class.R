##//////////////////////////////////////////////////////////////////////////////
##//RLum.Results-class.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: JLU Giessen/Freiberg Instruments
##version: 0.2
##date: 2013-09-25
##==============================================================================

##class definition
setClass("RLum.Results",
         representation(
           originator = "character",
           data = "list"
         ),
         contains = "RLum",
         prototype = list (
           originator = character(),
           data = list()
         ),                
         S3methods=TRUE
)


# Validation --------------------------------------------------------------

setValidity("RLum.Results",
            function(object){
              
             ##calc_OSLLxTxRatio
             if(object@originator == "calc_OSLLxTxRatio"){
               
               #print(is(object@data[[1]], "data.frame"))
               
             }
              
              
            }           
            )

# constructor (set) method for object class -------------------------------

setGeneric("set_RLum.Results",
           function(originator, data) {standardGeneric("set_RLum.Results")})

setMethod("set_RLum.Results", 
          signature = c(originator = "ANY", data = "list"), 
          
          function(originator, data){             
                         
            if(missing(originator) == TRUE){
              
              ##originator is the calling function (function in which the 
              ##function set_RLum.Results is called)
              originator <- as.character(sys.call(which=-2)[[1]])
              
            }
            
            new("RLum.Results", 
                
                originator = originator,
                data = data)
            
          })


# GetMethods --------------------------------------------------------------


setGeneric("get_RLum.Results",
             function(object, data.object) {standardGeneric("get_RLum.Results")})

setMethod("get_RLum.Results", 
          signature=signature(object = "RLum.Results", data.object = "ANY"), 
          definition = function(object, data.object) {
            
            if(missing(data.object)==FALSE){
             if(is(data.object, "character")==FALSE){
              
                stop("[get_RLum.Results] Error: 'data.object' has to be a character!")
              
              }
            }
            
            ##allow to access a specific data object
            if(missing(data.object) == FALSE){
              
              if(is.null(try(object@data[[data.object]])) == TRUE){
                
                error.message1 <- paste(names(object@data), collapse = ", ")
                error.message <- paste("[get_RLum.Results] Error: data.object unknown. Valid object names are: ", error.message1)
               
                stop(error.message)
                
              }else{
                
                return(object@data[[data.object]])
                
              }
              
              
            }else{
            
            ##-------------------------------------------------------------
            ##calc_OSLLxTxRatio            
            if(object@originator == "calc_OSLLxTxRatio") {
              
              return(as.data.frame(object@data))
              
            }
            
            ##-------------------------------------------------------------
            ##calc_TLLxTxRatio            
            if(object@originator == "calc_TLLxTxRatio") {
              
              return(as.data.frame(object@data))
              
            }
            
            ##-------------------------------------------------------------
            ##plot_GrowthCurve         
            if(object@originator == "plot_GrowthCurve") {
              
              if(missing(data.object)==TRUE){
                
                return(object@data$De)
                
              }else{
                
                if(data.object%in%names(object@data)==FALSE){
                
                  #valid.names <- names(object@data))
                  stop(paste("\n[get_RLum.Results] Error: 'data.object' is unknown for this RLum.Results object produced by ", object@originator,"()! 
                             Valid 'data.objects' are: ",paste(names(object@data), collapse=", "), sep=""))
                  
                }else{
                  
                  return(object@data[data.object][[1]])
                  
                }
                
              }
              
            }
            
            ##-------------------------------------------------------------
            ##analyse_SAR.CWOSL        
            if(object@originator == "analyse_SAR.CWOSL") {
              
              return(object@data[[1]])
              
            }
            
            ##-------------------------------------------------------------
            ##analyse_IRSAR.RF        
            if(object@originator == "analyse_IRSAR.RF") {
              
              return(object@data$De.values)
              
            }
            
            ##-------------------------------------------------------------
            ##fit_CWCurve        
            if(object@originator == "fit_CWCurve") {
              
              return(object@data$output.table)
              
            }
            
            ##-------------------------------------------------------------
            ##fit_LMCurve        
            if(object@originator == "fit_LMCurve") {
              
              return(object@data$output.table)
              
            }
            
          }##end if missing data.object 
          })

##=============================================================================##
# merge_RLum.Results ------------------------------------------------------
## merging is done by append objects to the first object in a list

setGeneric("merge_RLum.Results",
           function(object.list) {standardGeneric("merge_RLum.Results")})

setMethod("merge_RLum.Results", 
          signature=signature(object.list = "list"), 
          definition = function(object.list){
            
            ##-------------------------------------------------------------
            ##Some integrity checks
            
            ##check if input object is a list
            if(is(object.list, "list") == FALSE){
              
              stop("[merge_RLum.Results] Error: 'object.list' has to of type 'list'!")
              
            }else{
                            
              ##check if objects in the list are of type RLum.Results
              temp.originator <- sapply(1:length(object.list), function(x){
                
                if(is(object.list[[x]], "RLum.Results") == FALSE){
                  
                  stop("[merge_RLum.Results] Error: objects to merge have
                       to be of type 'RLum.Results'!")
                  
                }
                
                object.list[[x]]@originator     
                
                })
            }
              
              ##check if originator is different 
              if(length(unique(temp.originator))>1){
                
                stop("[merge_RLum.Results] Error: 'RLum.Results' object originator 
differs!")
              }   
    
            ##-------------------------------------------------------------
            ##merge objects depending on the data structure
            
            for(i in 1:length(object.list[[1]]@data)){
              
              ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
              ##vector or data.frame or matrix 
              if(is(object.list[[1]]@data[[i]], "data.frame") == TRUE ||
                 is(object.list[[1]]@data[[i]], "vector") == TRUE || 
                 is(object.list[[1]]@data[[i]], "matrix") == TRUE){
                
                ##grep elements and write them into a list
                temp.list <- lapply(1:length(object.list), function(x){
                                                        
                     object.list[[x]]@data[[i]]                     
                                                        
                   })
                

                ##combine them using rbind
                object.list[[1]]@data[[i]] <- do.call(rbind,temp.list)
                
              }else{
                
                ##++++++++++++++++++++++++++++++++++++++++++++++++++++++++
                ##all other elements 
                
                ##grep elements and write them into a list
                object.list[[1]]@data[[i]] <- lapply(1:length(object.list), function(x){
                  
                  object.list[[x]]@data[[i]]                     
                  
                })
                
              }
              
            }##end loop

            ##return
            return(object.list[[1]])
            
            
          })##end set method

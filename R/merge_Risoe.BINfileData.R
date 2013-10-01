##//////////////////////////////////////////////////////////////////////////////
##//merge_Risoe.BINfileData.R
##//////////////////////////////////////////////////////////////////////////////
##==============================================================================
##author: Sebastian Kreutzer
##organisation: Freiberg Instruments/JLU Giessen
##version: 0.1
##date: 2013-09-06
##==============================================================================


merge_Risoe.BINfileData <- function(input.objects,
                                    output.file,
                                    keep.position.number = FALSE
                                    ){

  
# Integrity Checks --------------------------------------------------------

  if(length(input.objects) < 2){
    
    stop("[merge_Risoe.BINfileData] 
                Error: At least two input objects are needed!")    
    
  }

  if(is(input.objects, "character") == TRUE){
    
      for(i in 1:length(input.objects)){
        
        if(file.exists(input.objects[i])==FALSE){
          
          stop("[merge_Risoe.BINfileData] 
                Error: File",input.objects[i],"does not exists!")
          
        }
        
      }  

  }else{
    
    if(is(input.objects, "list") == TRUE){
      
      for(i in 1:length(input.objects)){
        
        if(is(input.objects[[i]], "Risoe.BINfileData") == FALSE){
          
          stop("[merge_Risoe.BINfileData] 
                Error: Input list does not contain Risoe.BINfileData objects!")
          
        }
        
      }
        
      }else{
        
        stop("[merge_Risoe.BINfileData] 
                Error: Input object is not a 'character' nor a 'list'!")
        
      }
    
  }
    
    
# Import Files ------------------------------------------------------------



  ##set temp object
  temp <- list()


  ##loop over all files to store the results in a list
  ##or the input is already a list

  if(is(input.objects, "character") == TRUE){
  for(i in 1:length(input.objects)){
    
    temp[i] <- readBIN2R(input.objects[i])  
        
  }
  }else{
    
   temp <- input.objects
    
  }

# Get POSITION vales -------------------------------------------------------

  ##grep maximum position value from the first file
  temp.position.max <- max(temp[[1]]@METADATA[, "POSITION"])

  ##grep all position values except from the first file
  temp.position.values <- sapply(2:length(temp), function(x){
    
    temp <- temp[[x]]@METADATA[, "POSITION"]+temp.position.max
    temp.position.max <- max(temp)
    
    return(temp)
  })

  temp.position.values <- c(temp[[1]]@METADATA[, "POSITION"], temp.position.values)

# Get overall record length -----------------------------------------------

  temp.record.length <- sum(sapply(1:length(temp), function(x){
    
    length(temp[[x]]@METADATA[,"ID"])
    
  }))


# Merge Files -------------------------------------------------------------
  
    ##Loop for similar input objects 
    for(i in 1:length(input.objects)){
    
     if(exists("temp.new.METADATA") == FALSE){
     
       temp.new.METADATA <- temp[[i]]@METADATA
       temp.new.DATA <- temp[[i]]@DATA
     
      }else{
    
       temp.new.METADATA <- rbind(temp.new.METADATA, temp[[i]]@METADATA)
       temp.new.DATA <- c(temp.new.DATA, temp[[i]]@DATA)
     
     }
    }
 

  ##SET RECORD ID in METADATA
  temp.new.METADATA$ID <- 1:temp.record.length

  ##SET POSITION VALUES 
  if(keep.position.number == FALSE){
    
    temp.new.METADATA$POSITION <- temp.position.values
    
  }

##TODO version number?
# Produce BIN file object -------------------------------------------------

  temp.new <- new("Risoe.BINfileData",
                  METADATA = temp.new.METADATA,
                  DATA = temp.new.DATA)  



# OUTPUT ------------------------------------------------------------------

  if(missing(output.file) == FALSE){

    writeR2BIN(temp.new, output.file)
    
  }else{
    
    return(temp.new)
    
  }

}

##//////////////////////////////////////////////////////////////////////////////
##//Risoe.BINfileData2RLum.Analysis.R
##//////////////////////////////////////////////////////////////////////////////
##
##==============================================================================
##author: Sebastian Kreutzer
##organisation: Freiberg Instruments, JLU Giessen
##version: 0.1
##date: 2013-08-04
##==============================================================================

Risoe.BINfileData2RLum.Analysis <- function (

  object,
  pos,
  run,
  set,
  ltype = c("IRSL","OSL","TL","RIR","RBR","USER", "RL"),
  protocol = "unknown"
  
  ){
  
  
# Integrity Check ---------------------------------------------------------

  if (is(object,"Risoe.BINfileData")==FALSE){
    stop("[Risoe.BINfileData2RLum.Analysis] Error: Input object is not of type 'Risoe.BINfileData'.")
  }

  if (missing(pos)==TRUE){
    stop("[Risoe.BINfileData2RLum.Analysis] Error: No value for 'pos' set.")
  }

  if (is(pos,"numeric")==FALSE){
   stop("[Risoe.BINfileData2RLum.Analysis] Error: Argument 'pos' has to be of data type integer.")
  }

  ##get and check valid positions
  positions.valid <- paste(as.character(unique(object@METADATA[,"POSITION"])), collapse=", ")

  if ((pos %in% unique(object@METADATA[,"POSITION"])) == FALSE){
   stop(paste("[Risoe.BINfileData2RLum.Analysis] Error: pos=",pos, " is no valid position. 
              Valid positions are: ", positions.valid, sep=""))
  }

  ##WARNINGS
  if (length(which(pos/1:48 == 1)) == 0){
    warning("[Risoe.BINfileData2RLum.Analysis] Value for 'pos' out bounds specified for
            a Risoe BIN-file.")
  }


# Grep run and set data ---------------------------------------------------


  ##grep values according to their criteria
  run <- unique(object@METADATA[, "RUN"])
  set <- unique(object@METADATA[, "SET"])

# Select values -----------------------------------------------------------

  ##deselect all values
  object@METADATA[, "SEL"] <- FALSE

  ##select data
  object@METADATA[
    which(
      object@METADATA[,"POSITION"] == pos &
      (object@METADATA[,"RUN"] %in% run) == TRUE &
      (object@METADATA[,"SET"] %in% set) == TRUE &
      (object@METADATA[,"LTYPE"] %in% ltype) == TRUE      
      )    
    , "SEL"] <- TRUE

# Limit object to selection -----------------------------------------------

  object@DATA <- object@DATA[object@METADATA[object@METADATA[,"SEL"] == TRUE,"ID"]]
  object@METADATA <- object@METADATA[object@METADATA[,"SEL"] == TRUE,]
    
# Convert values ----------------------------------------------------------

  new("RLum.Analysis", 
      records = lapply(1:length(object@DATA),function(x){
        
        ##calculate values for matrix 
        i<-seq(object@METADATA[x,"HIGH"]/object@METADATA[x,"NPOINTS"],
               object@METADATA[x,"HIGH"],
               by=object@METADATA[x,"HIGH"]/object@METADATA[x,"NPOINTS"])
      
        j<-unlist(object@DATA[x])
        
        ##set RLum.Data.Curve object
        set_RLum.Data.Curve(
          recordType = as.character(object@METADATA[x,"LTYPE"]),
          data = matrix(c(i,j),nrow=length(i),ncol=2),
          info = as.list(object@METADATA[x,]))
               
      }),
      protocol = protocol
  )
} 

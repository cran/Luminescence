Risoe.BINfileData2RLum.Analysis<- structure(function(#Convert Risoe.BINfileData object to an RLum.Analysis object
  ### Converts values from one specific position of a Risoe.BINfileData S4-class object
  ### to an RLum.Analysis object. 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.1 [2013-08-04]
  # ===========================================================================

  object,
  ### \code{\linkS4class{Risoe.BINfileData}} (\bold{required}): \code{Risoe.BINfileData} object
  
  pos,
  ### \code{\link{integer}} (\bold{required}): position number of the \code{Risoe.BINfileData}
  ### object for which the curves are stored in the \code{RLum.Analysis} object.
  
  run,
  ### \code{\link{vector}, \link{numeric}} (optional): run number from the measurement to limit
  ### the converted data set (e.g. run = c(1:48)).
  
  set,
  ### \code{\link{vector}, \link{numeric}} (optional): set number from the measurement to limit
  ### the converted data set (e.g. set = c(1:48)).
  
  ltype = c("IRSL","OSL","TL","RIR","RBR","USER", "RL"),
  ### \code{\link{vector}, \link{character}} (with default): curve type to limit 
  ### the converted data. Allowed values are: \code{IRSL}, \code{OSL}, 
  ### \code{TL}, \code{RIR}, \code{RBR} and \code{USER}
  
  protocol = "unknown"
  ### \code{\link{character}} (optional): sets protocol type for analysis 
  ### object. Value may be used by subsequent analysis functions. 
  
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

  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------

  ##details<<
  ## The \code{\linkS4class{RLum.Analysis}} object requires a set of curves for specific 
  ## further protocol analyses. However, the \code{\linkS4class{Risoe.BINfileData}} 
  ## usually contains a set of curves for different aliquots and different protocol 
  ## types that may be mixed up. Therefore, a conversion is needed. 

  ##value<<
  ## Returns an \code{\linkS4class{RLum.Analysis}} object. 

  ##references<<
  ## #

  ##note<<
  ## The \code{protocol} argument of the \code{\linkS4class{RLum.Analysis}} 
  ## object is set to 'unknown' if not stated otherwise.  

  ##seealso<<
  ## \code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum.Analysis}},
  ## \code{\link{readBIN2R}}

  ##keyword<<
  ## manip

}, ex=function(){
  
  ##load data
  data(ExampleData.BINfileData, envir = environment())
  
  ##convert values for position 1
  Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos = 1)
  
})#END OF STRUCTURE 
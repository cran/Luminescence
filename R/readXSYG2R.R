readXSYG2R <- structure(function(#Import XSYG files to R
  ### Import XSYG files produced by a Freiberg Instrument's lexsyg reader into R

  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.1
  # ===========================================================================
  
  file,
  ### \link{character} (\bold{required}): path and file name of the XSYG file
  
  import = TRUE,
  ### \link{logical} (with default): if set to \code{FALSE} only the XSYG file structure
  ### is shown
  
  txtProgressBar = TRUE
  ### \link{logical} (with default): enables \code{TRUE} or disables \code{FALSE}
  ## the progression bar during import
  
){
 
  ##TODO
  ## - one file can contain several sequences  
  ## allow for short check, i.e. just show a data.frame with all sequences 
  ## data in the file
  
  
  # (0) config --------------------------------------------------------------
  #version.supported <- c("1.0")
  
  #additional functions
  ##get curve value
  get_XSYG.curve.values <- function(curve.node){
    
    ##1st string split 
    curve.node <- unlist(strsplit(xmlValue(curve.node), ";"))
    
    ##2nd string split
    curve.node <- as.numeric(unlist(strsplit(curve.node,"[:,:]")))
    
    ##set as matrix
    curve.node <- t(matrix(curve.node, nrow=2))
    
  }
  
  
  # (1) Integrity tests -----------------------------------------------------
  
  ##parse XML tree using the package XML
  temp <- try(xmlRoot(xmlTreeParse(file)), silent = TRUE)
  
  ##show error
  if(is(temp, "try-error") == TRUE){
    
    stop("[readXSYG2R.R] >> Error: XML file not readable!)")
    
  }
  
  # (2) Further file processing ---------------------------------------------
  
  ##==========================================================================##
  ##SHOW STRUCTURE
  if(import == FALSE){
    
    ##sample information
    temp.sample <- as.data.frame(xmlAttrs(temp))
    colnames(temp.sample) <- ""
    
    ##grep sequences files
    
    ##set data.frame
    temp.sequence.header <- data.frame(t(1:length(names(xmlAttrs(temp[[1]])))))       
    colnames(temp.sequence.header) <- names(xmlAttrs(temp[[1]]))
    
    
    ##fill information in data.frame
    for(i in 1:length(temp)){
      
      temp.sequence.header[i,] <- t(xmlAttrs(temp[[i]]))
      
    }
   
    output <-  list(Sample = temp.sample, Sequences = temp.sequence.header)
    return(output)
    
  }else{
    
  
  ##==========================================================================##
  ##IMPORT XSYG FILE
  
  ##Display output
  cat("[readXSYG2R]\n")
    
  ##PROGRESS BAR
  if(txtProgressBar ==TRUE){
      pb <- txtProgressBar(min=0,max=length(temp), char = "=", style=3)
  }  
    
  ##create list
  output <- list()
  
  ##loop over the entire sequence by sequence
  output <- lapply(1:length(temp), function(x){
    
    ##read sequence header
    temp.sequence.header <- as.data.frame(xmlAttrs(temp[[x]]))
    colnames(temp.sequence.header) <- ""
  
    
     ###-----------------------------------------------------------------------
     ###LOOP
     ##read records >> records are combined to one RLum.Analysis object
     temp.sequence.object <- unlist(lapply(1:length(temp[[x]]), function(i){
        
      ##get recordType
      temp.sequence.object.recordType <- xmlAttrs(temp[[x]][[i]])["recordType"]
 
      ##grep data curve >> measured TODO: just the first curve is chosen
      ##what we do with the other curves?
#       temp.sequence.object.curveValue <- getNodeSet(temp[[x]][[i]], 
#                         "//Curve[@curveType='measured']")[[1]]
      
       lapply(1:length(temp[[x]][[i]]), function(j){
         
         ##get values
         temp.sequence.object.curveValue <- temp[[x]][[i]][[j]]
        
         ##get curveType
         temp.sequence.object.curveType <- as.character(xmlAttrs(temp[[x]][[i]][[j]])["curveType"])
         
         ##get detector
         temp.sequence.object.detector <- as.character(xmlAttrs(temp[[x]][[i]][[j]])["detector"])
         
         temp.sequence.object.info <- as.list(xmlAttrs(temp.sequence.object.curveValue)) 
      
         set_RLum.Data.Curve(recordType = paste(temp.sequence.object.recordType, 
                                                " (",temp.sequence.object.detector,")",
                                                sep = ""),
                    curveType = temp.sequence.object.curveType,
                    data = get_XSYG.curve.values(temp.sequence.object.curveValue),
                    info = temp.sequence.object.info)  
         
       })
       ##grep further curve values
       #temp.sequence.object.info <- as.list(xmlAttrs(temp.sequence.object.curveValue))     
      
       ##create curve object
#        set_RLum.Data.Curve(recordType = temp.sequence.object.recordType,
#                            data = get_XSYG.curve.values(temp.sequence.object.curveValue),
#                            info = temp.sequence.object.info)        
      
     }))

     ##set RLum.Analysis object
     temp.sequence.object <-  set_RLum.Analysis(
                                records = temp.sequence.object, 
                                protocol = as.character(
                                  temp.sequence.header["protocol",1]))
    
    
    
    ##update progress bar
    if(txtProgressBar == TRUE){
      setTxtProgressBar(pb, x)
    }
    
    ##merge output
    temp.output <- list(Sequence.Header = temp.sequence.header,
                        Sequence.Object = temp.sequence.object)
        
    
    
  })##end loop for sequence
   
  ##close ProgressBar
  if(txtProgressBar == TRUE){close(pb)}
  
  cat(paste("\t >>",length(temp), " sequence(s) loaded successfully."), sep = "")
  
  ##output
  invisible(output)
 
  }#end if 
 
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##value<<
  ## \bold{Using the option \code{import = FALSE}}\cr\cr
  ## A list consisting of two elements is shown 
  ## \item{Sample}{\link{data.frame} with information on file.}
  ## \item{Sequences}{\link{data.frame} with information on the sequences 
  ## stored in the XSYG file}\cr\cr
  ##\bold{Using the option \code{import = TRUE} (default)} \cr\cr
  ## A list is provided, the list elements contain:
  ## \item{Sequence.Header}{\link{data.frame} with information on the sequence.}
  ## \item{Sequence.Object}{\code{\linkS4class{RLum.Analysis}} containing the curves}
  
  ##details<<
  ## \bold{How the import functions works?}\cr\cr
  ## The function uses the \code{\link{xml}} package to parse the file structure.
  ## Each sequence is subsequently translated into an \code{\linkS4class{RLum.Analysis}}
  ## object.\cr\cr
  ##
  ## \bold{General structure XSYG format}\cr\cr
  ## \code{<?xml?}\cr
  ## \code{ <Sample>}\cr
  ## \code{  <Sequence>}\cr
  ## \code{   <Record>}\cr
  ## \code{    <Curve name="first curve" />}\cr
  ## \code{     <Curve name="curve with data">}\cr
  ## \code{      x0 , y0 ; x1 , y1 ; x2 , y2 ; x3 , y3}\cr
  ## \code{     </Curve>}\cr
  ## \code{   </Record>}\cr
  ## \code{  </Sequence>}\cr
  ## \code{ </Sample>}\cr\cr
  ## So far each XSYG file can only contain one \code{<Sample></Sample>}, but 
  ## multiple sequences. \cr\cr
  ## Each record may comprises several curves.
  
  ##references<<
  ## Grehl, S., Kreutzer, S., Hoehne, M., 2013. Documentation of the XSYG file format.
  ## Unpublished Technical Note. Freiberg, Germany \cr\cr
  ## \bold{Further reading} \cr\cr
  ## XML: \url{http://en.wikipedia.org/wiki/XML}
  
  ##note<<
  ## This function is a beta version as the XSYG file format is not yet fully
  ## specified. 
  ## Thus further file operations (merge, export, write) should be done using the functions
  ## provided with the package \code{\link{xml}}
  
  ##seealso<<
  ## \code{\link{xml}}, \code{\linkS4class{RLum.Analysis}}, 
  ## \code{\linkS4class{RLum.Data.Curve}}
  
  ##keyword<<
  ## IO
  
}, ex=function(){
  
  ##Import XSYG file
  ##uncomment for usage
  #temp <- readXSYG2R("input_file.xsyg")
  
  ##Additional examples for pure XML import using the package XML
  ##uncomment for usage
  
    ##import entire file
    #temp <- xmlRoot(xmlTreeParse("input_file.xsyg"))
    
    ##search for specific subnodes with curves containing 'OSL'
    #getNodeSet(temp, "//Sample/Sequence/Record[@recordType = 'OSL']/Curve")
    
})
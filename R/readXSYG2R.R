readXSYG2R <- structure(function(#Import XSYG files to R
  ### Imports XSYG files produced by a Freiberg Instrument lexsyg reader into R.

  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.2 [2013-11-20]
  # ===========================================================================
  
  file,
  ### \link{character} (\bold{required}): path and file name of the XSYG file.
  
  recalculate.TL.curves = TRUE, 
  ### \link{logical} (with default): if set to \code{TRUE}, TL curves are returned 
  ### as temperature against count values (see details for more information)
  ### Note: The option overwrites the time vs. count TL curve. Select \code{FALSE}
  ### to import the raw data delivered by the lexsyg.
  
  import = TRUE,
  ### \link{logical} (with default): if set to \code{FALSE}, only the XSYG file structure
  ### is shown.
  
  txtProgressBar = TRUE
  ### \link{logical} (with default): enables \code{TRUE} or disables \code{FALSE}
  ### the progression bar during import
  
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
  
  get_XSYG.spectrum.values <- function(curve.node){
    
    ##1st grep wavelength table
    wavelength <- xmlAttrs(curve.node)["wavelengthTable"]
    
    ##string split
    wavelength <- as.numeric(unlist(strsplit(wavelength, ";")))
    
    ##2nd grep time values
    curve.node <- unlist(strsplit(xmlValue(curve.node), ";"))  
    curve.node <- unlist(strsplit(curve.node, ","), recursive = FALSE)
    
    curve.node.time <- as.numeric(curve.node[seq(1,length(curve.node),2)])
    
    ##3rd grep count values
    curve.node.count <- as.character(curve.node[seq(2,length(curve.node),2)])
    
    ##remove from pattern...
    curve.node.count <- do.call("gsub", list(pattern="[[]|[]]", replacement=" ",
                                             x=curve.node.count))
    
    ##4th combine to spectrum matrix
    spectrum.matrix <- matrix(0,length(wavelength),length(curve.node.time))
    spectrum.matrix <- sapply(1:length(curve.node.time), function(x){
      
      as.numeric(unlist(strsplit(curve.node.count[x], "[|]")))
      
    })
    
    
    ##change row names (rows are wavelength)
    rownames(spectrum.matrix) <- round(wavelength, digits=3)
    
    ##change column names (columns are time/temp values)
    colnames(spectrum.matrix) <- round(curve.node.time, digits=3)
    
    
    return(spectrum.matrix)
  }
  
  # (1) Integrity tests -----------------------------------------------------
  
  ##parse XML tree using the package XML
  temp <- try(xmlRoot(xmlTreeParse(file, useInternalNodes = TRUE)), silent = TRUE)

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
    for(i in 1:xmlSize(temp)){
      
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
      pb <- txtProgressBar(min=0,max=xmlSize(temp), char = "=", style=3)
  }  
    
  ##create list
  output <- list()
  
  ##loop over the entire sequence by sequence
  output <- lapply(1:xmlSize(temp), function(x){
    
    ##read sequence header
    temp.sequence.header <- as.data.frame(xmlAttrs(temp[[x]]))
    colnames(temp.sequence.header) <- ""  
    
     ###-----------------------------------------------------------------------
     ##LOOP
     ##read records >> records are combined to one RLum.Analysis object
     temp.sequence.object <- unlist(lapply(1:xmlSize(temp[[x]]), function(i){
        
      ##get recordType
      temp.sequence.object.recordType <- xmlAttrs(temp[[x]][[i]])["recordType"]
      
       lapply(1:xmlSize(temp[[x]][[i]]), function(j){       
         
         ##get values
         temp.sequence.object.curveValue <- temp[[x]][[i]][[j]]
        
         ##get curveType
         temp.sequence.object.curveType <- as.character(
           xmlAttrs(temp[[x]][[i]][[j]])["curveType"])
         
         ##get detector
         temp.sequence.object.detector <- as.character(
           xmlAttrs(temp[[x]][[i]][[j]])["detector"])
         
         ##get additional information
         temp.sequence.object.info <- as.list(xmlAttrs(temp.sequence.object.curveValue)) 

      
      
         ## TL curve recalculation ============================================
         if(recalculate.TL.curves == TRUE){
         
         ##TL curve heating values is stored in the 3rd curve of every 
         if(temp.sequence.object.recordType == "TL" && 
            "Spectrometer" %in% temp.sequence.object.detector == FALSE &&
            j == 1){
           
            #grep values from PMT measurement 
            temp.sequence.object.curveValue.PMT <- get_XSYG.curve.values(
              temp[[x]][[i]][[j]])
            
              ##round values (1 digit is resolution of heating element)
              temp.sequence.object.curveValue.PMT[,1] <- round(
                temp.sequence.object.curveValue.PMT[,1], digits = 1)
            
            #grep values from heating element
            temp.sequence.object.curveValue.heating.element <- get_XSYG.curve.values(
              temp[[x]][[i]][[3]])
            
             #reduce matrix values to values of the heating element
             temp.sequence.object.curveValue.heating.element <- 
              temp.sequence.object.curveValue.heating.element[
                temp.sequence.object.curveValue.heating.element[,1] >=
                  min(temp.sequence.object.curveValue.PMT[,1]) &
                temp.sequence.object.curveValue.heating.element[,1] <=
                  max(temp.sequence.object.curveValue.PMT[,1]),]
            
                ## calculate corresponding heating rate, this makes only sense
                ## for heating, therefor is has to be the maximum value
              
                ##remove 0 values (not measured) and limit to peak
                heating.rate.values <- temp.sequence.object.curveValue.heating.element[
                  temp.sequence.object.curveValue.heating.element[,2] > 0 & 
                  temp.sequence.object.curveValue.heating.element[,2] <= 
                    max(temp.sequence.object.curveValue.heating.element[,2]),]
          
                heating.rate <- (heating.rate.values[length(heating.rate.values[,2]), 2] -
                                 heating.rate.values[1,2])/
                                (heating.rate.values[length(heating.rate.values[,1]), 1] -
                                 heating.rate.values[1,1])
              
                                
                ##round values  
                heating.rate <- round(heating.rate, digits=1)
              
                ##add to info element
                temp.sequence.object.info <- c(temp.sequence.object.info, 
                                               RATE = heating.rate)
            
            ##grep unqiue values 
            temp.sequence.object.curveValue.PMT.unique <-  unique(
              temp.sequence.object.curveValue.PMT[,1])
            
            ##calculate mean count value for similar entries
            temp.sequence.object.curveValue.PMT.counts <- sapply(
              1:length(temp.sequence.object.curveValue.PMT.unique), function(z){
                
                mean(temp.sequence.object.curveValue.PMT[
                  temp.sequence.object.curveValue.PMT[,1] == 
                  temp.sequence.object.curveValue.PMT.unique[z], 2])

              })
            
            ##set new set
            temp.sequence.object.curveValue.PMT <- as.matrix(cbind(
              temp.sequence.object.curveValue.PMT.unique,
              temp.sequence.object.curveValue.PMT.counts))
            

            ##combine values
            temperature.values <- sapply(
              1:length(temp.sequence.object.curveValue.PMT[,1]), function(n){
              
              temp.sequence.object.curveValue.PMT[n,2] <- 
               temp.sequence.object.curveValue.heating.element[
                  temp.sequence.object.curveValue.heating.element[,1] ==
                  temp.sequence.object.curveValue.PMT[n,1], 2]
              
             })
            
            ##combine as matrix
            temp.sequence.object.curveValue <- as.matrix(cbind(
              temperature.values,
              temp.sequence.object.curveValue.PMT[,2]))
            
            temp.sequence.object.info$curveDescripter <- "T [\u00B0C]; cts [a.u.]"
          
            ##order values by increasing temperature
            temp.sequence.object.curveValue <- 
              temp.sequence.object.curveValue[order(
                temp.sequence.object.curveValue[,1]),]
          
          }##endif          
         }##endif recalculate.TL.curves == TRUE
        
         
         ##Set RLum.Data objects  
         if("Spectrometer" %in% temp.sequence.object.detector == FALSE){           
           
            if(is(temp.sequence.object.curveValue, "matrix") == FALSE){
              
              temp.sequence.object.curveValue <- 
                get_XSYG.curve.values(temp.sequence.object.curveValue)
              
            }
          
           
         set_RLum.Data.Curve(recordType = paste(temp.sequence.object.recordType, 
                                                " (",temp.sequence.object.detector,")",
                                                sep = ""),
                    curveType = temp.sequence.object.curveType,
                    data = temp.sequence.object.curveValue,
                    info = temp.sequence.object.info)  
         
         }else if("Spectrometer" %in% temp.sequence.object.detector == TRUE) {
    
           set_RLum.Data.Spectrum(recordType = paste(temp.sequence.object.recordType, 
                                                  " (",temp.sequence.object.detector,")",
                                                  sep = ""),
                               curveType = temp.sequence.object.curveType,
                               data = get_XSYG.spectrum.values(
                                 temp.sequence.object.curveValue),
                               info = temp.sequence.object.info)  

         }
         
       })   
      
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
  
  cat(paste("\t >>",xmlSize(temp), " sequence(s) loaded successfully."), sep = "")
  




  
  
  
  ##output
  invisible(output)
 
  }#end if 
 
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##value<<
  ## \bold{Using the option \code{import = FALSE}}\cr\cr
  ## A list consisting of two elements is shown:
  ## \item{Sample}{\link{data.frame} with information on file.}
  ## \item{Sequences}{\link{data.frame} with information on the sequences 
  ## stored in the XSYG file}.\cr\cr
  ##\bold{Using the option \code{import = TRUE} (default)} \cr\cr
  ## A list is provided, the list elements contain:
  ## \item{Sequence.Header}{\link{data.frame} with information on the sequence.}
  ## \item{Sequence.Object}{\code{\linkS4class{RLum.Analysis}} containing the curves.}
  
  ##details<<
  ## \bold{How does the import function work?}\cr\cr
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
  ## So far, each XSYG file can only contain one \code{<Sample></Sample>}, but 
  ## multiple sequences. \cr\cr
  ## Each record may compris several curves.\cr\cr
  ##
  ## \bold{TL curve recalculation}\cr
  ## On the lexsyg device TL curves are recorded as time against count values. 
  ## Temperature values are monitored on the heating plate and stored in a separate
  ## curve (time vs. temperature). If the option \code{recalculate.TL.curves = TRUE}
  ## is chosen, the time values for each TL curve are replaced by temperature values.\cr
  ##
  ## Practically, this means combining two matrices with different row numbers by 
  ## their time values. If multiple count values per temperature are obtained, 
  ## the mean of count values per temperature value is calculated. 
  ## Note: Temperature values for spectrum curves are currently not supported. 
  
  ##references<<
  ## Grehl, S., Kreutzer, S., Hoehne, M., 2013. Documentation of the XSYG file format.
  ## Unpublished Technical Note. Freiberg, Germany \cr\cr
  ## \bold{Further reading} \cr\cr
  ## XML: \url{http://en.wikipedia.org/wiki/XML}
  
  ##note<<
  ## This function is a beta version as the XSYG file format is not yet fully
  ## specified. 
  ## Thus, further file operations (merge, export, write) should be done using the functions
  ## provided with the package \code{\link{xml}}.\cr
  ##
  ## \bold{So far, no image data import is provided!}\cr
  ## Corresponding values in the XSXG file are skipped. 
  
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
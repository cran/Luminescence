##//////////////////////////////////////////////////////////////////////////////
##//readBIN2R.R
##//////////////////////////////////////////////////////////////////////////////
##=============================================================================#
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.5.1
#date: 2013-03-10
##=============================================================================#
##

readBIN2R <- function(file,
                      show.raw.values = FALSE,  
                      n.records,
                      show.record.number = FALSE,
                      txtProgressBar = TRUE
                      ){

  
  
# Config ------------------------------------------------------------------  
  
  ##set supported BIN format version
  VERSION.supported <- as.raw(c(03,04))
  
# Set Translation Matrices ------------------------------------------------

##LTYPE
LTYPE.TranslationMatrix <- matrix(NA, nrow=14, ncol=2)
LTYPE.TranslationMatrix[,1] <- 0:13 
LTYPE.TranslationMatrix[,2] <- c("TL",
                                 "OSL",
                                 "IRSL",
                                 "M-IR",
                                 "M-VIS",
                                 "TOL",
                                 "TRPOSL",
                                 "RIR",
                                 "RBR",
                                 "USER",
                                 "POSL",
                                 "SGOSL",
                                 "RL",
                                 "XRF")

##DTYPE
DTYPE.TranslationMatrix <- matrix(NA, nrow=8, ncol=2)
DTYPE.TranslationMatrix[,1] <- 0:7
DTYPE.TranslationMatrix[,2] <- c("Natural","N+dose","Bleach",
                                 "Bleach+dose","Natural (Bleach)",
                                 "N+dose (Bleach)","Dose","Background")  


##LIGHTSOURCE
LIGHTSOURCE.TranslationMatrix <- matrix(NA, nrow=8, ncol=2)
LIGHTSOURCE.TranslationMatrix[,1] <- 0:7
LIGHTSOURCE.TranslationMatrix[,2] <- c("None",
                                       "Lamp",
                                       "IR diodes/IR Laser",
                                       "Calibration LED",
                                       "Blue Diodes",
                                       "White light",
                                       "Green laser (single grain)",
                                       "IR laser (single grain)"
                                       ) 
 
# Open Connection ---------------------------------------------------------


#open connection
con<-file(file, "rb")

   ##get information about file size
   file.size<-file.info(file)

   ##output
   cat(paste("\n[readBIN2R.R]\n\t >> ",file,sep=""), fill=TRUE)
    
   ##set progressbar
   if(txtProgressBar==TRUE){
    pb<-txtProgressBar(min=0,max=file.size$size, char="=", style=3)
   }

##read data up to the end of con

##set ID
ID<-0



# LOOP --------------------------------------------------------------------

##start loop for import BIN data
while(length(VERSION<-readBin(con, what="raw", 1, size=1, endian="litte"))>0) {

      ##stop input if wrong VERSION    
      if((VERSION%in%VERSION.supported) == FALSE){
        
        ##close connection 
        close(con)
          
        ##show error message
        stop("[readBIN2R] Error: This BIN format version is currently not supported! Please check the manual for details.")
      
      }    
  
      ##print record ID for debugging purposes
      if(show.record.number == TRUE){
          
          cat(ID,", ")
          if(ID%%10==0){
            cat("\n")    
          }
        } 
  
      #empty byte position
      EMPTY<-readBin(con, what="raw", 1, size=1, endian="litte")
 
  ##LENGTH, PREVIOUS, NPOINTS, LTYPE
  temp <- readBin(con, what="int", 3, size=2, endian="little") 
   
        LENGTH <- temp[1]
        PREVIOUS <- temp[2]
        NPOINTS <- temp[3]
          
   
  ##LTYPE    
  LTYPE<-readBin(con, what="int", 1, size=1, endian="little")

  ##LOW, HIGH, RATE
  temp <- readBin(con, what="double", 3, size=4, endian="little")
      
      LOW <- temp[1]
      HIGH <- temp[2]
      RATE <- temp[3]

      
  TEMPERATURE<-readBin(con, what="integer", 1, size=2, endian="little")
      
  ##XCOORD, YCOORD, TOLDELAY, TOLON, TOLOFF
  temp <- readBin(con, what="integer", 5, size=2, endian="little")
      
      XCOORD <- temp[1]
      YCOORD <- temp[2]
      TOLDELAY <- temp[3]
      TOLON <- temp[4]
      TOLOFF <- temp[5]
      
  
  ##POSITION
  POSITION<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##RUN    
  RUN<-readBin(con, what="int", 1, size=1, endian="little")
    
  ##TIME
  TIME_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
   
    ##time size corrections for wrong time formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007
    TIME_SIZE<-6
    TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)
 
   
  ##DATE
  DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
        
    ##date size corrections for wrong date formats; set n to 6 for all values 
    ##accoording the handbook of Geoff Duller, 2007  
    DATE_SIZE<-6      
    DATE<-readChar(con, DATE_SIZE, useBytes=TRUE)
     
     
  ##SEQUENCE
  SEQUENCE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SEQUENCE<-readChar(con, SEQUENCE_SIZE, useBytes=TRUE)
  
     #step forward in con
     if(8-SEQUENCE_SIZE>0){
      STEPPING<-readBin(con, what="raw", (8-c(SEQUENCE_SIZE)),size=1, endian="little")
      }
   
      
  ##USER
  USER_SIZE<-readBin(con, what="int", 1, size=1, endian="little")   
  USER<-readChar(con, USER_SIZE, useBytes=FALSE)
      
    #step forward in con
    if(8-c(USER_SIZE)>0){
      STEPPING<-readBin(con, what="raw", (8-c(USER_SIZE)), size=1, endian="little")
    }
    
  ##DTYPE
  DTYPE<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##IRR_TIME
  IRR_TIME<-readBin(con, what="double", 1, size=4, endian="little") 
      
  ##IRR_TYPE    
  IRR_TYPE<-readBin(con, what="int", 1, size=1, endian="little")
    
  ##IRR_UNIT    
  IRR_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##BL_TIME    
  BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")
      
  ##BL_UNIT    
  BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##AN_TEMP, AN_TIME, NORM1, NORM2, NORM3, BG
  temp <- readBin(con, what="double", 6, size=4, endian="little")  
      
      AN_TEMP <- temp[1]
      AN_TIME <- temp[2]
      NORM1 <- temp[3]
      NORM2 <- temp[4]
      NORM3 <- temp[5]
      BG <- temp[6]
              
  ##SHIFT    
  SHIFT<-readBin(con, what="integer", 1, size=2, endian="little")

  ##SAMPLE
  SAMPLE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SAMPLE<-readChar(con, SAMPLE_SIZE, useBytes=TRUE) #however it should be set to 20
          
    #step forward in con
    if(20-c(SAMPLE_SIZE)>0){
     STEPPING<-readBin(con, what="raw", (20-c(SAMPLE_SIZE)), size=1, endian="little")
    }
      
  ##COMMENT
  COMMENT_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  COMMENT<-readChar(con, COMMENT_SIZE, useBytes=TRUE) #set to 80 (manual)
  
    #step forward in con
    if(80-c(COMMENT_SIZE)>0){
     STEPPING<-readBin(con, what="raw", (80-c(COMMENT_SIZE)), size=1, endian="little")
    }
      
  ##LIGHTSOURCE, SET, TAG
  temp <- readBin(con, what="int", 3, size=1, endian="little")
      
      LIGHTSOURCE <- temp[1]
      SET <- temp[2]
      TAG <- temp[3]
            
    
  ##GRAIN    
  GRAIN<-readBin(con, what="integer", 1, size=2, endian="little")
  
  ##LPOWER    
  LPOWER<-readBin(con, what="double", 1, size=4, endian="little")  

  ##SYSTEMID    
  SYSTEMID<-readBin(con, what="integer", 1, size=2, endian="little")
  
  ##RESERVED    
  RESERVED<-readBin(con, what="raw", 54, size=1, endian="little")

  #DPOINTS
  DPOINTS<-readBin(con, what="integer", NPOINTS, size=4, endian="little")
      
  #SET UNIQUE ID
  ID<-ID+1    
  
  ##update progress bar
  if(txtProgressBar==TRUE){
    setTxtProgressBar(pb, seek(con,origin="current"))
  }
      
  ##set data.frame for output or append data on data.frame    
  if(exists("results")==FALSE) {
    results<-data.frame(ID=ID,
                        SEL=TRUE,
                        VERSION=VERSION,
                        LENGTH=LENGTH,
                        PREVIOUS=PREVIOUS,
                        NPOINTS=NPOINTS,
                        LTYPE=LTYPE,
                        LOW=LOW,
                        HIGH=HIGH,
                        RATE=RATE,
                        TEMPERATURE=TEMPERATURE,
                        XCOORD=XCOORD,
                        YCOORD=YCOORD,
                        TOLDELAY=TOLDELAY,
                        TOLON=TOLON,
                        TOLOFF=TOLOFF,
                        POSITION=POSITION,
                        RUN=RUN,
                        TIME=TIME,
                        DATE=DATE,
                        SEQUENCE=SEQUENCE,
                        USER=USER,
                        DTYPE=DTYPE,
                        IRR_TIME=IRR_TIME,
                        IRR_TYPE=IRR_TYPE,
                        IRR_UNIT=IRR_UNIT,
                        BL_TIME=BL_TIME,
                        BL_UNIT=BL_UNIT,
                        AN_TEMP=AN_TEMP,
                        AN_TIME=AN_TIME,
                        NORM1=NORM1,
                        NORM2=NORM2,
                        NORM3=NORM3,
                        BG=BG,
                        SHIFT=SHIFT,
                        SAMPLE=SAMPLE,
                        COMMENT=COMMENT,
                        LIGHTSOURCE=LIGHTSOURCE,
                        SET=SET,
                        TAG=TAG,
                        GRAIN=GRAIN,
                        LPOWER=LPOWER,
                        SYSTEMID=SYSTEMID
                      ) #end set data.frame
                      
                      #set variable for DPOINTS handling
                      DATA<-list(DPOINTS)
  }else{
  temp<-data.frame(ID=ID,
                   SEL=TRUE,
                   VERSION=VERSION,
                   LENGTH=LENGTH,
                   PREVIOUS=PREVIOUS,
                   NPOINTS=NPOINTS,
                   LTYPE=LTYPE,
                   LOW=LOW,
                   HIGH=HIGH,
                   RATE=RATE,
                   TEMPERATURE=TEMPERATURE,
                   XCOORD=XCOORD,
                   YCOORD=YCOORD,
                   TOLDELAY=TOLDELAY,
                   TOLON=TOLON,
                   TOLOFF=TOLOFF,
                   POSITION=POSITION,
                   RUN=RUN,
                   TIME=TIME,
                   DATE=DATE,
                   SEQUENCE=SEQUENCE,
                   USER=USER,
                   DTYPE=DTYPE,
                   IRR_TIME=IRR_TIME,
                   IRR_TYPE=IRR_TYPE,
                   IRR_UNIT=IRR_UNIT,
                   BL_TIME=BL_TIME,
                   BL_UNIT=BL_UNIT,
                   AN_TEMP=AN_TEMP,
                   AN_TIME=AN_TIME,
                   NORM1=NORM1,
                   NORM2=NORM2,
                   NORM3=NORM3,
                   BG=BG,
                   SHIFT=SHIFT,
                   SAMPLE=SAMPLE,
                   COMMENT=COMMENT,
                   LIGHTSOURCE=LIGHTSOURCE,
                   SET=SET,
                   TAG=TAG,
                   GRAIN=GRAIN,
                   LPOWER=LPOWER,
                   SYSTEMID=SYSTEMID
                                    
                                           
                   
                   )
                
                #combine DPOINT values and the rest of the data
                DATA<-c(DATA,list(DPOINTS))
                results<-rbind(results,temp)
  }#end else  
  
  ##BREAK    
  ##stop loop if record limit is reached  
  if(missing(n.records)==FALSE){
    
    if(n.records==ID){break()}
    
  }  
      
}#endwhile::end lopp 


##close con 
close(con)

##close
if(txtProgressBar==TRUE){close(pb)}

##output
cat(paste("\t >> ",ID,"records have been read successfully!\n\n",paste=""))

##produce S4 object for output
object <- new("Risoe.BINfileData",
                    METADATA=results,
                    DATA=DATA
                    )

##============================================================================##
# Convert Translation Matrix Values ---------------------------------------


if(show.raw.values == FALSE) {
##LTYPE
object@METADATA[,"LTYPE"]<- sapply(1:length(object@METADATA[,"LTYPE"]),function(x){
   
  as.character(LTYPE.TranslationMatrix[object@METADATA[x,"LTYPE"]==LTYPE.TranslationMatrix[,1],2])
  
})

##TIME CONVERSION

object@METADATA[,"TIME"]<- sapply(1:length(object@METADATA[,"TIME"]),function(x){

  format(strptime(as.character(object@METADATA[x,"TIME"]),"%H%M%S"),"%H:%M:%S")   

})

##DTYPE CONVERSION
object@METADATA[,"DTYPE"]<- sapply(1:length(object@METADATA[,"DTYPE"]),function(x){
  
  as.character(DTYPE.TranslationMatrix[object@METADATA[x,"DTYPE"]==DTYPE.TranslationMatrix[,1],2])
  
})

##LIGHTSOURCE CONVERSION
object@METADATA[,"LIGHTSOURCE"]<- sapply(1:length(object@METADATA[,"LIGHTSOURCE"]),function(x){
  
  as.character(LIGHTSOURCE.TranslationMatrix[object@METADATA[x,"LIGHTSOURCE"]==LIGHTSOURCE.TranslationMatrix[,1],2])
  
})
}

##return values
return(object)
}

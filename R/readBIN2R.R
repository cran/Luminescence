##//////////////////////////////////////////////
##//readBIN2R.R
##/////////////////////////////////////////////
##======================================
#author: Sebastian Kreutzer
#organisation: JLU Giessen
#vers.: 0.3.1
#date: 10/12/2012
##======================================
##

readBIN2R <- function(file){

##set translation matrix for values
LTYPE.TranslationMatrix<-c("TL","OSL","IRSL","M-IR","M-VIS","TOL","PULSED", "RIR", "RBR","USER")
DTYPE.TranslationMatrix<-c("Natural","N+dose","Bleach","Bleach+dose","Natural (Bleach)","N+dose (Bleach)","Dose","Background")  
LIGHTSOURCE.TranslationMatrix<-c("None","Lamp","IR diodes/IR Laser","Calibration LED","Blue Diodes")  
  
#open connection
con<-file(file, "rb")

   ##get information about file size
   file.size<-file.info(file)

   ##output
   cat(paste("\n[readBIN2R.R]\n\t >> ",file,sep=""), fill=TRUE)
    
   ##set progressbar
   pb<-txtProgressBar(min=0,max=file.size$size, char="=", style=3)

##read data up to the end of con

##set ID
ID<-0

##start loop for import BIN data
while(length(VERSION<-readBin(con, what="raw", 1, size=1, endian="litte"))>0) {
  
      #empty byte position
      EMPTY<-readBin(con, what="raw", 1, size=1, endian="litte")
 
  LENGTH<-readBin(con, what="int", 1, size=2, endian="little")
  PREVIOUS<-readBin(con, what="int", 1, size=2, endian="little")
  NPOINTS<-readBin(con, what="int", 1, size=2, endian="little")
  
  #LTYPE    
  LTYPE<-readBin(con, what="int", 1, size=1, endian="little")
      
      #translate values in character 
      LTYPE<-LTYPE.TranslationMatrix[LTYPE+1]
      
  LOW<-readBin(con, what="double", 1, size=4, endian="little")
  HIGH<-readBin(con, what="double", 1, size=4, endian="little")
  RATE<-readBin(con, what="double", 1, size=4, endian="little")
  TEMPERATURE<-readBin(con, what="integer", 1, size=2, endian="little")
  XCOORD<-readBin(con, what="integer", 1, size=2, endian="little")
  YCOORD<-readBin(con, what="integer", 1, size=2, endian="little")
  TOLDELAY<-readBin(con, what="integer", 1, size=2, endian="little")
  TOLON<-readBin(con, what="integer", 1, size=2, endian="little")
  TOLOFF<-readBin(con, what="integer", 1, size=2, endian="little")
  POSITION<-readBin(con, what="int", 1, size=1, endian="little")
  RUN<-readBin(con, what="int", 1, size=1, endian="little")

  #TIME
  TIME_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  
  ##time size corrections for wrong time formats; set n to 6 for all values 
  ##accoording the handbook of Geoff Duller, 2007
  TIME_SIZE<-6
  
  TIME<-readChar(con, TIME_SIZE, useBytes=TRUE)
  TIME<-format(strptime(as.character(TIME),"%H%M%S"),"%H:%M:%S")    
       
  #DATE
  DATE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
      
  ##date size corrections for wrong date formats; set n to 6 for all values 
  ##accoording the handbook of Geoff Duller, 2007  
  DATE_SIZE<-6    
      
  DATE<-readChar(con, DATE_SIZE, useBytes=TRUE)
        
  #DATE<-as.Date(as.character(DATE),"%d%m%y")   #no data conversion due to missing year value

  #SEQUENCE
  SEQUENCE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SEQUENCE<-readChar(con, SEQUENCE_SIZE, useBytes=TRUE)
    
     #step forward in con
     STEPPING<-readBin(con, what="raw", (8-c(SEQUENCE_SIZE)),size=1, endian="little")
     
     
  #USER
  USER_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  USER<-readChar(con, USER_SIZE, useBytes=FALSE)

    #step forward in con
    STEPPING<-readBin(con, what="raw", (8-c(USER_SIZE)), size=1, endian="little")
    
    
  #DTYPE
  DTYPE<-readBin(con, what="int", 1, size=1, endian="little")
      
      #translate values in character 
      DTYPE<-DTYPE.TranslationMatrix[DTYPE+1]
      

  IRR_TIME<-readBin(con, what="double", 1, size=4, endian="little")  
  IRR_TYPE<-readBin(con, what="int", 1, size=1, endian="little")
  IRR_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
  BL_TIME<-readBin(con, what="double", 1, size=4, endian="little")
  BL_UNIT<-readBin(con, what="int", 1, size=1, endian="little")
  AN_TEMP<-readBin(con, what="double", 1, size=4, endian="little")
  AN_TIME<-readBin(con, what="double", 1, size=4, endian="little")

  NORM1<-readBin(con, what="double", 1, size=4, endian="little")
  NORM2<-readBin(con, what="double", 1, size=4, endian="little")
  NORM3<-readBin(con, what="double", 1, size=4, endian="little")

  BG<-readBin(con, what="double", 1, size=4, endian="little")
  SHIFT<-readBin(con, what="integer", 1, size=2, endian="little")

  #SAMPLE
  SAMPLE_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  SAMPLE<-readChar(con, SAMPLE_SIZE, useBytes=TRUE) #however it should be set to 20
      
    #step forward in con
    STEPPING<-readBin(con, what="raw", (20-c(SAMPLE_SIZE)), size=1, endian="little")

  #COMMENT
  COMMENT_SIZE<-readBin(con, what="int", 1, size=1, endian="little")
  COMMENT<-readChar(con, COMMENT_SIZE, useBytes=TRUE) #set to 80 (manual)
  
    #step forward in con
     STEPPING<-readBin(con, what="raw", (80-c(COMMENT_SIZE)), size=1, endian="little")
  
  #LIGHTSOURCE    
  LIGHTSOURCE<-readBin(con, what="int", 1, size=1, endian="little")
      
      #translate values in character 
      LIGHTSOURCE<-LIGHTSOURCE.TranslationMatrix[LIGHTSOURCE+1]
      
  SET<-readBin(con, what="int", 1, size=1, endian="little")
  TAG<-readBin(con, what="int", 1, size=1, endian="little")
  GRAIN<-readBin(con, what="integer", 1, size=2, endian="little")
  LPOWER<-readBin(con, what="double", 1, size=4, endian="little")  
  
  SYSTEMID<-readBin(con, what="integer", 1, size=2, endian="little")

  RESERVED<-readBin(con, what="raw", 54, size=1, endian="little")
  
  #DPOINTS
  DPOINTS<-readBin(con, what="integer", NPOINTS, size=4, endian="little")
  
  #SET UNIQUE ID
  ID<-ID+1    
  
  ##updata progress bar
  setTxtProgressBar(pb, seek(con,origin="current"))

      
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
}#endwhile::end lopp 

##close con 
close(con)

##close
close(pb)

##output
cat(paste("\t >> ",ID,"records have been read successfully!\n\n",paste=""))

##produce S4 object for output
output.BINdata<-new("Risoe.BINfileData",
                    METADATA=results,
                    DATA=DATA
                    )


##return values
return(output.BINdata)
}

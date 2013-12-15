plot_RLum.Data.Spectrum<- structure(function(#Plot function for an RLum.Data.Spectrum S4 class object
  ### The function provides a standardized plot output for spectrum data of an 
  ### RLum.Data.Spectrum S4 class object 
  
  # ===========================================================================
  ##author<<
  ## Sebastian Kreutzer, JLU Giessen (Germany)
  
  ##section<<
  ## version 0.1 [2013-11-14]
  # ===========================================================================

  object, 
  ### \code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): 
  ### S4 object of class \code{RLum.Data.Spectrum}
  
  par.local = TRUE,
  ### \code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
  ### the plot is shown in one column and one row. If \code{par.local = FALSE} 
  ### global parameters are inherited.
  
  plot.type = "contour",
  ### \code{\link{character}} (with default): plot type, for 3D-plot use \code{persp}, 
  ### or \code{persp3d} and \code{contour} for a 2D-plot.\cr
  ###
  ### Note: The use of \code{persp3d} will produce a dynamic 3D surface plot on the
  ### screen.
  
  optical.wavelength.colours = TRUE,
  ### \code{\link{logical}} (with default): use optical wavelength colour palette.
  ### Note: For this, the spectrum range is limited: \code{c(350,750)}. 
  ### Own colours can be set with the argument \code{col}.
  
  ...
  ### further arguments and graphical parameters that will be passed to the 
  ### \code{plot} function.
){
  
  # Integrity check -----------------------------------------------------------
  
  ##check if object is of class RLum.Data.Spectrum
  if(class(object) != "RLum.Data.Spectrum"){
    
    stop("[plot_RLum.Data.Spectrum]: Input object is not of type RLum.Data.Spectrum")
    
  }
  
  ##XSYG
  ##check for curveDescripter
  if("curveDescripter" %in% names(object@info) == TRUE){
    
    temp.lab <- strsplit(object@info$curveDescripter, split = ";")[[1]]

    xlab <- temp.lab[2]
    ylab <- temp.lab[1]
    zlab <- temp.lab[3]
    
  }
  
  ##deal with addition arguments 
  extraArgs <- list(...) 
  
  main <- if("main" %in% names(extraArgs)) {extraArgs$main} else 
  {"RLum.Data.Spectrum"}
  
  xlab <- if("xlab" %in% names(extraArgs)) {extraArgs$xlab} else 
  {xlab}
  
  ylab <- if("ylab" %in% names(extraArgs)) {extraArgs$ylab} else 
  {ylab}
  
  zlab <- if("zlab" %in% names(extraArgs)) {extraArgs$zlab} else 
  {zlab}

  xlim <- if("xlim" %in% names(extraArgs)) {extraArgs$xlim} else 
  {c(min(as.numeric(rownames(object@data))), 
     max(as.numeric(rownames(object@data))))}
    
  ylim <- if("ylim" %in% names(extraArgs)) {extraArgs$ylim} else 
  {c(min(as.numeric(colnames(object@data))), 
     max(as.numeric(colnames(object@data))))}
  
  mtext <- if("mtext" %in% names(extraArgs)) {extraArgs$mtext} else 
  {""}
  
  cex <- if("cex" %in% names(extraArgs)) {extraArgs$cex} else 
  {1}
  
  phi <- if("phi" %in% names(extraArgs)) {extraArgs$phi} else 
  {30}
  
  theta <- if("theta" %in% names(extraArgs)) {extraArgs$theta} else 
  {30}
  
  shade <- if("shade" %in% names(extraArgs)) {extraArgs$shade} else 
  {0.4}
  
  expand <- if("expand" %in% names(extraArgs)) {extraArgs$expand} else 
  {1}
  
  ticktype <- if("ticktype" %in% names(extraArgs)) {extraArgs$ticktype} else 
  {"detailed"}
  
  log<- if("log" %in% names(extraArgs)) {extraArgs$log} else 
  {""}
  
  
  # prepare values for plot ---------------------------------------------------
  
  ##limit values for wavelength color 
  if(optical.wavelength.colours == TRUE){
    
    xlim <- c(300,750)
    
  }
  
  temp.xyz <- as(object, "matrix")
  
  ##reduce for xlim 
  temp.xyz <- temp.xyz[as.numeric(rownames(temp.xyz)) >= xlim[1] & 
                       as.numeric(rownames(temp.xyz)) <= xlim[2],]
  
  ##reduce for ylim 
  temp.xyz <- temp.xyz[, as.numeric(colnames(temp.xyz)) >= ylim[1] & 
                         as.numeric(colnames(temp.xyz)) <= ylim[2]]
  
  
  ## wavelength
  x <- as.numeric(rownames(temp.xyz))
  
  ## time/temp
  y <- as.numeric(colnames(temp.xyz))
  
  
  # set color values --------------------------------------------------------
  
  col <- "grey"
  
  if(optical.wavelength.colours == TRUE){
    
    col.violet <- c(300,450)
    col.blue <- c(450,495)
    col.green <- c(495,570)
    col.yellow <- c(570,590)
    col.orange <- c(590,620)
    col.red <- c(620,750)
    
    #set colour pallett 
    col <- unlist(sapply(1:length(x), function(i){
      
      if(x[i] >= col.violet[1] & x[i] < col.violet[2]){"violet"}
      else if(x[i] >= col.blue[1] & x[i] < col.blue[2]){"blue"}
      else if(x[i] >= col.green[1] & x[i] < col.green[2]){"green"}
      else if(x[i] >= col.yellow[1] & x[i] < col.yellow[2]){"yellow"}
      else if(x[i] >= col.orange[1] & x[i] < col.orange[2]){"orange"}
      else if(x[i] >= col.red[1]){"red"}
      
    }))
  }
  
  if("col" %in% names(extraArgs)) {
    
    col <- extraArgs$col
    
  }
  
  
  # Do log scaling if needed -------------------------------------------------
  
  ##x
  if(grepl("x", log)==TRUE){x <- log10(x)}
    
  ##y
  if(grepl("y", log)==TRUE){y <- log10(y)}
  
  ##z
  if(grepl("z", log)==TRUE){temp.xyz <- log10(temp.xyz)}
    
  
  # PLOT --------------------------------------------------------------------
  
  ##par setting for possible combination with plot method for RLum.Analysis objects
  if(par.local == TRUE){par(mfrow=c(1,1), cex = cex)}
  
  if(plot.type == "persp3d"){
    
  ## ==========================================================================#
  ##perspective plot 3D screen (package rgl)
  ## ==========================================================================#
    persp3d(x, y, temp.xyz, 
            xlab = xlab,
            ylab = ylab,
            zlab = zlab,
            col = col,
            main = main)
    

  }else if(plot.type == "persp"){
  ## ==========================================================================#
  ##perspective plot
  ## ==========================================================================#
  persp(x, y, temp.xyz, 
        shade = shade,
        phi = phi,
        theta = theta, 
        xlab = xlab,
        ylab = ylab,
        zlab = zlab,
        scale = TRUE,
        col = col,
        main = main, 
        expand = expand, 
        ticktype = ticktype)
  
  
    ##plot additional mtext
    mtext(mtext, side = 3, cex = cex*0.8)  

    
   }else if(plot.type == "contour") {
   ## ==========================================================================#
   ##contour plot
   ## ==========================================================================#
   contour(x,y,temp.xyz,
           xlab = xlab,
           ylab = ylab,
           main = main,
           col = "black")
   
   ##plot additional mtext
   mtext(mtext, side = 3, cex = cex*0.8)  
  
   }else{
     
     stop("[plot_RLum.Data.Spectrum] Error: Unknown plot type.")
     
   }

  
  # DOCUMENTATION - INLINEDOC LINES -----------------------------------------
  
  ##details<<
  ## Spectrum is visualized as 3D or 2D plot. Both plot types are based on internal
  ## R plot functions. \cr
  ##
  ## Arguments that will be passed to \code{\link{persp}}:
  ## \itemize{ 
  ## \item \code{shade}: default is \code{0.4}  
  ## \item \code{phi}: default is \code{30}
  ## \item \code{theta}: default is \code{30}
  ## \item \code{expand}: default is \code{1}
  ## \item \code{ticktype}: default is \code{detailed}
  ## } 
  ## 
  ## \bold{Further arguments that will be passed}\cr
  ## \code{xlab}, \code{ylab}, \code{zlab}, \code{xlim}, \code{ylim}, \code{main}, 
  ## \code{mtext}
                                  
  
  ##value<<
  ## Returns a plot.
  
  ##references<<
  ## #
  
  ##note<<
  ## Not all arguments of \code{\link{plot}} will be passed!
  
  ##seealso<<
  ## \code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{persp}}, 
  ## \code{\link{persp3d}}, \code{\link{contour}}
  
  ##keyword<<
  ## aplot
  
}, ex=function(){
  
 ##(1) simple call (note: currently no example data set is provided!)
 # plot_RLum.Data.Spectrum(Example.data)  
  
})#END OF STRUCTURE
\name{plot_BINfileData}
\alias{plot_BINfileData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot single luminescence curves from a BIN file object.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot single luminescence curves from an object returned by the \link{readBIN2R} function.

}
\usage{
plot_BINfileData(BINfileData, position, run, set,
                 sorter = "POSITION", 
                 ltype = c("IRSL","OSL","TL","RIR","RBR"),
                 curve.transformation,    
                 
                 dose_rate,
                 temp.lab = "deg. C", 
                 cex.global = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{BINfileData}{\link{Risoe.BINfileData-class} (\bold{required}): requires an S4 object returned by the 
    \link{readBIN2R} function.}
    \item{position}{\link{vector} (optional): option to limit the plotted curves by position (e.g. position = 1, position = c(1,3,5)).}
    \item{run}{\link{vector} (optional): option to limit the plotted curves by run (e.g. run = 1, set = c(1,3,5)).}
    \item{set}{\link{vector} (optional): option to limit the plotted curves by set (e.g. run = 1, set = c(1,3,5)).}
    \item{sorter}{\link{character} (with default): the plot output can be ordered by "POSITION","SET" or "RUN". POSITION, SET and RUN are options defined in the Risoe Sequence Editor.}
    \item{ltype}{\link{character} (with default):  option to limit the plotted curves by the type of luminescence stimulation. 
    Allowed values: "IRSL","OSL","TL","RIR" and "RBR". All type of curves are plotted by default.}
    \item{curve.transformation}{\link{character} (optional): allows to transform CW-OSL and CW-IRSL curves
    to be transformed to pseudo-LM curves via transformation functions. Allowed values are: 
    \code{CW2pLM}, \code{CW2pLMi}, \code{CW2pHMi} and \code{CW2pPMi}. See Details.}
  
    \item{dose_rate}{\link{numeric} (optional): dose rate of the irradition source at the measurement date. If set, the given irradiation dose will be shown in Gy. See details.}
      \item{temp.lab}{\link{character} (optional): option to plot a circle (for degree) before the C (Celsius) instead of 'deg.'.
      This argument is just a technical issue (s. details).}
    
    \item{cex.global}{\link{numeric} (with default): global scaling factor.}
 
    
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
\bold{Nomenclature}\cr

The nomenclature used for the function (e.g. ltype, position) are taken from the Analyst manual (Duller, 2007, p. 42): \cr

\tabular{rlll}{
[,1] \tab ID \tab: Unique record ID (same ID as in slot \code{DATA}) \tab \code{numeric} \cr
[,2] \tab SEL \tab: Record selection \tab \code{logical} \cr
[,3] \tab VERSION \tab: Data format version number \tab \code{raw} \cr
[,4] \tab LENGTH  \tab: Length of this record \tab \code{integer} \cr
[,5] \tab PREVIOUS \tab: Length of previous record \tab \code{integer} \cr
[,6] \tab NPOINTS \tab: Number of data points in the record \tab \code{integer} \cr
[,7] \tab LTYPE \tab: Luminescence type \tab \code{factor} \cr
[,8] \tab LOW \tab: Low (temperature, time, wavelength) \tab \code{numeric} \cr
[,10] \tab HIGH \tab: High (temperature, time, wavelength) \tab \code{numeric} \cr
[,11] \tab RATE \tab: Rate (heating rate, scan rate) \tab \code{numeric} \cr
[,12] \tab TEMPERATURE \tab: Sample temperature \tab \code{integer} \cr
[,13] \tab XCOORD \tab: X position of a single grain \tab \code{integer} \cr
[,14] \tab YCOORD \tab: Y position of a single grain \tab \code{integer} \cr
[,15] \tab TOLDELAY \tab: TOL 'delay' channels \tab \code{integer} \cr
[,16] \tab TOLON  \tab: TOL 'on' channels \tab \code{integer} \cr
[,17] \tab TOLOFF  \tab: TOL 'off' channels \tab \code{integer} \cr
[,18] \tab POSITION \tab: Carousel position \tab \code{integer} \cr
[,19] \tab RUN \tab: Run number \tab \code{integer} \cr
[,20] \tab TIME \tab: Data collection time (hh-mm-ss) \tab \code{factor} \cr
[,21] \tab DATA \tab: Data collection date (dd-mm-yy) \tab \code{factor} \cr
[,22] \tab SEQUENCE  \tab: Sequence name \tab \code{factor} \cr
[,23] \tab USER  \tab: User name \tab \code{factor} \cr
[,24] \tab DTYPE  \tab: Data type \tab \code{factor} \cr
[,25] \tab IRR_TIME \tab: Irradiation time \tab \code{numeric} \cr
[,26] \tab IRR_TYPE  \tab: Irradiation type (alpha, beta or gamma) \tab \code{integer} \cr
[,27] \tab IRR_UNIT  \tab: Irradiation unit (Gy, Rads, secs, mins, hrs) \tab \code{integer} \cr
[,28] \tab BL_TIME  \tab: Bleaching time \tab \code{numeric} \cr
[,29] \tab BL_UNIT  \tab: Bleaching unit (mJ, J, secs, mins, hrs) \tab \code{integer} \cr
[,30] \tab AN_TEMP  \tab: Annealing temperature \tab \code{numeric} \cr
[,31] \tab AN_TIME  \tab: Annealing time \tab \code{numeric} \cr
[,32] \tab NORM1  \tab: Normalisation factor (1) \tab \code{numeric} \cr
[,33] \tab NORM2  \tab: Normalisation factor (2) \tab \code{numeric} \cr
[,34] \tab NORM3  \tab: Normalisation factor (3) \tab \code{numeric} \cr
[,35] \tab BG \tab: Background level \tab \code{numeric} \cr
[,36] \tab SHIFT \tab: Number of channels to shift data \tab \code{integer} \cr
[,37] \tab SAMPLE \tab: Sample name \tab \code{factor} \cr
[,38] \tab COMMENT \tab: Comment \tab \code{factor} \cr
[,39] \tab LIGHTSOURCE \tab: Light source \tab \code{factor} \cr
[,40] \tab SET \tab: Set Number \tab \code{integer} \cr
[,41] \tab TAG \tab: Tag \tab \code{integer} \cr
[,42] \tab GRAIN \tab: Grain number \tab \code{integer} \cr
[,43] \tab LPOWER \tab: Optical Stimulation Power \tab \code{numeric} \cr
[,44] \tab SYSTEMID \tab: System ID \tab \code{integer} 
}

\bold{curve.transformation}\cr

This argument allows to transform continuous wave (CW) curves to pseudo (linear)
modulated curves. For the transformation the functions of the package are used. 
Currently it is not possible to pass further arguments to the transformation functions.
The argument works only for ltype \code{OSL} and \code{IRSL}.

\bold{Irradiation time}\cr

Plotting the irradiation time (s) or the given dose (Gy) requires that the variable \code{IRR_TIME} has been set within the BIN-file. This is normally done by using the 'Run Info' option within the Sequence Editor or by 
editing in R.\cr

\bold{Circle instead of 'deg.'}\cr

Unfortunately, the circle (for degree) is not an ASCII character and therefore a warning is produced during compiling the R-package. To avoid the warning message the default scale unit is 'deg. C'. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a plot.
}
\references{
%% ~put references to the literature/web site here ~
Duller, G., 2007. Analyst. pp. 1-45. 

}
\author{Sebastian Kreutzer, JLU Giessen (Germany), 
Michael Dietze, TU Dresden (Germany)}
\note{
%%  ~~further notes~~

The function has been successfully tested for the Sequence Editor file output version 3 and 4. 
}

\section{Version}{0.4 [2013-03-05]}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{readBIN2R}}, \code{\link{CW2pLM}}, \code{\link{CW2pLMi}}, \code{\link{CW2pPMi}},
\code{\link{CW2pHMi}}

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##load data
data(ExampleData.BINfileData)

##plot all curves from the first position to the desktop
#pdf(file = "~/Desktop/CurveOutput.pdf", paper = "a4", height = 11, onefile = TRUE)
  
  ##example - load from *.bin file
  #BINfile<-"[your path]"
  #BINfileData<-readBIN2R(BINfile)

#par(mfrow = c(4,3), oma = c(0.5,1,0.5,1))
#plot_BINfileData(CWOSL.SAR.Data,position = 1)
#mtext(side = 4, BINfile, outer = TRUE, col = "blue", cex = .7)
#dev.off()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}

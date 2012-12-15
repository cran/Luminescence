\name{readBIN2R}
\alias{readBIN2R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import Risoe BIN-file into R}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Import a *.bin file produced by a Risoe DA15 and DA20 TL/OSL reader into R.
}
\usage{
readBIN2R(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
  \link{character} (\bold{required}): the bin-file name (including path), e.g. \cr
  [WIN]: \code{readBIN2R("C:/Desktop/test.bin")}, \cr  
  [MAC/LINUX]: \code{readBIN2R("/User/test/Desktop/test.bin")}
} 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The binary data file is parsed byte per byte following the data structure published in the Appendices of 
the Analyst manual p. 42. 
\cr

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

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns an S4 \link{Risoe.BINfileData-class} object containing two slots:

\item{METADATA}{A \link{data.frame} containing all variables stored in the bin-file.}
\item{DATA}{A \link{list} containing a numeric \link{vector} of the measured data. The ID corresponds to the record ID in METADATA}

}
\references{
%% ~put references to the literature/web site here ~
Duller, G., 2007. Analyst. 
}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
%%  ~~further notes~~
Function has been successfully tested for BIN-file version 3 and 4. The version number depends on used the Sequence Editor. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{readBin}}
}
\examples{
##readBIN2R("~/Desktop/Example.bin")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}

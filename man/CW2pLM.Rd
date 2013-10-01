\name{CW2pLM}
\alias{CW2pLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform a CW-OSL curve into a pLM-OSL curve}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Transforms a conventionally measured continuous-wave (CW) curve into a pseudo linearly modulated (pLM)
curve using the equations given in Bulur (2000). 
}
\usage{CW2pLM(values)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{\code{\linkS4class{RLum.Data.Curve}} or \code{\link{data.frame}} (\bold{required}):
  \code{RLum.Data.Curve} data object. Alternatively, a \code{data.frame} of the 
  measured curve data of type stimulation time (t) (\code{values[,1]}) and measured counts (cts) (\code{values[,2]}) can be provided.}
  

}
\details{
%%  ~~ If necessary, more details than the description above ~~
According to Bulur (2000) the curve data are transformed by introducing two 
new parameters P (stimulation period) and u (transformed time):

 \deqn{P=2*max(t)}
 \deqn{u=\sqrt{(2*t*P)}}

The new count values are then calculated by
  \deqn{ctsNEW = cts(u/P)}

and the returned \code{data.frame} is produced by: \code{data.frame(u,ctsNEW)}

}
\value{
The function returns the same data type as the input data type with the
transformed curve values.

\item{\code{\link{data.frame}}}{generic R data structure}
\item{\code{\linkS4class{RLum.Data.Curve}}}{package \code{\linkS4class{RLum} object}}
}
\references{
%% ~put references to the literature/web site here ~
Bulur, E., 2000. A simple transformation for converting CW-OSL curves to LM-OSL curves. Radiation Measurements, 32, 141-145. 
}
\author{Sebastian Kreutzer, JLU Giessen (Germany)}

\section{Version}{0.4 [2013-03-27]}

\note{
%%  ~~further notes~~
The transformation is recommended for curves recorded with a channel resolution of at least 0.05 s/channel.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\link{CW2pLM}\cr

The output of the function can be further used for LM-OSL fitting: 

\code{\link{CW2pLMi}}, \code{\link{CW2pHMi}}, \code{\link{CW2pPMi}}, \code{\link{fit_LMCurve}},
\code{\linkS4class{RLum.Data.Curve}}, \code{\link{plot_RLum}}

}
\examples{
##read curve from CWOSL.SAR.Data transform curve and plot values
data(ExampleData.BINfileData, envir = environment())

##read id for the 1st OSL curve
id.OSL <- CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"LTYPE"] == "OSL","ID"]

##produce x and y (time and count data for the data set)
x<-seq(CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"],
       CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"],
       by = CWOSL.SAR.Data@METADATA[id.OSL[1],"HIGH"]/CWOSL.SAR.Data@METADATA[id.OSL[1],"NPOINTS"])
y <- unlist(CWOSL.SAR.Data@DATA[id.OSL[1]])
values <- data.frame(x,y)

##transform values
values.transformed <- CW2pLM(values)

##plot
plot(values.transformed)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

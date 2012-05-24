\name{CW2pLM}
\alias{CW2pLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transforms a CW curve in a pLM curve}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Transform a conventional measured continuous-wave (CW)curve in a pseudo linearly modulated (pLM)
curve using the equations given in Bulur (2000). The transformed 
values may be used for further LM fitting analysis. 
}
\usage{CW2pLM(values)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{\code{\link{data.frame}} (\bold{required}): measured curve data of type stimulation time (t) 
  (\code{values[,1]}) and measured counts (cts) (\code{values[,2]})}
  

}
\details{
%%  ~~ If necessary, more details than the description above ~~
According to Bulur (2000) the curve data are transformed by introducing two 
new parameters P and u:

 \deqn{P=2*max(t)}
 \deqn{u=\sqrt{(2*t*P)}}

The new count values are then calculated by
  \deqn{ctsNEW = cts(u/P)}

And the returned \code{data.frame} is produced by: \code{data.frame(u,ctsNEW)}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a \code{data.frame} of the same form as the input of the transformed curve data.
}
\references{
%% ~put references to the literature/web site here ~
Bulur, E. (2000). A simple transformation for converting CW-OSL curves to LM-OSL curves. Radiation Measurements, 32, 141-145. 
}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
%%  ~~further notes~~
The transformation makes just sense if the channel resolution of the measurement is at least 0.05 s/channel.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
The output of the function can be further used for LM-OSL fitting: 

\code{\link{fit_LMCurve}}
}
\examples{
##(1)
##read curve from BINfileData transform curve an plot values
data(ExampleData.BINfileData)

##read id for the 1st OSL curve
id.OSL<-BINfileData@METADATA[BINfileData@METADATA[,"LTYPE"]=="OSL","ID"]

##produce x and y (time and count data for the data set)
x<-seq(BINfileData@METADATA[id.OSL[1],"HIGH"]/BINfileData@METADATA[id.OSL[1],"NPOINTS"],
       BINfileData@METADATA[id.OSL[1],"HIGH"],
       by=BINfileData@METADATA[id.OSL[1],"HIGH"]/BINfileData@METADATA[id.OSL[1],"NPOINTS"])
y<-unlist(BINfileData@DATA[id.OSL[1]])
values<-data.frame(x,y)

##transform values
values.transformed<-CW2pLM(values)

##plot
plot(values.transformed)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
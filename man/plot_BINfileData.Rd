\name{plot_BINfileData}
\alias{plot_BINfileData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot single luminescence curves from a BIN file object.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot single luminescence curves from an object returned by the \link{readBIN2R} function.

}
\usage{
plot_BINfileData(BINfileData, position, 
                 sorter="POSITION", 
                 ltype=c("IRSL","OSL","TL","RIR","RBR"),
                 dose_rate,
                 cex.global=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{BINfileData}{\link{Risoe.BINfileData-class} (\bold{required}): requires an S4 object returned by the 
    \link{readBIN2R} function.}
    \item{position}{\link{vector} (optional): option to limit the plotted curves by position (e.g. position=1, position=c(1,3,5)).}
    \item{sorter}{\link{character} (with default): the plot output can ordered by "POSITION","SET" or "RUN". POSITION, SET and RUN are options defined in the Risoe Sequence Editor.}
    \item{ltype}{\link{character} (with default):  option to limit the plotted curves by the type of luminescence stimulation. 
    Allowed values: "IRSL","OSL","TL","RIR" and "RBR". All type of curves are plotted by default.}
    \item{dose_rate}{\link{numeric} (optional): dose rate of the irradition source at the measurement date. If set, the given irradiation
    dose will be shown in Gy. See details.}
    \item{cex.global}{\link{numeric} (with default): global scaling factor.}
 
    
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
The nomenclature used for the function (e.g. ltype, position) are taken from the Analyst manual (Duller, 2007). \cr

\bold{irradiation time}\cr

Plotting the irradiation time (s) or the given dose (Gy) requires that the variable IRR_TIME has been set within the 
BIN-file. This is normally done by using the 'Run Info' option within the Sequence Editor.
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
\author{Sebastian Kreutzer, JLU Giessen (Germany), 2012}
\note{
%%  ~~further notes~~

The function has only been tested for the Sequence Editor file output version 3 and 4. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{readBIN2R}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
#uncomment for usage

##load data
#data(ExampleData.BINfileData)

##plot all curves from the first position to the desktop
#pdf(file="~/Desktop/CurveOutput.pdf",paper="a4", height=11, onefile=TRUE)
  
  ##example - load from *.bin file
  #BINfile<-"[your path]"
  #BINfileData<-readBIN2R(BINfile)

#par(mfrow=c(4,3), oma=c(0.5,1,0.5,1))
#plot_BINfileData(BINfileData,position=1)
#mtext(side=4,BINfile,outer=TRUE,col="blue", cex=.7)
#dev.off()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
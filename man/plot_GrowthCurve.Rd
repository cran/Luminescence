\name{plot_GrowthCurve}
\alias{plot_GrowthCurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit and plot a growth curve for luminescence data (Lx/Tx against dose)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A dose response curve for luminescence measurments using a regenerative protocol
is produced. 
}
\usage{
plot_GrowthCurve(sample,main="Growth Curve",mtext="",
                 fit.method="EXP",
                 fit.weights=TRUE,
                 fit.includingRepeatedRegPoints=TRUE,
                 fit.NumberRegPoints,
                 fit.NumberRegPointsReal,
                 NumberIterations.MC=100,xlab="s",
                 output.plot=TRUE,output.plotExtended=TRUE,
                 cex.global=1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{sample}{\link{data.frame} (\bold{required}): data frame with colums for x=Dose,y=LxTx,z=LxTx.Error, y1=TnTx.
                  The column for the test dose response is optional, but requires 'TnTx' as column name if used.}
    \item{main}{\link{character} (with default): header of the plot.}
    \item{mtext}{\link{character} (optional): additional text on the right side of the plot.}
    \item{fit.method}{\link{character} (with default): functions used for fitting. Possible options are:
                      \code{LIN}, \code{EXP}, \code{EXP OR LIN},\code{EXP+LIN} or \code{EXP+EXP}}. See details.
    \item{fit.weights}{\link{logical} (with default): option wether the fitting is done with or without weights. See details.}
    \item{fit.includingRepeatedRegPoints}{\link{logical} (with default): includes repeated points for fitting (TRUE/FALSE)}
    \item{fit.NumberRegPoints}{\link{integer} (optional):  set number of regeneration points manually. By default the number of all(!) regeneration
    points is grepped automatically.}
    \item{fit.NumberRegPointsReal}{\link{integer} (optional): if the number of regeneration points is provided manually 
    the value of the real regenerations points = all points - repeated points - reg 0 have to be inserted. }
    \item{NumberIterations.MC}{\link{integer} (with default): number of Monte Carlo simulation runs for the error estimation. See details.}
    \item{xlab}{\link{character} (with default): unit for the x-axis labeling. Possible values are "Gy" and "s".}
    \item{output.plot}{\link{logical} (with default): plot output (TRUE/FASLE).}
    \item{output.plotExtended}{\link{logical} (with default): If \code{TRUE} 3-plots on one plot area are provided: (1) growth curve, 
    (2) histogram from error Monte Carlo simulation and (3) a test dose response plot. If \code{FALSE} just the growth 
    curve will be plotted. \bold{Requires:} \code{output.plot=TRUE}}
    \item{cex.global}{\link{numeric} (with default): global scale factor.}
    
    
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
\bold{fitting methods} \cr

For all options (except for the \code{LIN} and the \code{EXP OR LIN}) the \link{nls} function with the \code{port} algorithm is used. 

\code{LIN}: fit a linear function to the data using \link{lm}:
            \deqn{y = mx+n} \cr

\code{EXP}: try to fit a function of the form \deqn{y = a*(1-exp(-(x+c)/b))} 
Parameters b and c are approximated by a linear fit using \link{lm}.\cr

\code{EXP OR LIN}: works for some cases where a \code{EXP} fit failes. If the \code{EXP} fit failes a \code{LIN} 
fit is done instead. \cr

\code{EXP+LIN}: try to fit a exponential plus linear function of the form: 

      \deqn{y = a*(1-exp(-(x+c)/b)+(g*x))}
      
      The De is calculated by iteration.
      
      \bold{Note:} In the context of luminescence dating this function has no physical meaning. Therefore no
      D0 value is returned.\cr

\code{EXP+EXP}: try to fit a double exponential function of the form
\deqn{y = (a1*(1-exp(-(x)/b1)))+(a2*(1-exp(-(x)/b2)))}
This fitting is highly robust against wrong start parameters and should be further improved.\cr

\bold{fit weighting} (suggested by Michael Dietze and Margret Fuchs)\cr

If the option \code{fit.weights=TRUE} weights are calculated using provided signal errors (Lx/Tx error) using the equation:

\deqn{fit.weights = 1/error/(sum(1/error))}

\bold{error estmation using Monte Carlo simulation}\cr

Error estimation is done using a Monte Carlo (MC) simulation approach. A set of input values is constructed by 
randomly drawing curve data from a normal distribution. The normal distribution is defined by the input values (mean=value, sd=value.error).
Then a growth curve fit is tried for each dataset which results in new distribution of values. The \link{sd} 
of this distribution is the error of the De. With increasing iterations the error value is becoming more
stable. Note: It may take some calculation time with increaing MC runs especially for the composed functions (\code{EXP+LIN}
and \code{EXP+EXP}).

Every error estimation is done with the function of the chosen fitting method. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{list}{a list containing the De (De, De Error, D01 value, D02 value and Fit type) and Fit object \link{nls} object for \code{EXP}, \code{EXP+LIN} and \code{EXP+EXP}. In case of a linear
fit \code{EXP OR LIN} a \link{lm} object is returned.}
\item{Plot}{A plot of the growth curve. A histogram for the error calculation and a test dose response plot.}
%% ...

}
\references{
%% ~put references to the literature/web site here ~
Duller, G.A.T., 2007. Assessing the error on equivalent dose estimates derived from single aliquot regenerative dose measurements. Ancient TL 25, pp. 15-24.

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology, 11, pp.1-27.
}
\author{Sebastian Kreutzer, JLU Giessen (Germany), 2012}
\note{
%%  ~~further notes~~
No D01 is returned for the fit functions \code{EXP+LIN} and \code{LIN}. A D02 value is provided for
the function \code{EXP+EXP} only.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist}}, \code{\link{plot}},  \code{\link{nls}}, \code{\link{lm}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##(1) plot growth curve for a dummy data.set
data(ExampleData.LxTxData)
plot_GrowthCurve(LxTxData)
                                   
                                   
##==================================================##
##(2) plot the growth curve only

##pdf(file="~/Desktop/Growth_Curve_Dummy.pdf",paper="special")
data(ExampleData.LxTxData)
plot_GrowthCurve(LxTxData)
##dev.off()                                   
                                   
##==================================================##
##(3) plot growth curve with pdf output on desktop (path works for Mac)

##pdf(file="~/Desktop/Growth_Curve_Dummy.pdf",paper="special")
data(ExampleData.LxTxData)
plot_GrowthCurve(LxTxData)
##dev.off()


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
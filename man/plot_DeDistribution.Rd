\name{plot_DeDistribution}
\alias{plot_DeDistribution}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot De distribution with a kernel density estimate (KDE)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot De values in ascending order and combine the plot with a kernel density function. 

}
\usage{
plot_DeDistribution(values,main=expression(paste(D[e], " Distribution")),
                    distribution.parameter="",mtext="", bw="nrd0",
                    xlab=expression(paste(D[e], " [Gy]")),
                    cex.global=1)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{values}{\link{data.frame} (\bold{required}): De (\code{values[,1]}) and De error (\code{values[,2]})}
    \item{main}{\link{character} (with default): title of the plot}
    \item{distribution.parameter}{\link{character} (optional): plot additional distribution parameter, i.e. mean, median or sd.
    Example: \code{distribution.parameter=c("median","mean","sd")}. Mean and median are potted as a line, the sd is shown as a gray polygon.}
    \item{mtext}{\link{character} (optional): further sample information (works as \link{mtext}).}
    \item{bw}{\link{character} (with default): bin-width, choose a numeric value for manual setting.}
    \item{xlab}{\link{character} (with default): label for x-axis unit. Possible values are "Gy", "s", "ka" or a user defined
    value.}
    \item{cex.global}{global scaling factor.}
    
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
##
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
Berger, G.W., 2010. An alternate form of probability-distribution plot for De values. Ancient TL 28, 11-21.

Berger, G.W., 2011. Response to Galbraith. Ancient TL 29, 48-50.

Galbraith, R.F., 2011. Some comments arising from Berger (2010). Ancient TL 29, 41-47.

Galbraith, R.F., Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology 1-71.



}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
The plot output is no 'PD' plot (compare the discussion of Berger and Galbraith in Ancient TL)!

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{density}}, \code{\link{plot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ExampleData.DeValues)
plot_DeDistribution(ExampleData.DeValues,  xlab=expression(paste(D[e], " [s]")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}
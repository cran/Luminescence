\name{plot_Histogram}
\alias{plot_Histogram}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a histogram with a separate error plot}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot a predefined histogram with an accompanying error plot as suggested
by Rex Galbraith at the UK LED in Oxford 2010.
}
\usage{
plot_Histogram(values,
               main = "Histogram",
               mtext = "",
               xlab = expression(paste(D[e]," [Gy]")), 
               cex.global = 1,
               breaks = "Sturges", normal_curve = FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{values}{\link{data.frame} (\bold{required}): De (\code{values[,1]}) and De error (\code{values[,2]})}
    \item{main}{\link{character} (with default): title of the plot}
    \item{mtext}{\link{character} (optional): further sample information (\link{mtext})}
    \item{xlab}{\link{character} (with default): label x-axis (xlab, see also \link{plot})}
    \item{cex.global}{\link{numeric} (with default): global scaling factor}
    \item{breaks}{(with default): set breakpoints for histogram. Works as in \link{hist}.}
    \item{normal_curve}{\link{logical} (with default): add a normal curve to the histogram. Mean and sd are calculated
    from the input data. More see details section.}
    
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
If the normal curve is added, the y-axis in the histogram will show the probability density. 
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
Galbraith, R., 2010. Statistics in OSL: Some Current Questions; Ask Rex. Oral presentation during the
UK TL/OSL/ESR Meeting at the School of Geography and the Environment, University of Oxford, 8-10 September 2010.

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology, 11, pp.1-27.

}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
%%  ~~further notes~~
The input data is not restricted to a special type of input data.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{hist}}, \code{\link{plot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##load data
data(ExampleData.DeValues)

##plot histogram
plot_Histogram(ExampleData.DeValues, xlab = expression(paste(D[e]," [s]")))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

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
               mtext,
               cex.global,
               breaks,
               se,
               rug,
               normal_curve,
               summary,
               summary.pos,
               colour,
               ...) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{values}{\link{data.frame} (\bold{required}): De (\code{values[,1]}) and De error (\code{values[,2]})}
    \item{mtext}{\link{character} (optional): further sample information (\link{mtext})}
    \item{cex.global}{\link{numeric} (with default): global scaling factor}
    \item{breaks}{(with default): set breakpoints for histogram. Works as in \link{hist}.}
    \item{se}{\link{logical} (optional): plot standard error points over the histogram, default is \link{FALSE}.}
    \item{rug}{\link{logical} (optional): add rugs to the histogram, default is \code{TRUE}.}
    \item{normal_curve}{\link{logical} (with default): add a normal curve to the histogram. Mean and sd are calculated from the input data. More see details section.}
    \item{summary}{\link{character} (optinal): add numerical output to the plot. Can be one or more out of: "n" (number of
          samples), "mean" (mean De value), "median" (median of the De values), "kdemax" (maximum value of probability 
          density function), "sdrel" (relative standard deviation), "sdabs" (absolute standard deviation), "serel" (relative
          standard error) and "seabs" (absolute standard deviation).}
    \item{summary.pos}{\link{numeric} (with default): optional position coordinates for the statistical
          summary. Y-coordinate refers to the right hand y-axis.}
    \item{colour}{\link{numeric} or \link{character}(with default): optional vector of length 4 which specifies the colours 
          of the folloqing plot items in exactly this order: histogram bars, rug lines, normal distribution curve and 
          standard error points (e.g. c("grey", "black", "red", "grey")).}
    \item{...}{further arguments and graphical parameters passed to \code{\link{plot}}. If y-axis labels are provided, these
          must be specified as a vector of length 2 since the plot features two axes (e.g. \code{xlab = c("axis label 1", 
          "axis label 2")).}}
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
\author{Sebastian Kreutzer, JLU Giessen (Germany), Michael Dietze (TU Dresden)}

\section{Version}{0.4 [2013-03-04]}

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
## load data
data(ExampleData.DeValues)

## plot histogram the easiest way
plot_Histogram(ExampleData.DeValues)

## plot histogram with some more modifications
plot_Histogram(ExampleData.DeValues, 
               rug = TRUE, 
               normal_curve = TRUE, 
               cex.global = 0.9, 
               pch = 2,
               colour = c("grey", "black", "blue", "green"),
               summary = c("n", "mean", "sdrel"),
               summary.pos = c(3500, 140),
               main = "Histogram of De-values",
               mtext = "Example data set", 
               ylab = c(expression(paste(D[e], " Distribution")),
                        "Std.-err."),
               xlim = c(1800, 4000))
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

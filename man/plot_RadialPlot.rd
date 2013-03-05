\name{plot_RadialPlot}
\alias{plot_RadialPlot}
\title{
Produce a Galbraith's radial plot. 
}
\description{
A Galbraith radial plot is produced on a logarithmic or a linear De scale. The function is
based on a rewritten S script of Rex Galbraith. 
}
\usage{
plot_RadialPlot(sample, 
                sample.groups, 
                sample.legend, 
                sample.lty = 1, 
                sample.pch = 1, 
                sample.col = "black", 
                sample.min = FALSE,
                sample.max = FALSE,
                sample.mtext, 
                zscale.log = TRUE, 
                zaxis.scale, zaxis.group_circle = FALSE, yaxis.scale, 
                plot.2sigmaRange = TRUE, 
                plot.area_ratio = 4.5/6, 
                zlab = expression(paste(D[e], " [Gy]")), 
                main = expression(paste(D[e], " Distribution", sep = "")), 
                cex.global = 1, xscale_factor = 1.01)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{\link{data.frame} (\bold{required}): two column data frame with the input values, e.g. 'de' and 'se'.
}
  \item{sample.groups}{\link{list} (optional): option to group the input data set like: 
  
  \code{sample.groups = list(c(1:14), c(15:26), c(27:40))}
}
  \item{sample.legend}{\link{character} (optional): character vector for a legend. This option is provided for the parameter \code{sample.groups}, but can also be used for one sample.  
}
  \item{sample.lty}{\link{vector} (with default): line type for the central value (see \link{par}. If the sample is grouped, a line type can be defined for each data set. )
}
  \item{sample.pch}{\link{vector} (with default): point type for the presented data (see \link{par}. If the sample is grouped, a point type can be defined for each data.set
}
  \item{sample.col}{\link{vector} or \link{character} (with default): colour of the points and lines (see \link{colors}). If the sample is grouped, a colour can be defined for each group.
}
\item{sample.min}{\link{logical} (with default): add minimum value to respective data point in plot.
}
\item{sample.max}{\link{logical} (with default): add maximum value to respective data point in plot.
}
\item{sample.mtext}{\link{character} (optional): \link{mtext} on the top of the plot. By default, a statistical summary of the sample(s) is plotted. To prevent
any text set this parameter to \code{""}.
}
  \item{zscale.log}{\link{logical} (with default): log De scale (\code{TRUE/FALSE})
}
  \item{zaxis.scale}{\link{numeric} (optional): option to set the z-scale manually. 
  
  Example: \code{zaxis.scale = seq(50,120, by = 10)}
}
  \item{zaxis.group_circle}{\link{logical} (with default): shows additional group circles for the 2-sigma uncertainties on the z-scale.
}
  \item{yaxis.scale}{\link{vector} (optional): option for manual y-axis scaling. Example: \code{yaxis.scale=c(-15,15)}
}
  \item{plot.2sigmaRange}{\link{logical} (with default): plot a grey polygon showing the 2-sigma range of the central value.
}
  \item{plot.area_ratio}{\link{vector} (with default): option for manual plot ratio adjustments. 
  
  Example \code{plot.area_ratio=4.5/5.5} 
}
  \item{zlab}{\link{character} (with default): z-axis (semi circle) label
}
  \item{main}{\link{character} (with default): title of the plot
}
  \item{cex.global}{\link{numeric} (with default): global scaling factor
}
  \item{xscale_factor}{\link{numeric} (with default): scaling factor for the z-scale ticks.
}
}
\details{
Details and the theoretical background on the radial plot are given in the cited literature.

}
\value{
Returns a plot.
}
\references{

Galbraith, R.F., 1988. Graphical Display of Estimates Having Differing Standard Errors. Technometrics, 30 (3), pp. 271-281.

Galbraith, R.F., 1990. The radial plot: Graphical assessment of spread in ages. International Journal of Radiation Applications and Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3), pp. 207-214.

Galbraith, R. & Green, P., 1990. Estimating the component ages in a finite mixture. International Journal of Radiation Applications and Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3), pp. 197-206. 

Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission track ages. Nuclear Tracks And Radiation Measurements, 21 (4), pp. 459-470.

Galbraith, R.F., 1994. Some Applications of Radial Plots. Journal of the American Statistical Association, 89 (428), pp. 1232-1242.

Galbraith, R.F., 2010. On plotting OSL equivalent doses. Ancient TL, 28 (1), pp. 1-10. 

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology, 11, pp.1-27.

}
\author{Original S script: Rex Gablraith, University College London (UK), 
Revised R script: Sebastian Kreutzer, JLU Giessen (Germany),
Michael Dietze, TU Dresden (Germany)
}
\note{
This function is based on an S script of Rex Galbraith. To reduce the manual adjustments, the function 
has been rewritten. Thanks to Rex Galbraith for useful comments on this function.
}

\section{Version}{0.4 [2013-03-03]}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{plot}, \link{legend}
}
\examples{
##load data
data(ExampleData.DeValues)

##plot the example data set the easiest way
plot_RadialPlot(ExampleData.DeValues)

##Notice that a statistical summary is provided by default.
##However, a sample ID is missing. Notice also that the z-scale
##units are sub-optimally spaced and not (yet) logarithmically
##scaled. To take care of this, look at the following code:

## a) plot a user-defined sample text
usertext <- "Example data set"

## b) define an evenly-spaced z-axis between min and max
zscale <- seq(from = min(ExampleData.DeValues$ED),
              to = max(ExampleData.DeValues$ED),
              by = 500)
              
##plot the same data set a bit more user-adjusted
plot_RadialPlot(sample = ExampleData.DeValues,
                zscale.log = TRUE,
                zaxis.scale = zscale,
                zlab = expression(paste(D[e], " [s]")),
                sample.mtext = usertext,
                sample.pch = 4)

##Now let us assume that the data set consists of two
##different groups of samples that should however be
##plotted in one graph. The function can handle grouped
##data by specifying a group agument:

group.indices <- list(c(1:10),  # group 1 with samples 1 to 10
                      c(11:20)) # group 2 with samples 11 to 20

##plot the data.set grouped 
plot_RadialPlot(ExampleData.DeValues,
                zscale.log = TRUE, 
                zaxis.scale = seq(2000, 4000, by = 500), 
                zlab = expression(paste(D[e], " [s]")),
                sample.groups = group.indices,
                sample.col = c("royalblue", "orange3"),
                sample.pch = c(3, 4),
                cex.global = 0.8)
                
##Notice that the default statistical summary is provided
##for each group individually and that z-axis limits were
##changed from example 1.


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

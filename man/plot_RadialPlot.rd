\name{plot_RadialPlot}
\alias{plot_RadialPlot}
\title{Function to create a Radial Plot}
\description{A Galbraith's radial plot is produced on a logarithmic or a linear 
scale.}
\usage{plot_RadialPlot(data, na.exclude = TRUE, negatives = "remove", 
    log.z = TRUE, central.value, centrality = "mean.weighted", 
    mtext, summary, summary.pos, legend, legend.pos, stats, plot.ratio, 
    bar.col, y.ticks = TRUE, grid.col, line, line.col, line.label, 
    output = FALSE, ...)}
\arguments{
  \item{data}{\code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
(required): for \code{data.frame} two columns: De (\code{data[,1]})
and De error (\code{data[,2]}). To plot several data sets in one plot,
the data sets must be provided as \code{list}, e.g. 
\code{list(data.1, data.2)}.}
  \item{na.exclude}{\code{\link{logical}} (with default): excludes \code{NA} values from 
the data set prior to any further operations.}
  \item{negatives}{\code{\link{character}} (with default): rule for negative values. Default
is \code{"remove"} (i.e. negative values are removed from the data set).}
  \item{log.z}{\code{\link{logical}} (with default): Option to display the z-axis
in logarithmic scale. Default is \code{TRUE}.}
  \item{central.value}{\code{\link{numeric}}: User-defined central value, primarily used for
horizontal centering of the z-axis.}
  \item{centrality}{\code{\link{character}} (with default): measure of centrality, used for
automatically centering the plot and drawing the central line. Can be
one out of \code{"mean"}, \code{"median"}, \code{"mean.weighted"}, 
and \code{"median.weighted"}. Default is \code{"mean.weighted"}.}
  \item{mtext}{\code{\link{character}}: additional text below the plot title.}
  \item{summary}{\code{\link{character}} (optional): adds numerical output to the plot. 
Can be one or more out of: \code{"n"} (number of samples), \code{"mean"} (mean De 
value), \code{"mean.weighted"} (error-weighted mean), \code{"median"} (median of 
the De values), \code{"sdrel"} (relative standard deviation in 
percent), \code{"sdabs"} (absolute standard deviation), \code{"serel"} (relative 
standard error), \code{"seabs"} (absolute standard error) and \code{"in.ci"} (percent
of samples in confidence interval, e.g. 2-sigma).}
  \item{summary.pos}{\code{\link{numeric}} or \code{\link{character}} (with default): optional  
position coordinates or keyword (e.g. \code{"topright"}) for the 
statistical summary. Alternatively, the keyword \code{"sub"} may be
specified to place the summary below the plot header. However, this
latter option is only possible if \code{mtext} is not used.}
  \item{legend}{\code{\link{character}} vector (optional): legend content to be added
to the plot.}
  \item{legend.pos}{\code{\link{numeric}} or \code{\link{character}} (with default): optional  
position coordinates or keyword (e.g. \code{"topright"}) for the legend
to be plotted.}
  \item{stats}{\code{\link{character}}: additional labels of statistically important
values in the plot. One or more out of the following: \code{"min"}, 
\code{"max"}, \code{"median"}.}
  \item{plot.ratio}{\code{\link{numeric}}: User-defined plot area ratio (i.e. curvature of
the z-axis). If omitted, the default value (\code{4.5/5.5}) is used and
modified automatically to optimise the z-axis curvature. 
The parameter should be decreased when data points are plotted outside
the z-axis or when the z-axis gets too elliptic.}
  \item{bar.col}{\code{\link{character}} or \code{\link{numeric}} (with default): colour 
of the bar showing the 2-sigma range around the central value. To 
disable the bar, use \code{"none"}. Default is \code{"grey"}.}
  \item{y.ticks}{\code{\link{logical}}: Option to hide y-axis labels. Useful for data  
with small scatter.}
  \item{grid.col}{\code{\link{character}} or \code{\link{numeric}} (with default): colour 
of the grid lines (originating at [0,0] and stretching to the z-scale).  
To disable grid lines, use \code{"none"}. Default is \code{"grey"}.}
  \item{line}{\code{\link{numeric}}: numeric values of the additional lines to be
added.}
  \item{line.col}{\code{\link{character}} or \code{\link{numeric}}: colour of the
additional lines.}
  \item{line.label}{\code{\link{character}}: labels for the additional lines.}
  \item{output}{\code{\link{logical}}: Optional output of numerical plot parameters.
These can be useful to reproduce similar plots. Default is \code{FALSE}.}
  \item{\dots}{Further plot arguments to pass. \code{xlab} must be a vector of length 2,
specifying the upper and lower x-axes labels.}
}
\details{Details and the theoretical background of the radial plot are given 
in the cited literature. This function is based on an S script of Rex 
Galbraith. To reduce the manual adjustments, the function has been 
rewritten. Thanks to Rex Galbraith for useful comments on this function.
\cr Plotting can be disabled by adding the argument 
\code{plot = "FALSE"}, e.g. to return only numeric plot output.\cr

Earlier versions of the Radial Plot in this package had the 2-sigma-bar
drawn onto the z-axis. However, this might have caused misunderstanding
in that the 2-sigma range may also refer to the z-scale, which it does
not! Rather it applies only to the x-y-coordinate system (standardised
error vs. precision). A spread in doses or ages must be drawn as lines
originating at zero precision (x0) and zero standardised estimate (y0).
Such a range may be drawn by adding lines to the radial plot (
\code{line}, \code{line.col}, \code{line.label}, cf. examples).}
\value{Returns a plot object.}
\references{Galbraith, R.F., 1988. Graphical Display of Estimates Having Differing 
Standard Errors. Technometrics, 30 (3), 271-281.

Galbraith, R.F., 1990. The radial plot: Graphical assessment of spread in 
ages. International Journal of Radiation Applications and Instrumentation. 
Part D. Nuclear Tracks and Radiation Measurements, 17 (3), 207-214. 

Galbraith, R. & Green, P., 1990. Estimating the component ages in a 
finite mixture. International Journal of Radiation Applications and 
Instrumentation. Part D. Nuclear Tracks and Radiation Measurements, 17 (3) 
197-206. 

Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission 
track ages. Nuclear Tracks And Radiation Measurements, 21 (4),  
459-470. 

Galbraith, R.F., 1994. Some Applications of Radial Plots. Journal of the 
American Statistical Association, 89 (428), 1232-1242.

Galbraith, R.F., 2010. On plotting OSL equivalent doses. Ancient TL, 
28 (1), 1-10. 

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
dose and error calculation and display in OSL dating: An overview and 
some recommendations. Quaternary Geochronology, 11, 1-27. }
\author{Michael Dietze, GFZ Potsdam (Germany), Sebastian Kreutzer, JLU Giessen 
(Germany)\cr
Based on a rewritten S script of Rex Galbraith, 2010\cr
R Luminescence Package Team}



\seealso{\code{\link{plot}}, \code{\link{plot_KDE}}, \code{\link{plot_Histogram}}}
\examples{
## load example data
data(ExampleData.DeValues, envir = environment())
ExampleData.DeValues <- 
  Second2Gray(values = ExampleData.DeValues, dose_rate = c(0.0438,0.0019))

## plot the example data straightforward
plot_RadialPlot(data = ExampleData.DeValues)

## now with linear z-scale
plot_RadialPlot(data = ExampleData.DeValues,
                log.z = FALSE)

## now with output of the plot parameters
plot1 <- plot_RadialPlot(data = ExampleData.DeValues,
                         log.z = FALSE,
                         output = TRUE)
plot1
plot1$zlim

## now with adjusted z-scale limits
plot_RadialPlot(data = ExampleData.DeValues,
               log.z = FALSE,
               zlim = c(100, 200))

## now the two plots with serious but seasonally changing fun
#plot_RadialPlot(data = data.3, fun = TRUE)

## now with user-defined central value, in log-scale again
plot_RadialPlot(data = ExampleData.DeValues,
                central.value = 150)

## now with legend, colour, different points and smaller scale
plot_RadialPlot(data = ExampleData.DeValues,
                legend.text = "Sample 1",
                col = "tomato4",
                bar.col = "peachpuff",
                pch = "R",
                cex = 0.8)

## now without 2-sigma bar, y-axis, grid lines and central value line
plot_RadialPlot(data = ExampleData.DeValues,
                bar.col = "none",
                grid.col = "none",
                y.ticks = FALSE,
                lwd = 0)

## now with user-defined axes labels
plot_RadialPlot(data = ExampleData.DeValues,
                xlab = c("Data error [\%]",
                         "Data precision"),
                ylab = "Scatter",
                zlab = "Equivalent dose [Gy]")

## now with minimum, maximum and median value indicated
plot_RadialPlot(data = ExampleData.DeValues,
                central.value = 150,
                stats = c("min", "max", "median"))

## now with a brief statistical summary
plot_RadialPlot(data = ExampleData.DeValues,
                summary = c("n", "in.ci"))

## now with another statistical summary as subheader
plot_RadialPlot(data = ExampleData.DeValues,
                summary = c("mean.weighted", "median"),
                summary.pos = "sub")

## now the data set is split into sub-groups, one is manipulated
data.1 <- ExampleData.DeValues[1:15,]
data.2 <- ExampleData.DeValues[16:25,] * 1.3

## now a common dataset is created from the two subgroups
data.3 <- list(data.1, data.2)

## now the two data sets are plotted in one plot
plot_RadialPlot(data = data.3)

## now with some graphical modification
plot_RadialPlot(data = data.3,
                col = c("darkblue", "darkgreen"),
                bar.col = c("lightblue", "lightgreen"),
                pch = c(2, 6),
                summary = c("n", "in.ci"),
                summary.pos = "sub",
                legend = c("Sample 1", "Sample 2"))
}
\section{Function version}{0.5.2 (2014-06-05 13:42:41)}

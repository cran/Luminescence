\name{plot_KDE}
\alias{plot_KDE}
\title{Plot kernel density estimate with statistics}
\description{Plot a kernel density estimate of measurement values in combination with 
the actual values and associated error bars in ascending order. 
Optionally, statistical measures such as mean, median, standard 
deviation, standard error and quartile range can be provided visually 
and numerically.}
\usage{plot_KDE(values, na.exclude = TRUE, distribution.parameters, 
    summary, summary.pos, bw = "nrd0", ...)}
\arguments{
  \item{values}{\code{\link{data.frame}} or \code{\linkS4class{RLum.Results}} object 
(required): for \code{data.frame}: two columns: De (\code{values[,1]})
and De error (\code{values[,2]}). For plotting multiple data sets, these
must be provided as \code{list} (e.g. \code{list(dataset1, dataset2)}).}
  \item{na.exclude}{\code{\link{logical}} (with default): exclude NA values from the data
set prior to any further operations.}
  \item{distribution.parameters}{\code{\link{character}} (optional): plot additional distribution 
parameters. Can be one or more out of "mean", "median", "kdemax" 
(maximum value of probability density function), "sd" (standard 
deviation) and "qr" (quartile range). Example: 
\code{distribution.parameter=c("median","mean","sd")}. Mean and 
median are potted as a line, the standard deviation is shown as a 
gray polygon. Polygons are not possible for multiple data sets.}
  \item{summary}{\code{\link{character}} (optinal): add numerical output to the plot. 
Can be one or more out of: "n" (number of samples), "mean" (mean De 
value), "median" (median of the De values), "kdemax" (maximum value of 
probability density function), "sdrel" (relative standard deviation in 
percent), "sdabs" (absolute standard deviation), "serel" (relative 
standard error) and "seabs" (absolute standard error). Summary
information is not possible for multiple datasets.}
  \item{summary.pos}{\code{\link{numeric}} (with default): optional position coordinates for 
the statistical summary. Y-coordinate refers to the right hand y-axis.}
  \item{bw}{\code{\link{character}} (with default): bin-width, choose a numeric 
value for manual setting.}
  \item{\dots}{further arguments and graphical parameters passed to \code{\link{plot}}.}
}
\details{The function allows to pass several plot arguments, such as \code{main}, 
\code{xlab}, \code{cex}. However, as the figure is an overlay of two 
separate plots, \code{ylim} must be specified in the order: c(ymin_axis1, 
ymax_axis1, ymin_axis2, ymax_axis2). Similarly, if other than the default 
colours are desired, the argument col must be provided with colours in 
the following order: probability density function, De values, De error 
bars, sd or qr polygon. See examples for some further explanations. For 
details on the calculation of the bin-width (parameter \code{bw}) see 
\code{\link{density}}.}


\author{R Luminescence Package Team}
\note{The plot output is no 'PD' plot (cf. the discussion of Berger and 
Galbraith in Ancient TL; see references)!}


\seealso{\code{\link{density}}, \code{\link{plot}}}
\examples{
# read example data set
data(ExampleData.DeValues, envir = environment())

# native function call
plot_KDE(ExampleData.DeValues)

# function call with some user-defined modifications
plot_KDE(ExampleData.DeValues,
         main = "Plot of Dose distribution data",
         col = c("red", "black", "grey", "cyan"),
         xlab = "Equivalent dose [Gy]",
         ylabs = c("KDE estimate", "cumulative De values"),
         distribution.parameters = c("qr", "median"), 
         xlim = c(2000, 5000),
         ylims = c(0, 0.005, -5, 50),
         summary = c("n", "median", "serel", "seabs"),
         cex = 0.8)

# function call with complete numerical statistical description output
plot_KDE(ExampleData.DeValues,
         distribution.parameters = c("qr", "kdemax"), 
         summary = c("n", "mean", "median", "kdemax", "serel", 
                     "sdrel", "sdabs", "seabs"))

# function call with two data sets
dataset1 <- ExampleData.DeValues[1:8,]
dataset2 <- ExampleData.DeValues[9:25,]

plot_KDE(list(dataset1, dataset2), col = c("blue", "orange"))
}

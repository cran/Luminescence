\name{plot_KDE}
\alias{plot_KDE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a kernel density estimate (KDE), sorted values with associated errors and statistical measures}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Plot a kernel density estimate of measurement values in combination with the 
actual values and associated error bars in ascending order. Optionally, statistical
measures such as mean, median, standard deviation, standard error and quartile 
range can be provided visually and as numeric values.
}
\usage{
plot_KDE(values,
         distribution.parameters, 
         summary,
         summary.pos,
         bw = "nrd0",
         ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{values}{\link{data.frame} (\bold{required}): two columns: De (\code{values[,1]}) and De error (\code{values[,2]})}
    \item{distribution.parameters}{\link{character} (optional): plot additional distribution parameters. Can be one or more
          out of "mean", "median", "KDEmax" (maximum value of probability density function), "sd" (standard deviation) and 
          "qr" (quartile range).
          Example: \code{distribution.parameter=c("median","mean","sd")}. Mean and median are potted as a line, the standard 
          deviation is shown as a gray polygon.}
    \item{summary}{\link{character} (optinal): add numerical output to the plot. Can be one or more out of: "n" (number of
          samples), "mean" (mean De value), "median" (median of the De values), "kdemax" (maximum value of probability 
          density function), "sdrel" (relative standard deviation), "sdabs" (absolute standard deviation), "serel" (relative
          standard error) and "seabs" (absolute standard error).}
    \item{summary.pos}{\link{numeric} (with default): optional position coordinates for the statistical
    summary. Y-coordinate refers to the right hand y-axis.}
    \item{bw}{\link{character} (with default): bin-width, choose a numeric value for manual setting.}
    \item{...}{further arguments and graphical parameters passed to \code{\link{plot}}}
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
The function allows, to pass several plot arguments, such as main, xlab, cex. However, since the figure is an overlay of two separate plots, ylim must be specified in the order: c(ymin_axis1, ymax_axis1, ymin_axis2, ymax_axis2). Similarly, if other than the default colours are desired, the argument col must be provided with colours in the following order: probability density function, De values, De error bars, sd or qr polygon. See examples for some further explanations.For details on the calculation of the bin-width (parameter \code{bw}) see \code{\link{density}}.
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
Berger, G.W., 2010. An alternate form of probability-distribution plot for De values. Ancient TL 28, pp. 11-21.

Berger, G.W., 2011. Response to Galbraith. Ancient TL 29, pp. 48-50.

Galbraith, R.F., 2011. Some comments arising from Berger (2010). Ancient TL 29, pp. 41-47.

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL 
dating: An overview and some recommendations. Quaternary Geochronology, 11, pp.1-27.



}
\author{Sebastian Kreutzer, JLU Giessen, (Germany),
Michael Dietze, TU Dresden, (Germany)}
\note{
The plot output is no 'PD' plot (cf. the discussion of Berger and Galbraith in Ancient TL; see references)!

}
\section{Version}{3.0.4 [2013-03-13]}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{density}}, \code{\link{plot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# read example data set
data(ExampleData.DeValues)

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
                    summary = c("n", "mean", "median", "kdemax", "serel", "sdrel", "sdabs", "seabs"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

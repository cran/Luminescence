\name{plot_RLum.Data.Spectrum}
\alias{plot_RLum.Data.Spectrum}
\title{Plot function for an RLum.Data.Spectrum S4 class object}
\description{The function provides a standardized plot output for spectrum data of an 
RLum.Data.Spectrum S4 class object }
\usage{plot_RLum.Data.Spectrum(object, par.local = TRUE, plot.type = "contour", 
    optical.wavelength.colours = TRUE, ...)}
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Spectrum}} (\bold{required}): 
S4 object of class \code{RLum.Data.Spectrum}}
  \item{par.local}{\code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
the plot is shown in one column and one row. If \code{par.local = FALSE} 
global parameters are inherited.}
  \item{plot.type}{\code{\link{character}} (with default): plot type, for 3D-plot use \code{persp}, 
or \code{persp3d} and \code{contour} for a 2D-plot.\cr

Note: The use of \code{persp3d} will produce a dynamic 3D surface plot on the
screen.}
  \item{optical.wavelength.colours}{\code{\link{logical}} (with default): use optical wavelength colour palette.
Note: For this, the spectrum range is limited: \code{c(350,750)}. 
Own colours can be set with the argument \code{col}.}
  \item{\dots}{further arguments and graphical parameters that will be passed to the 
\code{plot} function.}
}
\details{Spectrum is visualized as 3D or 2D plot. Both plot types are based on internal
R plot functions. \cr

Arguments that will be passed to \code{\link{persp}}:
\itemize{ 
\item \code{shade}: default is \code{0.4}  
\item \code{phi}: default is \code{30}
\item \code{theta}: default is \code{30}
\item \code{expand}: default is \code{1}
\item \code{ticktype}: default is \code{detailed}
} 

\bold{Further arguments that will be passed}\cr
\code{xlab}, \code{ylab}, \code{zlab}, \code{xlim}, \code{ylim}, \code{main}, 
\code{mtext}}
\value{Returns a plot.}
\references{#}
\author{Sebastian Kreutzer, JLU Giessen (Germany)
R Luminescence Package Team}
\note{Not all arguments of \code{\link{plot}} will be passed!}


\seealso{\code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{persp}}, 
\code{\link{persp3d}}, \code{\link{contour}}}
\examples{
 
##(1) simple call (note: currently no example data set is provided!)
# plot_RLum.Data.Spectrum(Example.data)  
 
}

\keyword{aplot}

\name{plot_RLum.Data.Curve}
\alias{plot_RLum.Data.Curve}
 
\title{
Plot function for an RLum.Data.Curve S4 class object
}
\description{
The functions provides a standardized plot output for curve data of an RLum.Data.Curve S4 class object
}
\usage{
plot_RLum.Data.Curve(object, par.local = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{\linkS4class{RLum.Data.Curve}} (\bold{required}): S4 object of class \code{RLum.Data.Curve}
}

  \item{par.local}{\code{\link{logical}} (with default): use local graphical parameters for plotting, e.g.
  the plot is shown in one column and one row. If \code{par.local = FALSE} global parameters are inherited.  
}

  \item{\dots}{further arguments and graphical parameters passed to the \code{plot} function
}
}
\details{
Only single curve data can be plotted with this function. Arguments according to \code{\link{plot}}.

}
\value{
Returns a plot.
}

\references{
#
}

\author{Sebastian Kreutzer, JLU Giessen/Freiberg Instruments (Germany)
}

\section{Version}{0.1 [2013-03-27]}


\note{
Not all arguments of \code{\link{plot}} will be passed!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot}}, \code{\link{plot_RLum}}
}
\examples{
##plot curve data

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum.Data.Curve object
plot_RLum.Data.Curve(temp)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

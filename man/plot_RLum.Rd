\name{plot_RLum}
\alias{plot_RLum}
\title{
General plot function for RLum S4 class objects
}
\description{
Function calls object specific plot functions for RLum S4 class objects
}
\usage{
plot_RLum(object, ...)
}

\arguments{
  \item{object}{\code{\linkS4class{RLum}} (\bold{required}): S4 object of class \code{RLum}}

  \item{...}{further arguments and graphical parameters passed to the specific plot functions}
}

\details{
The function provides a generalized access point for plotting specific \code{\linkS4class{RLum}} objects. 
Depending on the input object the corresponding plot function will be selected. 
Allowed arguments can be found in the documentations of each plot function. 

\tabular{lll}{
\bold{object} \tab \tab \bold{corresponding plot function} \cr

\code{\linkS4class{RLum.Data.Curve}} \tab : \tab \code{\link{plot_RLum.Data.Curve}} \cr
\code{\linkS4class{RLum.Analysis}} \tab : \tab \code{\link{plot_RLum.Analysis}}





}

}
\value{
Returns a plot
}
\references{
#
}
\author{Sebastian Kreutzer, 2013 (Freiberg Instruments/JLU Giessen, Germany)
}

\section{Version}{0.1 [2013-01-18]}

\note{
The provided plot output depends on the object.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot_RLum.Data.Curve}}, 
\code{\linkS4class{RLum.Data.Curve}}
}
\examples{
##plot curve data

#load Example data
data(ExampleData.CW_OSL_Curve, envir = environment())

#transform data.frame to RLum.Data.Curve object
temp <- as(ExampleData.CW_OSL_Curve, "RLum.Data.Curve")

#plot RLum object 
plot_RLum(temp)
}
\keyword{aplot}

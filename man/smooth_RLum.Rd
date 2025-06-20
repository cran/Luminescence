% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Luminescence-generics.R
\name{smooth_RLum}
\alias{smooth_RLum}
\alias{smooth_RLum,list-method}
\title{Smoothing of data for RLum S4-class objects}
\usage{
smooth_RLum(object, ...)

\S4method{smooth_RLum}{list}(object, ...)
}
\arguments{
\item{object}{\linkS4class{RLum} (\strong{required}):
S4 object of class \code{RLum}}

\item{...}{further arguments passed to the specific class method}
}
\value{
An object of the same type as the input object provided.
}
\description{
The function provides a generalised access point for specific
\linkS4class{RLum} objects. Depending on the input object, the corresponding
function will be selected.
Allowed arguments can be found in the documentations of the corresponding
\linkS4class{RLum} class. The smoothing is based on an internal function
called \code{.smoothing}.
}
\section{Functions}{
\itemize{
\item \code{smooth_RLum(list)}: Returns a list of \linkS4class{RLum} objects that had been passed to \link{smooth_RLum}

}}
\note{
Currently only \code{RLum} objects of class \code{RLum.Data.Curve} and \code{RLum.Analysis}
(with curve data) are supported.
}
\section{Function version}{
 0.1.0
}

\examples{

## load example data
data(ExampleData.CW_OSL_Curve, envir = environment())

## create RLum.Data.Curve object from this example
curve <-
  set_RLum(
      class = "RLum.Data.Curve",
      recordType = "OSL",
      data = as.matrix(ExampleData.CW_OSL_Curve)
  )

## plot data without and with smoothing
plot_RLum(curve)
plot_RLum(smooth_RLum(curve))

}
\seealso{
\linkS4class{RLum.Data.Curve}, \linkS4class{RLum.Analysis}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2025. smooth_RLum(): Smoothing of data for RLum S4-class objects. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{utilities}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_RLum.R
\name{merge_RLum}
\alias{merge_RLum}
\title{General merge function for RLum S4 class objects}
\usage{
merge_RLum(objects, ...)
}
\arguments{
\item{objects}{\link{list} of \linkS4class{RLum} (\strong{required}):
list of S4 object of class \code{RLum}}

\item{...}{further arguments that one might want to pass to the specific merge function}
}
\value{
Return is the same as input objects as provided in the list.
}
\description{
Function calls object-specific merge functions for RLum S4 class objects.

The function provides a generalised access point for merging specific
\linkS4class{RLum} objects. Depending on the input object, the
corresponding merge function will be selected.  Allowed arguments can be
found in the documentation of each merge function.
Empty list elements (\code{NULL}) are automatically removed from the input \code{list}.

\tabular{lll}{
\strong{object} \tab \tab \strong{corresponding merge function} \cr
\linkS4class{RLum.Data.Curve} \tab -> \tab \link{merge_RLum.Data.Curve} \cr
\linkS4class{RLum.Data.Spectrum} \tab -> \tab \link{merge_RLum.Data.Spectrum} \cr
\linkS4class{RLum.Analysis} \tab -> \tab \link{merge_RLum.Analysis} \cr
\linkS4class{RLum.Results} \tab -> \tab \link{merge_RLum.Results}
}
}
\note{
So far not for every \code{RLum} object a merging function exists.
}
\section{Function version}{
 0.1.3
}

\examples{


##Example based using data and from the calc_CentralDose() function

##load example data
data(ExampleData.DeValues, envir = environment())

##apply the central dose model 1st time
temp1 <- calc_CentralDose(ExampleData.DeValues$CA1)

##apply the central dose model 2nd time
temp2 <- calc_CentralDose(ExampleData.DeValues$CA1)

##merge the results and store them in a new object
temp.merged <- get_RLum(merge_RLum(objects = list(temp1, temp2)))

}
\seealso{
\linkS4class{RLum.Data.Curve}, \linkS4class{RLum.Data.Image},
\linkS4class{RLum.Data.Spectrum}, \linkS4class{RLum.Analysis}, \linkS4class{RLum.Results}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2025. merge_RLum(): General merge function for RLum S4 class objects. Function version 0.1.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{utilities}

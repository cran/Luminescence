% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Luminescence-generics.R
\name{length_RLum}
\alias{length_RLum}
\title{Length retrieval function for RLum S4 class objects}
\usage{
length_RLum(object)
}
\arguments{
\item{object}{\linkS4class{RLum} (\strong{required}):
S4 object of class \code{RLum}}
}
\value{
An \link{integer} indicating the length of the object.
}
\description{
The function provides a generalised access point for specific
\linkS4class{RLum} objects. Depending on the input object, the corresponding
function will be selected.
Allowed arguments can be found in the documentations of the corresponding
\linkS4class{RLum} class.
}
\section{Function version}{
 0.1.0
}

\seealso{
\linkS4class{RLum.Data.Curve},
\linkS4class{RLum.Data.Image},
\linkS4class{RLum.Data.Spectrum},
\linkS4class{RLum.Analysis},
\linkS4class{RLum.Results}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2025. length_RLum(): Length retrieval function for RLum S4 class objects. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{utilities}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Luminescence-generics.R
\name{set_Risoe.BINfileData}
\alias{set_Risoe.BINfileData}
\title{General setter function for Risoe.BINfileData objects}
\usage{
set_Risoe.BINfileData(
  METADATA = data.frame(),
  DATA = list(),
  .RESERVED = list()
)
}
\arguments{
\item{METADATA}{x}

\item{DATA}{x}

\item{.RESERVED}{x}
}
\value{
A \linkS4class{Risoe.BINfileData} object.
}
\description{
The function provides a generalised access point for specific
\linkS4class{Risoe.BINfileData} objects. Depending on the input object, the
corresponding function will be selected.
Allowed arguments can be found in the documentations of the corresponding
\linkS4class{Risoe.BINfileData} class.
}
\section{Function version}{
 0.1
}

\seealso{
\linkS4class{Risoe.BINfileData}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2025. set_Risoe.BINfileData(): General setter function for Risoe.BINfileData objects. Function version 0.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{utilities}

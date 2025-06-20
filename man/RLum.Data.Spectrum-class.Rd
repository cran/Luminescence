% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RLum.Data.Spectrum-class.R
\docType{class}
\name{RLum.Data.Spectrum-class}
\alias{RLum.Data.Spectrum-class}
\alias{show,RLum.Data.Spectrum-method}
\alias{set_RLum,RLum.Data.Spectrum-method}
\alias{get_RLum,RLum.Data.Spectrum-method}
\alias{names_RLum,RLum.Data.Spectrum-method}
\alias{bin_RLum.Data,RLum.Data.Spectrum-method}
\title{Class \code{"RLum.Data.Spectrum"}}
\usage{
\S4method{show}{RLum.Data.Spectrum}(object)

\S4method{set_RLum}{RLum.Data.Spectrum}(
  class,
  originator,
  .uid,
  .pid,
  recordType = "Spectrum",
  curveType = NA_character_,
  data = matrix(),
  info = list()
)

\S4method{get_RLum}{RLum.Data.Spectrum}(object, info.object)

\S4method{names_RLum}{RLum.Data.Spectrum}(object)

\S4method{bin_RLum.Data}{RLum.Data.Spectrum}(object, bin_size.col = 1, bin_size.row = 1)
}
\arguments{
\item{object}{\code{\link{get_RLum}}, \code{\link{names_RLum}} (\strong{required}):
an object of class \linkS4class{RLum.Data.Spectrum}}

\item{class}{\code{\link{set_RLum}}; \link{character} (\emph{automatic}):
name of the \code{RLum} class to create.}

\item{originator}{\link{character} (\emph{automatic}):
contains the name of the calling function (the function that produces this object);
can be set manually.}

\item{.uid}{\code{\link{set_RLum}}; \link{character} (\emph{automatic}):
sets an unique ID for this object using the internal C++ function \code{create_UID}.}

\item{.pid}{\code{\link{set_RLum}}; \link{character} (\emph{with default}):
option to provide a parent id for nesting at will.}

\item{recordType}{\code{\link{set_RLum}}; \link{character}:
record type (e.g. "OSL")}

\item{curveType}{\code{\link{set_RLum}}; \link{character}:
curve type (e.g. "predefined" or "measured")}

\item{data}{\code{\link{set_RLum}}; \link{matrix}:
raw curve data. If data is of type \code{RLum.Data.Spectrum}, this can be used
to re-construct the object. If the object is reconstructed, \code{.uid}, \code{.pid}
and \code{originator} are always taken from the input object}

\item{info}{\code{\link{set_RLum}} \link{list}:
info elements}

\item{info.object}{\code{\link{get_RLum}}; \link{character} (\emph{optional}):
the name of the info object to be called. If no info element name
is provided, the raw curve data (matrix) will be returned}

\item{bin_size.col}{\link{integer} (\emph{with default}):
set number of channels used for each bin, e.g. \code{bin_size.col = 2} means that
two channels are binned. Note: The function does not check the input, very large values
mean a full column binning (a single sum)}

\item{bin_size.row}{\link{integer} (\emph{with default}):
set number of channels used for each bin, e.g. \code{bin_size.row = 2} means that
two channels are binned. Note: The function does not check the input, very large values
mean a full row binning (a single sum)}
}
\value{
\strong{\verb{[set_RLum]}}

An object from the class \code{RLum.Data.Spectrum}

\strong{\verb{[get_RLum]}}
\enumerate{
\item A \link{matrix} with the spectrum values or
\item only the info object if \code{info.object} was set.
}

\strong{\verb{[names_RLum]}}

The names of the info objects

\strong{\verb{[bin_RLum.Data]}}

Same object as input, after applying the binning.
}
\description{
Class for representing luminescence spectra data (TL/OSL/RF).
}
\section{Methods (by generic)}{
\itemize{
\item \code{show(RLum.Data.Spectrum)}: Show structure of \code{RLum.Data.Spectrum} object

\item \code{set_RLum(RLum.Data.Spectrum)}: Construction method for RLum.Data.Spectrum object. The \code{info} slot is
optional and by default it is set to an empty list

\item \code{get_RLum(RLum.Data.Spectrum)}: Accessor method for \code{RLum.Data.Spectrum} object.

\item \code{names_RLum(RLum.Data.Spectrum)}: Returns the names info elements coming along with this curve object

\item \code{bin_RLum.Data(RLum.Data.Spectrum)}: Allows binning of RLum.Data.Spectrum data. Count values and values on the x-axis are summed-up;
for wavelength/energy values the mean is calculated.

}}
\section{Slots}{

\describe{
\item{\code{recordType}}{Object of class \link{character} containing the type of the curve (e.g. "TL" or "OSL")}

\item{\code{curveType}}{Object of class \link{character} containing curve type, allowed values are
"measured" or "predefined"}

\item{\code{data}}{Object of class \link{matrix} containing spectrum (count) values.
Row labels indicate wavelength/pixel values, column labels are temperature or time values.}

\item{\code{info}}{Object of class \link{list} containing further meta information objects}
}}

\note{
The class should only contain data for a single spectra data set. For
additional elements the slot \code{info} can be used. Objects from this class are automatically
created by, e.g., \link{read_XSYG2R}
}
\section{Objects from the Class}{

Objects can be created by calls of the form \code{set_RLum("RLum.Data.Spectrum", ...)}.
}

\section{Class version}{
 0.5.2
}

\examples{

showClass("RLum.Data.Spectrum")

##show example data
data(ExampleData.XSYG, envir = environment())
TL.Spectrum

##show data matrix
get_RLum(TL.Spectrum)

##plot spectrum
\dontrun{
plot_RLum(TL.Spectrum)
}

}
\seealso{
\linkS4class{RLum}, \linkS4class{RLum.Data}, \link{plot_RLum}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., 2025. RLum.Data.Spectrum-class(): Class 'RLum.Data.Spectrum'. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{classes}
\keyword{internal}

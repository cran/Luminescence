% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_PSL2Risoe.BINfileData.R
\name{convert_PSL2Risoe.BINfileData}
\alias{convert_PSL2Risoe.BINfileData}
\title{Convert portable OSL data to a Risoe.BINfileData object}
\usage{
convert_PSL2Risoe.BINfileData(object, ...)
}
\arguments{
\item{object}{\linkS4class{RLum.Analysis} (\strong{required}):
\code{RLum.Analysis} object produced by \link{read_PSL2R}}

\item{...}{currently not used.}
}
\value{
Returns an S4 \linkS4class{Risoe.BINfileData} object that can be used to write a
BIN file using \link{write_R2BIN}.
}
\description{
Converts an \code{RLum.Analysis} object produced by the function \code{read_PSL2R()} to
a \code{Risoe.BINfileData} object \strong{(BETA)}.

This function converts an \linkS4class{RLum.Analysis} object that was produced
by the \link{read_PSL2R} function to a \linkS4class{Risoe.BINfileData}.
The \code{Risoe.BINfileData} can be used to write a Risoe BIN file via
\link{write_R2BIN}.
}
\section{Function version}{
 0.0.1
}

\examples{

# (1) load and plot example data set
data("ExampleData.portableOSL", envir = environment())
plot_RLum(ExampleData.portableOSL)

# (2) merge all RLum.Analysis objects into one
merged <- merge_RLum(ExampleData.portableOSL)
merged

# (3) convert to RisoeBINfile object
bin <- convert_PSL2Risoe.BINfileData(merged)
bin

# (4) write Risoe BIN file
\dontrun{
write_R2BIN(bin, "~/portableOSL.binx")
}

}
\seealso{
\linkS4class{RLum.Analysis}, \linkS4class{RLum.Data.Curve},
\linkS4class{Risoe.BINfileData}
}
\author{
Christoph Burow, University of Cologne (Germany)
, RLum Developer Team} 

\section{How to cite}{
Burow, C., 2025. convert_PSL2Risoe.BINfileData(): Convert portable OSL data to a Risoe.BINfileData object. Function version 0.0.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{IO}

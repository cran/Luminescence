% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_Second2Gray.R
\name{convert_Second2Gray}
\alias{convert_Second2Gray}
\title{Converting equivalent dose values from seconds (s) to Gray (Gy)}
\usage{
convert_Second2Gray(data, dose.rate, error.propagation = "omit")
}
\arguments{
\item{data}{\link{data.frame} (\strong{required}):
input values, structure: data (\code{values[,1]}) and data error (\code{values [,2]})
are required}

\item{dose.rate}{\linkS4class{RLum.Results}, \link{data.frame} or \link{numeric} (\strong{required}):
\code{RLum.Results} needs to be originated from the function \link{calc_SourceDoseRate},
for \code{vector} dose rate in Gy/s and dose rate error in Gy/s}

\item{error.propagation}{\link{character} (\emph{with default}):
error propagation method used for error calculation (\code{omit}, \code{gaussian} or
\code{absolute}), see details for further information}
}
\value{
Returns a \link{data.frame} with converted values.
}
\description{
Conversion of absorbed radiation dose in seconds (s) to the SI unit Gray
(Gy) including error propagation. Normally used for equivalent dose data.

Calculation of De values from seconds (s) to Gray (Gy)

\deqn{De [Gy] = De [s] * Dose Rate [Gy/s])}

Provided calculation error propagation methods for error calculation
(with \code{'se'} as the standard error and \code{'DR'} of the dose rate of the beta-source):

\strong{(1) \code{omit}} (default)

\deqn{se(De) [Gy] = se(De) [s] * DR [Gy/s]}

In this case the standard error of the dose rate of the beta-source is
treated as systematic (i.e. non-random), it error propagation is omitted.
However, the error must be considered during calculation of the final age.
(cf. Aitken, 1985, pp. 242). This approach can be seen as method (2) (gaussian)
for the case the (random) standard error of the beta-source calibration is
0. Which particular method is requested depends on the situation and cannot
be prescriptive.

\strong{(2) \code{gaussian}} error propagation

\deqn{se(De) [Gy] = \sqrt((DR [Gy/s] * se(De) [s])^2 + (De [s] * se(DR) [Gy/s])^2)}

Applicable under the assumption that errors of \code{De} and \code{se} are uncorrelated.

\strong{(3) \code{absolute}} error propagation

\deqn{se(De) [Gy]= abs(DR [Gy/s] * se(De) [s]) + abs(De [s] * se(DR) [Gy/s])}

Applicable under the assumption that errors of \code{De} and \code{se} are correlated.
}
\note{
If no or a wrong error propagation method is given, the execution of the function is
stopped. Furthermore, if a \code{data.frame} is provided for the dose rate values is has to
be of the same length as the data frame provided with the argument \code{data}
}
\section{Function version}{
 0.6.0
}

\examples{

##(A) for known source dose rate at date of measurement
## - load De data from the example data help file
data(ExampleData.DeValues, envir = environment())
## - convert De(s) to De(Gy)
convert_Second2Gray(ExampleData.DeValues$BT998, c(0.0438,0.0019))


##(B) for source dose rate calibration data
## - calculate source dose rate first
dose.rate <-  calc_SourceDoseRate(measurement.date = "2012-01-27",
                                  calib.date = "2014-12-19",
                                  calib.dose.rate = 0.0438,
                                  calib.error = 0.0019)
# read example data
data(ExampleData.DeValues, envir = environment())

# apply dose.rate to convert De(s) to De(Gy)
convert_Second2Gray(ExampleData.DeValues$BT998, dose.rate)

} 

\section{How to cite}{
Kreutzer, S., Dietze, M., Fuchs, M.C., 2025. convert_Second2Gray(): Converting equivalent dose values from seconds (s) to Gray (Gy). Function version 0.6.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Aitken, M.J., 1985. Thermoluminescence dating. Academic Press.
}
\seealso{
\link{calc_SourceDoseRate}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Michael Dietze, GFZ Potsdam (Germany)\cr
Margret C. Fuchs, HZDR, Helmholtz-Institute Freiberg for Resource Technology (Germany)
, RLum Developer Team}
\keyword{manip}

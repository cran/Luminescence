% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_Statistics.R
\name{calc_Statistics}
\alias{calc_Statistics}
\title{Function to calculate statistic measures}
\usage{
calc_Statistics(
  data,
  weight.calc = "square",
  digits = NULL,
  n.MCM = NULL,
  na.rm = TRUE
)
}
\arguments{
\item{data}{\link{data.frame} or \linkS4class{RLum.Results} object (\strong{required}):
for \link{data.frame} two columns: De (\code{data[, 1]}) and De error (\code{data[, 2]}).}

\item{weight.calc}{\link{character} (\emph{with default}):
type of weight calculation. One out of \code{"reciprocal"} (weight is 1/error),
\code{"square"} (weight is 1/error^2). Default is \code{"square"}.}

\item{digits}{\link{integer} (\emph{with default}):
number of decimal places to be used when rounding numbers. If set to \code{NULL}
(default), no rounding occurs.}

\item{n.MCM}{\link{numeric} (\emph{with default}):
number of samples drawn for Monte Carlo-based statistics.
\code{NULL} (the default) disables MC runs.}

\item{na.rm}{\link{logical} (\emph{with default}):
indicating whether \code{NA} values should be stripped before the computation proceeds.}
}
\value{
Returns a list with weighted and unweighted statistic measures.
}
\description{
This function calculates a number of descriptive statistics for estimates
with a given standard error (SE), most fundamentally using error-weighted approaches.
}
\details{
The option to use Monte Carlo Methods (\code{n.MCM}) allows calculating
all descriptive statistics based on random values. The distribution of these
random values is based on the Normal distribution with \code{De} values as
means and \code{De_error} values as one standard deviation. Increasing the
number of MCM-samples linearly increases computation time. On a Lenovo X230
machine evaluation of 25 Aliquots with n.MCM = 1000 takes 0.01 s, with
n = 100000, ca. 1.65 s. It might be useful to work with logarithms of these
values. See Dietze et al. (2016, Quaternary Geochronology) and the function
\link{plot_AbanicoPlot} for details.
}
\section{Function version}{
 0.1.7
}

\examples{

## load example data
data(ExampleData.DeValues, envir = environment())

## show a rough plot of the data to illustrate the non-normal distribution
plot_KDE(ExampleData.DeValues$BT998)

## calculate statistics and show output
str(calc_Statistics(ExampleData.DeValues$BT998))

\dontrun{
## now the same for 10000 normal distributed random numbers with equal errors
x <- as.data.frame(cbind(rnorm(n = 10^5, mean = 0, sd = 1),
                         rep(0.001, 10^5)))

## note the congruent results for weighted and unweighted measures
str(calc_Statistics(x))
}

}
\author{
Michael Dietze, GFZ Potsdam (Germany)
, RLum Developer Team} 

\section{How to cite}{
Dietze, M., 2025. calc_Statistics(): Function to calculate statistic measures. Function version 0.1.7. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{datagen}

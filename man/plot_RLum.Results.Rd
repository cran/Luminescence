% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_RLum.Results.R
\name{plot_RLum.Results}
\alias{plot_RLum.Results}
\title{Plot function for an RLum.Results S4 class object}
\usage{
plot_RLum.Results(object, single = TRUE, ...)
}
\arguments{
\item{object}{\linkS4class{RLum.Results} (\strong{required}):
S4 object of class \code{RLum.Results}}

\item{single}{\link{logical} (\emph{with default}):
single plot output (\code{TRUE/FALSE}) to allow for plotting the results in as
few plot windows as possible.}

\item{...}{further arguments and graphical parameters will be passed to
the \code{plot} function.}
}
\value{
Returns multiple plots.
}
\description{
The function provides a standardised plot output for data of an RLum.Results
S4 class object
}
\details{
The function produces a multiple plot output.  A file output is recommended
(e.g., \link{pdf}).
}
\note{
Not all arguments available for \link{plot} will be passed!
Only plotting of \code{RLum.Results} objects are supported.
}
\section{Function version}{
 0.2.1
}

\examples{


###load data
data(ExampleData.DeValues, envir = environment())

# apply the un-logged minimum age model
mam <- calc_MinDose(data = ExampleData.DeValues$CA1, sigmab = 0.2, log = TRUE, plot = FALSE)

##plot
plot_RLum.Results(mam)

# estimate the number of grains on an aliquot
grains<- calc_AliquotSize(grain.size = c(100,150), sample.diameter = 1, plot = FALSE, MC.iter = 100)

##plot
plot_RLum.Results(grains)


}
\seealso{
\link{plot}, \link{plot_RLum}
}
\author{
Christoph Burow, University of Cologne (Germany) \cr
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Burow, C., Kreutzer, S., 2025. plot_RLum.Results(): Plot function for an RLum.Results S4 class object. Function version 0.2.1. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{aplot}

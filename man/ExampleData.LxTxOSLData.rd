% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Luminescence-package.R
\name{ExampleData.LxTxOSLData}
\alias{ExampleData.LxTxOSLData}
\title{Example Lx and Tx curve data from an artificial OSL measurement}
\format{Two \code{data.frames} containing time and count values.}
\source{
Arbitrary OSL measurement.
}
\description{
Lx and Tx data of continous wave (CW-) OSL signal curves.
}
\examples{
##load data
data(ExampleData.LxTxOSLData, envir = environment())

##plot data
plot(Lx.data)
plot(Tx.data)
}
\references{
unpublished data
}


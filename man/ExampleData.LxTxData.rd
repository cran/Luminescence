% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/Luminescence-package.R
\name{ExampleData.LxTxData}
\alias{ExampleData.LxTxData}
\title{Example Lx/Tx data from CW-OSL SAR measurement}
\format{A \code{data.frame} with 4 columns (Dose, LxTx, LxTx.Error, TnTx).}
\source{
\tabular{ll}{ Lab: \tab Luminescence Laboratory Bayreuth\cr Lab-Code: \tab
BT607\cr Location: \tab Ostrau (Saxony-Anhalt/Germany)\cr Material: \tab
Middle grain (38-63 \eqn{\mu}m) quartz measured on a Risoe TL/OSL DA-15
reader.\cr }
}
\description{
LxTx data from a SAR measurement for the package Luminescence.
}
\examples{
##plot Lx/Tx data vs dose [s]
data(ExampleData.LxTxData, envir = environment())
plot(LxTxData$Dose,LxTxData$LxTx)
}
\references{
unpublished data
}


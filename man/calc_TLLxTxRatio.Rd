% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_TLLxTxRatio.R
\name{calc_TLLxTxRatio}
\alias{calc_TLLxTxRatio}
\title{Calculate the Lx/Tx ratio for a given set of TL curves -beta version-}
\usage{
calc_TLLxTxRatio(
  Lx.data.signal,
  Lx.data.background = NULL,
  Tx.data.signal,
  Tx.data.background = NULL,
  signal.integral.min,
  signal.integral.max
)
}
\arguments{
\item{Lx.data.signal}{\linkS4class{RLum.Data.Curve} or \link{data.frame} (\strong{required}):
TL data (x = temperature, y = counts) (TL signal)}

\item{Lx.data.background}{\linkS4class{RLum.Data.Curve} or \link{data.frame} (\emph{optional}):
TL data (x = temperature, y = counts).
If no data are provided no background subtraction is performed.}

\item{Tx.data.signal}{\linkS4class{RLum.Data.Curve} or \link{data.frame} (\strong{required}):
TL data (x = temperature, y = counts) (TL test signal)}

\item{Tx.data.background}{\linkS4class{RLum.Data.Curve} or \link{data.frame} (\emph{optional}):
TL data (x = temperature, y = counts).
If no data are provided no background subtraction is performed.}

\item{signal.integral.min}{\link{integer} (\strong{required}):
channel number for the lower signal integral bound
(e.g. \code{signal.integral.min = 100})}

\item{signal.integral.max}{\link{integer} (\strong{required}):
channel number for the upper signal integral bound
(e.g. \code{signal.integral.max = 200})}
}
\value{
Returns an S4 object of type \linkS4class{RLum.Results}.
Slot \code{data} contains a \link{list} with the following structure:

\if{html}{\out{<div class="sourceCode">}}\preformatted{$ LxTx.table
.. $ LnLx
.. $ LnLx.BG
.. $ TnTx
.. $ TnTx.BG
.. $ Net_LnLx
.. $ Net_LnLx.Error
}\if{html}{\out{</div>}}
}
\description{
Calculate Lx/Tx ratio for a given set of TL curves.
}
\details{
\strong{Uncertainty estimation}

The standard errors are calculated using the following generalised equation:

\deqn{SE_{signal} = abs(Signal_{net} * BG_f /BG_{signal})}

where \eqn{BG_f} is a term estimated by calculating the standard deviation of the sum of
the \eqn{L_x} background counts and the sum of the \eqn{T_x} background counts. However,
if both signals are similar the error becomes zero.
}
\note{
\strong{This function has still BETA status!} Please further note that a similar
background for both curves results in a zero error and is therefore set to \code{NA}.
}
\section{Function version}{
 0.3.3
}

\examples{

##load package example data
data(ExampleData.BINfileData, envir = environment())

##convert Risoe.BINfileData into a curve object
temp <- Risoe.BINfileData2RLum.Analysis(TL.SAR.Data, pos = 3)


Lx.data.signal <- get_RLum(temp, record.id=1)
Lx.data.background <- get_RLum(temp, record.id=2)
Tx.data.signal <- get_RLum(temp, record.id=3)
Tx.data.background <- get_RLum(temp, record.id=4)
signal.integral.min <- 210
signal.integral.max <- 230

output <- calc_TLLxTxRatio(
 Lx.data.signal,
 Lx.data.background,
 Tx.data.signal,
 Tx.data.background,
 signal.integral.min,
 signal.integral.max)
get_RLum(output)

}
\seealso{
\linkS4class{RLum.Results}, \link{analyse_SAR.TL}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
Christoph Schmidt, University of Bayreuth (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., Schmidt, C., 2025. calc_TLLxTxRatio(): Calculate the Lx/Tx ratio for a given set of TL curves -beta version-. Function version 0.3.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{datagen}

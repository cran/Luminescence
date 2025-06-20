% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_OSLLxTxDecomposed.R
\name{calc_OSLLxTxDecomposed}
\alias{calc_OSLLxTxDecomposed}
\title{Calculate Lx/Tx ratio for decomposed CW-OSL signal components}
\usage{
calc_OSLLxTxDecomposed(
  Lx.data,
  Tx.data = NULL,
  OSL.component = 1L,
  sig0 = 0,
  digits = NULL
)
}
\arguments{
\item{Lx.data}{\link{data.frame} (\strong{required}): Component table created by
\code{OSLdecomposition::RLum.OSL_decomposition} and per default located
at \code{object@records[[...]]@info$COMPONENTS}.The value of \verb{$n[OSL.component]}
is set as \code{LnLx}. The value of \verb{$n.error[OSL.component]} is set as \code{LnLx.error}}

\item{Tx.data}{\link{data.frame} (\emph{optional}): Component table created by
\code{OSLdecomposition::RLum.OSL_decomposition} and per default located at
\code{object@records[[...]]@info$COMPONENTS}. The value of \verb{$n[OSL.component]}
is set as \code{TnTx}. The value of \verb{$n.error[OSL.component]} is set as \code{TnTx.error}}

\item{OSL.component}{\link{integer} or \link{character} (\emph{optional}):
a single index or a name describing which OSL signal component shall be evaluated.
This argument can either be the name of the OSL component assigned by
\code{OSLdecomposition::RLum.OSL_global_fitting} or the index of component.
Then \code{'1'} selects the fastest decaying component, \code{'2'} the
second fastest and so on. If not defined, the fastest decaying component is selected.}

\item{sig0}{\link{numeric} (\emph{with default}): allows adding an extra error component
to the final \code{Lx/Tx} error value (e.g., instrumental error).}

\item{digits}{\link{integer} (\emph{with default}): round numbers to the specified digits.
If digits is set to \code{NULL} nothing is rounded.}
}
\value{
Returns an S4 object of type \linkS4class{RLum.Results}.

Slot \code{data} contains a \link{list} with the following structure:

\strong{@data}

\if{html}{\out{<div class="sourceCode">}}\preformatted{$LxTx.table (data.frame)
.. $ LnLx
.. $ TnTx
.. $ Net_LnLx
.. $ Net_LnLx.Error
.. $ Net_TnTx
.. $ Net_TnTx.Error
.. $ LxTx
.. $ LxTx.relError
.. $ LxTx.Error
}\if{html}{\out{</div>}}
}
\description{
Calculate \code{Lx/Tx} ratios from a given set of decomposed
CW-OSL curves decomposed by \code{OSLdecomposition::RLum.OSL_decomposition}.
}
\section{Function version}{
 0.1.0
}
 

\section{How to cite}{
Mittelstrass, D., 2025. calc_OSLLxTxDecomposed(): Calculate Lx/Tx ratio for decomposed CW-OSL signal components. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Mittelstrass D., Schmidt C., Beyer J., Straessner A., 2019.
Automated identification and separation of quartz CW-OSL signal components with R.
talk presented at DLED 2019, Bingen, Germany
\url{http://luminescence.de/OSLdecomp_talk.pdf}\cr
}
\seealso{
\linkS4class{RLum.Data.Curve}, \link{fit_DoseResponseCurve}, \link{analyse_SAR.CWOSL}
}
\author{
Dirk Mittelstrass
, RLum Developer Team}
\keyword{datagen}

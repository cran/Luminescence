\name{calc_OSLLxTxRatio}
\alias{calc_OSLLxTxRatio}
\title{Calculate Lx/Tx ratio for CW-OSL curves.}
\description{Calculate Lx/Tx ratios from two given OSL curves. }
\usage{calc_OSLLxTxRatio(Lx.data, Tx.data, signal.integral, background.integral)}
\arguments{
  \item{Lx.data}{\link{data.frame} (\bold{required}): requires a CW-OSL shine down curve 
(x = time, y = counts)}
  \item{Tx.data}{\link{data.frame} (optional): requires a CW-OSL shine down curve 
(x = time, y = counts). If no input is given the Tx.data will 
be treated as \code{NA} and no Lx/Tx ratio is calculated.}
  \item{signal.integral}{\link{vector} (\bold{required}): vector with the limits 
for the signal integral.}
  \item{background.integral}{\link{vector} (\bold{required}): vector with the bounds for the 
background integral.}
}
\details{The integrity of the chosen values for the signal and background integral 
is checked by the function; 
the signal integral limits have to be lower than the background integral limits. 
If a \link{vector} is given as input instead of a \link{data.frame}, 
an artificial \code{data.frame}
is produced. The error calculation is done according to Galbraith (2002).}
\value{
Returns an S4 object of type \code{\linkS4class{RLum.Results}}. 
Slot \code{data} containt a \link{data.frame} with the following structure:\cr 
$ LnLx  \cr        
$ LnLx.BG   \cr     
$ TnTx    \cr       
$ TnTx.BG    \cr   
$ Net_LnLx   \cr   
$ Net_LnLx.Error\cr 
$ Net_TnTx.Error}
\references{Duller, G., 2007. Analyst. \url{http://www.nutech.dtu.dk/Produkter/Dosimetri
/NUK_instruments/TL_OSL_readers/~/media/Risoe_dk/Erhvervskontakt/NUK/Documents
/Analyst\%20Manual\%20v3\%2022b.ashx}\cr\cr 
Galbraith, R.F., 2002. A note on the variance of a background-corrected 
OSL count. Ancient TL, 20 (2), 49-51. }
\author{Sebastian Kreutzer, JLU Giessen (Germany)
R Luminescence Package Team}
\note{The results of this function have been cross-checked with the Analyst (vers. 3.24b).   
Access to the results object via \code{get_RLum.Results}.}


\seealso{\code{\link{Analyse_SAR.OSLdata}}, \code{\link{plot_GrowthCurve}} }
\examples{

##load data
data(ExampleData.LxTxOSLData, envir = environment())

##calculate Lx/Tx ratio
results <- calc_OSLLxTxRatio(Lx.data, Tx.data, signal.integral = c(1:2), 
                           background.integral = c(85:100))

}

\keyword{datagen}

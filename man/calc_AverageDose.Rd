% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_AverageDose.R
\name{calc_AverageDose}
\alias{calc_AverageDose}
\title{Calculate the Average Dose and the dose rate dispersion}
\usage{
calc_AverageDose(
  data,
  sigma_m,
  Nb_BE = 500,
  na.rm = TRUE,
  plot = TRUE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{data}{\linkS4class{RLum.Results} or \link{data.frame} (\strong{required}):
for \link{data.frame}: two columns with \code{De} \code{(data[,1])} and \verb{De error} \code{(values[,2])}}

\item{sigma_m}{\link{numeric} (\strong{required}):
the overdispersion resulting from a dose recovery
experiment, i.e. when all grains have  received the same dose. Indeed in such a case, any
overdispersion (i.e. dispersion on top of analytical uncertainties) is, by definition, an
unrecognised measurement uncertainty.}

\item{Nb_BE}{\link{integer} (\emph{with default}):
sample size used for the bootstrapping}

\item{na.rm}{\link{logical} (\emph{with default}):
exclude NA values from the data set prior to any further operation.}

\item{plot}{\link{logical} (\emph{with default}):
enable/disable the plot output.}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{...}{further arguments that can be passed to \link[graphics:hist]{graphics::hist}. As three plots
are returned all arguments need to be provided as \link{list},
e.g., \code{main = list("Plot 1", "Plot 2", "Plot 3")}.
Note: not all arguments of \code{hist} are
supported, but the output of \code{hist} is returned and can be used of own plots. \cr

Further supported arguments: \code{mtext} (\link{character}), \code{rug} (\code{TRUE/FALSE}).}
}
\value{
The function returns numerical output and an (\emph{optional}) plot.

-----------------------------------\cr
\verb{[ NUMERICAL OUTPUT ]} \cr
-----------------------------------\cr
\strong{\code{RLum.Results}}-object\cr

\strong{slot:} \strong{\verb{@data}} \cr

\verb{[.. $summary : data.frame]}\cr

\tabular{lll}{
\strong{Column} \tab \strong{Type} \tab \strong{Description}\cr
AVERAGE_DOSE \tab \link{numeric} \tab the obtained average dose\cr
AVERAGE_DOSE.SE \tab \link{numeric} \tab the average dose error \cr
SIGMA_D \tab \link{numeric}\tab sigma \cr
SIGMA_D.SE \tab \link{numeric}\tab standard error of the sigma  \cr
IC_AVERAGE_DOSE.LEVEL  \tab \link{character}\tab confidence level average dose\cr
IC_AVERAGE_DOSE.LOWER  \tab \link{character}\tab lower quantile of average dose \cr
IC_AVERAGE_DOSE.UPPER \tab \link{character}\tab upper quantile of average dose\cr
IC_SIGMA_D.LEVEL \tab \link{integer}\tab confidence level sigma\cr
IC_SIGMA_D.LOWER \tab \link{character}\tab lower sigma quantile\cr
IC_SIGMA_D.UPPER \tab \link{character}\tab upper sigma quantile\cr
L_MAX \tab \link{character}\tab maximum likelihood value
}

\verb{[.. $dstar : matrix]} \cr

Matrix with bootstrap values\cr

\verb{[.. $hist : list]}\cr

Object as produced by the function histogram

------------------------\cr
\verb{[ PLOT OUTPUT ]}\cr
------------------------\cr

The function returns two different plot panels.

(1) An abanico plot with the dose values

(2) A histogram panel comprising 3 histograms with the equivalent dose and the bootstrapped average
dose and the sigma values.
}
\description{
This functions calculates the Average Dose and its extrinsic dispersion,
estimating the standard errors by bootstrapping based on the Average
Dose Model by Guerin et al., 2017.

\strong{\code{sigma_m}}\cr

The program requires the input of a known value of \code{sigma_m},
which corresponds to the intrinsic overdispersion, as determined
by a dose recovery experiment. Then the dispersion in doses (\code{sigma_d})
will be that over and above \code{sigma_m} (and individual uncertainties \code{sigma_wi}).
}
\note{
This function has beta status!
}
\section{Function version}{
 0.1.6
}

\examples{

##Example 01 using package example data
##load example data
data(ExampleData.DeValues, envir = environment())

##calculate Average dose
##(use only the first 56 values here)
AD <- calc_AverageDose(ExampleData.DeValues$CA1[1:56,], sigma_m = 0.1)

##plot De and set Average dose as central value
plot_AbanicoPlot(
 data = ExampleData.DeValues$CA1[1:56,],
 z.0 = AD$summary$AVERAGE_DOSE)

} 

\section{How to cite}{
Christophe, C., Philippe, A., Kreutzer, S., 2025. calc_AverageDose(): Calculate the Average Dose and the dose rate dispersion. Function version 0.1.6. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Guerin, G., Christophe, C., Philippe, A., Murray, A.S., Thomsen, K.J., Tribolo, C., Urbanova, P.,
Jain, M., Guibert, P., Mercier, N., Kreutzer, S., Lahaye, C., 2017. Absorbed dose, equivalent dose,
measured dose rates, and implications for OSL age estimates: Introducing the Average Dose Model.
Quaternary Geochronology 1-32. doi:10.1016/j.quageo.2017.04.002

\strong{Further reading}\cr

Efron, B., Tibshirani, R., 1986. Bootstrap Methods for Standard Errors, Confidence Intervals,
and Other Measures of Statistical Accuracy. Statistical Science 1, 54-75.
}
\seealso{
\link{read.table}, \link[graphics:hist]{graphics::hist}
}
\author{
Claire Christophe, IRAMAT-CRP2A, Université de Nantes (France),
Anne Philippe, Université de Nantes, (France),
Guillaume Guérin, IRAMAT-CRP2A, Université Bordeaux Montaigne, (France),
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team}
\keyword{datagen}

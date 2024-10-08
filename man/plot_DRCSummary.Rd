% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_DRCSummary.R
\name{plot_DRCSummary}
\alias{plot_DRCSummary}
\title{Create a Dose-Response Curve Summary Plot}
\usage{
plot_DRCSummary(
  object,
  source_dose_rate = NULL,
  sel_curves = NULL,
  show_dose_points = FALSE,
  show_natural = FALSE,
  n = 51L,
  ...
)
}
\arguments{
\item{object}{\linkS4class{RLum.Results} object (\strong{required}): input object created by the function \link{analyse_SAR.CWOSL}. The input object can be provided as \link{list}.}

\item{source_dose_rate}{\link{numeric} (\emph{optional}): allows to modify the axis and show values in Gy, instead seconds. Only a single numerical value is allowed.}

\item{sel_curves}{\link{numeric} (optional): id of the curves to be plotting in its occurring order. A sequence can
be provided for selecting, e.g., only every 2nd curve from the input object}

\item{show_dose_points}{\link{logical} (with default): enable or disable plot of dose points in the graph}

\item{show_natural}{\link{logical} (with default): enable or disable the plot of the natural \code{Lx/Tx} values}

\item{n}{\link{integer} (with default): the number of x-values used to evaluate one curve object. Large numbers slow
down the plotting process and are usually not needed}

\item{...}{Further arguments and graphical parameters to be passed. In particular: \code{main}, \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim}, \code{lty}, \code{lwd}, \code{pch}, \code{col.pch}, \code{col.lty}, \code{mtext}}
}
\value{
An \linkS4class{RLum.Results} object is returned:

Slot: \strong{@data}\cr

\tabular{lll}{
\strong{OBJECT} \tab \strong{TYPE} \tab \strong{COMMENT}\cr
\code{results} \tab \link{data.frame} \tab with dose and LxTx values \cr
\code{data} \tab \linkS4class{RLum.Results} \tab original input data \cr
}

Slot: \strong{@info}\cr

\tabular{lll}{
\strong{OBJECT} \tab \strong{TYPE} \tab \strong{COMMENT} \cr
\code{call} \tab \code{call} \tab the original function call \cr
\code{args} \tab \code{list} \tab arguments of the original function call \cr
}

\emph{Note: If the input object is a \link{list} a list of \linkS4class{RLum.Results} objects is returned.}
}
\description{
While analysing OSL SAR or pIRIR-data the view on the data is limited usually to one
dose-response curve (DRC) at the time for one aliquot. This function overcomes this limitation
by plotting all DRC from an \linkS4class{RLum.Results} object created by the function \link{analyse_SAR.CWOSL}
in one single plot.
}
\details{
If you want plot your DRC on an energy scale (dose in Gy), you can either use the option \code{source_dose_rate} provided
below or your can SAR analysis with the dose points in Gy (better axis scaling).
}
\section{Function version}{
 0.2.3
}

\examples{

#load data example data
data(ExampleData.BINfileData, envir = environment())

#transform the values from the first position in a RLum.Analysis object
object <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

results <- analyse_SAR.CWOSL(
  object = object,
  signal.integral.min = 1,
  signal.integral.max = 2,
   background.integral.min = 900,
   background.integral.max = 1000,
   plot = FALSE
 )

##plot only DRC
plot_DRCSummary(results)

}
\seealso{
\linkS4class{RLum.Results}, \link{analyse_SAR.CWOSL}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
Christoph Burow, University of Cologne (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., Burow, C., 2024. plot_DRCSummary(): Create a Dose-Response Curve Summary Plot. Function version 0.2.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., 2024. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 0.9.25. https://r-lum.github.io/Luminescence/
}


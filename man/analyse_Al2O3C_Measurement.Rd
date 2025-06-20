% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_Al2O3C_Measurement.R
\name{analyse_Al2O3C_Measurement}
\alias{analyse_Al2O3C_Measurement}
\title{Al2O3:C Passive Dosimeter Measurement Analysis}
\usage{
analyse_Al2O3C_Measurement(
  object,
  signal_integral = NULL,
  dose_points = c(0, 4),
  recordType = c("OSL (UVVIS)", "TL (UVVIS)"),
  calculate_TL_dose = FALSE,
  irradiation_time_correction = NULL,
  cross_talk_correction = NULL,
  travel_dosimeter = NULL,
  test_parameters = NULL,
  verbose = TRUE,
  plot = TRUE,
  ...
)
}
\arguments{
\item{object}{\linkS4class{RLum.Analysis} (\strong{required}):  measurement input}

\item{signal_integral}{\link{numeric} (\emph{optional}): signal integral, used for the signal
and the background. Example: \code{c(1:10)} for the first 10 channels.
If nothing is provided the full range is used}

\item{dose_points}{\link{numeric} (\emph{with default}):
vector with dose points, if dose points are repeated, only the general
pattern needs to be provided. Default values follow the suggestions
made by Kreutzer et al., 2018}

\item{recordType}{\link{character} (\emph{with default}): input curve selection, which is passed to
function \link{get_RLum}. To deactivate the automatic selection set the argument to \code{NULL}}

\item{calculate_TL_dose}{\link{logical} (\emph{with default}):
enables/disable experimental dose estimation based on the TL curves.
It is computed as the ratio of the peak sums of each curves +/- 5 channels.}

\item{irradiation_time_correction}{\link{numeric} or \linkS4class{RLum.Results} (\emph{optional}):
information on the used irradiation time correction obtained by another experiments.
If a \code{numeric} is provided it has to be of length two: mean, standard error}

\item{cross_talk_correction}{\link{numeric} or \linkS4class{RLum.Results} (\emph{optional}):
information on the used irradiation time correction obtained by another experiments.
If a \code{numeric} vector is provided it has to be of length three:
mean, 2.5 \% quantile, 97.5 \% quantile.}

\item{travel_dosimeter}{\link{numeric} (\emph{optional}): specify the position of
the travel dosimeter (so far measured at the same time). The dose of
travel dosimeter will be subtracted from all other values.}

\item{test_parameters}{\link{list} (\emph{with default}):
set test parameters. Supported parameters are: \code{TL_peak_shift} All input: \link{numeric}
values, \code{NA} and \code{NULL} (see details).}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{plot}{\link{logical} (\emph{with default}): enable/disable the plot output.
If \code{object} is of type \link{list}, a numeric vector can be provided to limit
the plot output to certain aliquots.}

\item{...}{further arguments that can be passed to the plot output, supported are \code{norm}, \code{main}, \code{mtext},
\code{title} (for self-call mode to specify, e.g., sample names)}
}
\value{
Function returns results numerically and graphically:

-----------------------------------\cr
\verb{[ NUMERICAL OUTPUT ]}\cr
-----------------------------------\cr

\strong{\code{RLum.Results}}-object

\strong{slot:} \strong{\verb{@data}}

\tabular{lll}{
\strong{Element} \tab \strong{Type} \tab \strong{Description}\cr
\verb{$data} \tab \code{data.frame} \tab the estimated equivalent dose \cr
\verb{$data_table} \tab \code{data.frame} \tab full dose and signal table \cr
\code{test_parameters} \tab \code{data.frame} \tab results with test parameters \cr
\code{data_TDcorrected} \tab \code{data.frame} \tab travel dosimeter corrected results (only if TD was provided)\cr
}

\emph{Note: If correction the irradiation time and the cross-talk correction method is used, the De
values in the table \code{data} table are already corrected, i.e. if you want to get an uncorrected value,
you can use the column \code{CT_CORRECTION} remove the correction}

\strong{slot:} \strong{\verb{@info}}

The original function call

------------------------\cr
\verb{[ PLOT OUTPUT ]}\cr
------------------------\cr
\itemize{
\item OSL and TL curves, combined on two plots.
}
}
\description{
The function provides the analysis routines for measurements on a
FI lexsyg SMART reader using Al2O3:C chips according to Kreutzer et al., 2018
}
\details{
\strong{Working with a travel dosimeter}

The function allows to define particular position numbers as travel
dosimeters. For example: \code{travel_dosimeter = c(1,3,5)} sets positions 1,
3 and 5 as travel dosimeters. These dose values #' of this dosimeters
are combined and automatically subtracted from the obtained dose values
of the other dosimeters.

\strong{Calculate TL dose}

The argument \code{calculate_TL_dose} provides the possibility to experimentally calculate a TL-dose,
i.e. an apparent dose value derived from the TL curve ratio. However, it should be noted that
this value is only a fall back in case something went wrong during the measurement of the optical
stimulation. The TL derived dose value is corrected for cross-talk and for the irradiation time,
but not considered if a travel dosimeter is defined.

Calculating the palaeodose is possible without \strong{any TL} curve in the sequence!

\strong{Test parameters}

\code{TL_peak_shift} \link{numeric} (default: \code{15}):

Checks whether the TL peak shift is bigger > 15 K, indicating a problem with the
thermal contact of the chip.

\code{stimulation_power} \link{numeric} (default: \code{0.05}):

So far available, information on the delivered optical stimulation are compared. Compared are
the information from the first curves with all others. If the ratio differs more from
unity than the defined by the threshold, a warning is returned.
}
\section{Function version}{
 0.2.6
}

\examples{
##load data
data(ExampleData.Al2O3C, envir = environment())

##run analysis
analyse_Al2O3C_Measurement(data_CrossTalk)

} 

\section{How to cite}{
Kreutzer, S., 2025. analyse_Al2O3C_Measurement(): Al2O3:C Passive Dosimeter Measurement Analysis. Function version 0.2.6. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Kreutzer, S., Martin, L., Guérin, G., Tribolo, C., Selva, P., Mercier, N., 2018. Environmental Dose Rate
Determination Using a Passive Dosimeter: Techniques and Workflow for alpha-Al2O3:C Chips.
Geochronometria 45, 56-67.
}
\seealso{
\link{analyse_Al2O3C_ITC}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team}
\keyword{datagen}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyse_FadingMeasurement.R
\name{analyse_FadingMeasurement}
\alias{analyse_FadingMeasurement}
\title{Analyse fading measurements and returns the fading rate per decade (g-value)}
\usage{
analyse_FadingMeasurement(
  object,
  structure = c("Lx", "Tx"),
  signal.integral,
  background.integral,
  t_star = "half",
  n.MC = 100,
  verbose = TRUE,
  plot = TRUE,
  plot_singlePanels = FALSE,
  ...
)
}
\arguments{
\item{object}{\linkS4class{RLum.Analysis} (\strong{required}):
input object with the measurement data. Alternatively, a \link{list} containing \linkS4class{RLum.Analysis}
objects or a \link{data.frame} with three columns
(x = LxTx, y = LxTx error, z = time since irradiation) can be provided.
Can also be a wide table, i.e. a \link{data.frame} with a number of columns divisible by 3
and where each triplet has the before mentioned column structure.

\strong{Please note: The input object should solely consists of the curve needed
for the data analysis, i.e. only IRSL curves representing Lx (and Tx). If
the object originated from an XSYG file, also the irradiation steps must
be preserved in the input object.}

If data from multiple aliquots are provided please \strong{see the details below} with regard to
Lx/Tx normalisation. \strong{The function assumes that all your measurements are related to
one (comparable) sample. If you have to treat independent samples, you have use this function
in a loop.}}

\item{structure}{\link{character} (\emph{with default}):
sets the structure of the measurement data. Allowed are \code{'Lx'} or \code{c('Lx','Tx')}.
Other input is ignored}

\item{signal.integral}{\link{vector} (\strong{required}): vector with channels for the signal integral
(e.g., \code{c(1:10)}). Not required if a \code{data.frame} with \code{LxTx} values is provided.}

\item{background.integral}{\link{vector} (\strong{required}): vector with channels for the background integral
(e.g., \code{c(90:100)}). Not required if a \code{data.frame} with \code{LxTx} values is provided.}

\item{t_star}{\link{character} (\emph{with default}):
method for calculating the time elapsed since irradiation if input is \strong{not} a \code{data.frame}.
Options are: \code{'half'} (the default), \verb{'half_complex}, which uses the long equation in Auclair et al. 2003, and
and \code{'end'}, which takes the time between irradiation and the measurement step.
Alternatively, \code{t_star} can be a function with one parameter which works on \code{t1}.
For more information see details. \cr

\emph{\code{t_star} has no effect if the input is a \link{data.frame}, because this input comes
without irradiation times.}}

\item{n.MC}{\link{integer} (\emph{with default}):
number for Monte Carlo runs for the error estimation.}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{plot}{\link{logical} (\emph{with default}):
enable/disable the plot output.}

\item{plot_singlePanels}{\link{logical} (\emph{with default}) or \link{numeric} (\emph{optional}):
enable/disable single plot mode, i.e. one plot window per plot.
Alternatively a vector specifying the plot to be drawn, e.g.,
\code{plot_singlePanels = c(3,4)} draws only the last two plots in separate
windows.}

\item{...}{(\emph{optional}) further arguments that can be passed to internally used functions. Supported arguments:
\code{xlab}, \code{log}, \code{mtext}, \code{plot.trend} (enable/disable trend blue line), and \code{xlim} for the
two first curve plots, and \code{ylim} for the fading
curve plot. For further plot customization please use the numerical output of the functions for
own plots.}
}
\value{
An \linkS4class{RLum.Results} object is returned:

Slot: \strong{@data}

\tabular{lll}{
\strong{OBJECT} \tab \strong{TYPE} \tab \strong{COMMENT}\cr
\code{fading_results} \tab \code{data.frame} \tab results of the fading measurement in a table \cr
\code{fit} \tab \code{lm} \tab object returned by the used linear fitting function \link[stats:lm]{stats::lm}\cr
\code{rho_prime} \tab \code{data.frame} \tab results of rho' estimation after Kars et al. (2008) \cr
\code{LxTx_table} \tab \code{data.frame} \tab Lx/Tx table, if curve data had been provided \cr
\code{irr.times} \tab \code{integer} \tab vector with the irradiation times in seconds \cr
}

Slot: \strong{@info}

\tabular{lll}{
\strong{OBJECT} \tab \code{TYPE} \tab \code{COMMENT}\cr
\code{call} \tab \code{call} \tab the original function call\cr
}
}
\description{
The function analyses fading measurements and returns a fading rate
including an error estimation. The function is not limited to standard
fading measurements, as can be seen, e.g., Huntley and Lamothe (2001).
Additionally, the density of recombination centres (rho') is estimated after
Kars et al. (2008).
}
\details{
All provided output corresponds to the \eqn{tc} value obtained by this analysis. Additionally
in the output object the g-value normalised to 2-days is provided. The output of this function
can be passed to the function \link{calc_FadingCorr}.

\strong{Fitting and error estimation}

For the fitting the function \link[stats:lm]{stats::lm} is used without applying weights. For the
error estimation all input values, except \code{tc}, as the precision can be considered as sufficiently
high enough with regard to the underlying problem, are sampled assuming a normal distribution
for each value with the value as the mean and the provided uncertainty as standard deviation.

\strong{The options for \code{t_star}}

\itemize{
\item \code{t_star = "half"} (the default) The calculation follows the simplified
version in Auclair et al. (2003), which reads
\deqn{t_{star} := t_1 + (t_2 - t_1)/2}
\item \code{t_star = "half_complex"} This option applies the complex function shown in Auclair et al. (2003),
which is derived from Aitken (1985) appendix F, equations 9 and 11.
It reads \deqn{t_{star} = t0 * 10^[(t_2 log(t_2/t_0) - t_1 log(t_1/t_0) - 0.43(t_2 - t_1))/(t_2 - t_1)]}
where 0.43 = \eqn{1/ln(10)}. t0, which is an arbitrary constant, is set to 1.
Please note that the equation in Auclair et al. (2003) is incorrect
insofar that it reads \eqn{10exp(...)}, where the base should be 10 and not the Euler's number.
Here we use the correct version (base 10).
\item \code{t_star = "end"} This option uses the simplest possible form for \code{t_star} which is the time since
irradiation without taking into account any addition parameter and it equals t1 in Auclair et al. (2003)
\item \verb{t_star = <function>} This last option allows you to provide an R function object that works on t1 and
gives you all possible freedom. For instance, you may want to define the following
function \code{fun <- function(x) {x^2}}, this would square all values of t1, because internally
it calls \code{fun(t1)}. The name of the function does not matter.
}

\strong{Density of recombination centres}

The density of recombination centres, expressed by the dimensionless variable rho', is estimated
by fitting equation 5 in Kars et al. (2008) to the data. For the fitting the function
\link[stats:nls]{stats::nls} is used without applying weights. For the error estimation the same
procedure as for the g-value is applied (see above).

\strong{Multiple aliquots & Lx/Tx normalisation}

Be aware that this function will always normalise all \eqn{\frac{L_x}{T_x}} values
by the \eqn{\frac{L_x}{T_x}} value of the
prompt measurement of the first aliquot. This implicitly assumes that there are no systematic
inter-aliquot variations in the \eqn{\frac{L_x}{T_x}} values.
If deemed necessary to normalise the \eqn{\frac{L_x}{T_x}} values  of each
aliquot by its individual prompt measurement please do so \strong{before} running
\link{analyse_FadingMeasurement} and provide the already normalised values for \code{object} instead.

\strong{Shine-down curve plots}
Please note that the shine-down curve plots are for information only. As such
a maximum of five pause steps are plotted to avoid graphically overloaded plots.
However, \emph{all} pause times are taken into consideration for the analysis.
}
\section{Function version}{
 0.1.23
}

\examples{

## load example data (sample UNIL/NB123, see ?ExampleData.Fading)
data("ExampleData.Fading", envir = environment())

##(1) get fading measurement data (here a three column data.frame)
fading_data <- ExampleData.Fading$fading.data$IR50

##(2) run analysis
g_value <- analyse_FadingMeasurement(
fading_data,
plot = TRUE,
verbose = TRUE,
n.MC = 10)

##(3) this can be further used in the function
## to correct the age according to Huntley & Lamothe, 2001
results <- calc_FadingCorr(
age.faded = c(100,2),
g_value = g_value,
n.MC = 10)

} 

\section{How to cite}{
Kreutzer, S., Burow, C., 2025. analyse_FadingMeasurement(): Analyse fading measurements and returns the fading rate per decade (g-value). Function version 0.1.23. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Aitken, M.J., 1985. Thermoluminescence dating, Studies in archaeological science.
Academic Press, London, Orlando.

Auclair, M., Lamothe, M., Huot, S., 2003. Measurement of anomalous fading for feldspar IRSL using
SAR. Radiation Measurements 37, 487-492. \doi{10.1016/S1350-4487(03)00018-0}

Huntley, D.J., Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars and the measurement
and correction for it in optical dating. Canadian Journal of Earth Sciences 38,
1093-1106. doi: \code{10.1139/cjes-38-7-1093}

Kars, R.H., Wallinga, J., Cohen, K.M., 2008. A new approach towards anomalous
fading correction for feldspar  IRSL dating-tests on samples in field saturation.
Radiation Measurements 43, 786-790. \doi{10.1016/j.radmeas.2008.01.021}
}
\seealso{
\link{calc_OSLLxTxRatio}, \link{read_BIN2R}, \link{read_XSYG2R},
\link{extract_IrradiationTimes}, \link{calc_FadingCorr}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany) \cr
Christoph Burow, University of Cologne (Germany)
, RLum Developer Team}
\keyword{datagen}

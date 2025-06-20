% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_EmissionSpectra.R
\name{fit_EmissionSpectra}
\alias{fit_EmissionSpectra}
\title{Luminescence Emission Spectra Deconvolution}
\usage{
fit_EmissionSpectra(
  object,
  frame = NULL,
  n_components = NULL,
  start_parameters = NULL,
  sub_negative = 0,
  input_scale = NULL,
  method_control = list(),
  verbose = TRUE,
  plot = TRUE,
  ...
)
}
\arguments{
\item{object}{\linkS4class{RLum.Data.Spectrum}, \link{matrix} (\strong{required}): input
object. Please note that an energy spectrum is expected}

\item{frame}{\link{integer} (\emph{optional}): number of the frame to be analysed. If
\code{NULL}, all available frames are analysed.}

\item{n_components}{\link{integer} (\emph{optional}): maximum number of components
desired: the number of component actually fitted may be smaller than this.
Can be combined with other parameters.}

\item{start_parameters}{\link{numeric} (\emph{optional}): allows to provide own start parameters for a
semi-automated procedure. Parameters need to be provided in eV. Every value provided replaces a
value from the automated peak finding algorithm (in ascending order).}

\item{sub_negative}{\link{numeric} (\emph{with default}): substitute negative values in the input object
by the number provided here (default: \code{0}). Can be set to \code{NULL}, i.e. negative values are kept.}

\item{input_scale}{\link{character} (\emph{optional}): defines whether your x-values
are expressed as wavelength or energy values. Allowed values are \code{"wavelength"},
\code{"energy"} or \code{NULL}, in which case the function tries to guess the input
automatically.}

\item{method_control}{\link{list} (\emph{optional}): options to control the fit method
and the output produced, see details.}

\item{verbose}{\link{logical} (\emph{with default}): enable/disable output to the
terminal.}

\item{plot}{\link{logical} (\emph{with default}): enable/disable the plot output.}

\item{...}{further arguments to be passed to control the plot output
(supported: \code{main}, \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim}, \code{log}, \code{mtext}, \code{legend} (\code{TRUE} or \code{FALSE}),
\code{legend.text}, \code{legend.pos})}
}
\value{
-----------------------------------\cr
\verb{[ NUMERICAL OUTPUT ]}\cr
-----------------------------------\cr

\strong{\code{RLum.Results}}-object

\strong{slot:} \strong{\verb{@data}}

\tabular{lll}{
\strong{Element} \tab \strong{Type} \tab \strong{Description}\cr
\verb{$data} \tab \code{matrix} \tab the final fit matrix \cr
\verb{$fit} \tab \code{nls} \tab the fit object returned by \link[minpack.lm:nls.lm]{minpack.lm::nls.lm} \cr
\verb{$fit_info} \tab \code{list} \tab a few additional parameters that can be used
to assess the quality of the fit \cr
\verb{$df_plot} \tab \code{list} \tab values of all curves in the plot for each
frame analysed (only if \code{method_control$export.plot.data = TRUE})
}

\strong{slot:} \strong{\verb{@info}}

The original function call

---------------------------------\cr
\verb{[ TERMINAL OUTPUT ]}   \cr
---------------------------------\cr

The terminal output provides brief information on the
deconvolution process and the obtained results.
Terminal output is only shown when \code{verbose = TRUE}.

---------------------------\cr
\verb{[ PLOT OUTPUT ]}      \cr
---------------------------\cr

The function returns a plot showing the raw signal with the
detected components. If the fitting failed, a basic plot is returned
showing the raw data and indicating the peaks detected for the start
parameter estimation. The grey band in the residual plot indicates the
10\% deviation from 0 (means no residual).
}
\description{
This function performs a luminescence spectra deconvolution on
\linkS4class{RLum.Data.Spectrum} and \link{matrix} objects on an \strong{energy scale}.
The function is optimised for emission spectra typically obtained in the
context of TL, OSL and RF  measurements detected between 200 and 1000 nm.
The function is not designed to deconvolve TL curves (counts against
temperature; no wavelength scale). If you are interested in such analysis,
please check, e.g., package \code{'tgcd'}.
}
\details{
\strong{Used equation}

The emission spectra (on an energy scale) can be best described as the sum of multiple
Gaussian components:

'\deqn{
y = \Sigma  Ci * 1/(\sigma_{i} * \sqrt(2 * \pi)) * exp(-1/2 * ((x - \mu_{i})/\sigma_{i}))^2)
}

with the parameters \eqn{\sigma} (peak width) and \eqn{\mu} (peak centre) and \eqn{C}
(scaling factor).

\strong{Start parameter estimation and fitting algorithm}

The spectrum deconvolution consists of the following steps:
\enumerate{
\item Peak finding \cr
\item Start parameter estimation \cr
\item Fitting via \link[minpack.lm:nls.lm]{minpack.lm::nls.lm}\cr
}

The peak finding is realised by an approach (re-)suggested by Petr Pikal via the R-help
mailing list (\verb{https://stat.ethz.ch/pipermail/r-help/2005-November/thread.html}) in November 2005.
This goes back to even earlier discussion in 2001 based on Prof Brian Ripley's idea.
It smartly uses the functions \link[stats:embed]{stats::embed} and \link{max.col} to identify peaks positions.
For the use in this context, the algorithm has been further modified to scale on the
input data resolution (cf. source code).\cr

The start parameter estimation uses random sampling from a range of meaningful parameters
and repeats the fitting until 1000 successful fits have been produced or the set \code{max.runs} value
is exceeded.

Currently the best fit is the one with the lowest number for squared residuals, but
other parameters are returned as well. If a series of curves needs to be analysed,
it is recommended to make few trial runs, then fix the number of components and
run  at least 10,000 iterations (parameter \code{method_control = list(max.runs = 10000)}).

\strong{Supported \code{method_control} settings}

\tabular{llll}{
\strong{Parameter} \tab \strong{Type} \tab \strong{Default} \tab \strong{Description}\cr
\code{max.runs} \tab \link{integer} \tab \code{10000} \tab maximum allowed search iterations, if exceed
the searching stops \cr
\code{graining} \tab \link{numeric} \tab \code{15} \tab control over how coarse or fine the spectrum is split into search intervals for the peak finding algorithm \cr
\code{norm} \tab \link{logical} \tab \code{TRUE} \tab normalise data to the highest count value before fitting \cr
\code{export.plot.data} \tab \link{logical} \tab \code{FALSE} \tab enable/disable export
of the values of all curves in the plot for each frame analysed \cr
\code{trace} \tab \link{logical} \tab \code{FALSE} \tab enable/disable the tracing of the minimisation routine
}
}
\section{Function version}{
 0.1.3
}

\examples{

##load example data
data(ExampleData.XSYG, envir = environment())

##subtract background
TL.Spectrum@data <- TL.Spectrum@data[] - TL.Spectrum@data[,15]

results <- fit_EmissionSpectra(
 object = TL.Spectrum,
 frame = 5,
 method_control = list(max.runs = 10)
)

##deconvolution of a TL spectrum
\dontrun{

##load example data

##replace 0 values
results <- fit_EmissionSpectra(
 object = TL.Spectrum,
 frame = 5, main = "TL spectrum"
)

}

}
\seealso{
\linkS4class{RLum.Data.Spectrum}, \linkS4class{RLum.Results}, \link{plot_RLum},
\link{convert_Wavelength2Energy}, \link[minpack.lm:nls.lm]{minpack.lm::nls.lm}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Marco Colombo, Institute of Geography, Heidelberg University (Germany)
, RLum Developer Team} 

\section{How to cite}{
Kreutzer, S., Colombo, M., 2025. fit_EmissionSpectra(): Luminescence Emission Spectra Deconvolution. Function version 0.1.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\keyword{datagen}

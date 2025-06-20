% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_SurfaceExposure.R
\name{fit_SurfaceExposure}
\alias{fit_SurfaceExposure}
\title{Nonlinear Least Squares Fit for OSL surface exposure data}
\usage{
fit_SurfaceExposure(
  data,
  sigmaphi = NULL,
  mu = NULL,
  age = NULL,
  Ddot = NULL,
  D0 = NULL,
  weights = FALSE,
  plot = TRUE,
  legend = TRUE,
  error_bars = TRUE,
  coord_flip = FALSE,
  ...
)
}
\arguments{
\item{data}{\link{data.frame} or \link{list} (\strong{required}):
Measured OSL surface exposure data with the following structure:

\if{html}{\out{<div class="sourceCode">}}\preformatted{                               (optional)
     | depth (a.u.)| intensity | error |
     |     [ ,1]   |    [ ,2]  | [ ,3] |
     |-------------|-----------|-------|
[1, ]|    ~~~~     |    ~~~~   | ~~~~  |
[2, ]|    ~~~~     |    ~~~~   | ~~~~  |
 ... |     ...     |     ...   |  ...  |
[x, ]|    ~~~~     |    ~~~~   | ~~~~  |

}\if{html}{\out{</div>}}

Alternatively, a \link{list} of \code{data.frames} can be provided, where each
\code{data.frame} has the same structure as shown above, with the exception that
they must \strong{not} include the optional error column. Providing a \link{list} as
input automatically activates the global fitting procedure (see details).}

\item{sigmaphi}{\link{numeric} (\emph{optional}):
A numeric value for \code{sigmaphi}, i.e. the charge detrapping rate.
Example: \code{sigmaphi = 5e-10}}

\item{mu}{\link{numeric} (\emph{optional}):
A numeric value for \code{mu}, i.e. the light attenuation coefficient. If \code{data}
is a list of \emph{n} samples, then \code{mu} must be a vector of length 1 or \emph{n}.
Example: \code{mu = 0.9}, or \code{mu = c(0.9, 0.8, 0.9)}.}

\item{age}{\link{numeric} (\emph{optional}):
The age (a) of the sample, if known. If \code{data} is a \link{list} of \emph{n} samples,
then \code{age} must be a numeric vector of length \emph{n}.
Example: \code{age = 10000}, or \code{age = c(1e4, 1e5, 1e6)}.}

\item{Ddot}{\link{numeric} (\emph{optional}):
A numeric value for the environmental dose rate (Gy/ka). For this argument
to be considered a value for \code{D0} must also be provided; otherwise it will be
ignored.}

\item{D0}{\link{numeric} (\emph{optional}):
A numeric value for the characteristic saturation dose (Gy). For this argument
to be considered a value for \code{Ddot} must also be provided; otherwise it will be
ignored.}

\item{weights}{\link{logical} (\emph{with default}):
If \code{TRUE} the fit will be weighted by the inverse square of the error.
Requires \code{data} to be a \link{data.frame} with three columns.}

\item{plot}{\link{logical} (\emph{with default}):
enable/disable the plot output.}

\item{legend}{\link{logical} (\emph{with default}):
Show or hide the equation inside the plot.}

\item{error_bars}{\link{logical} (\emph{with default}):
Show or hide error bars (only applies if errors were provided).}

\item{coord_flip}{\link{logical} (\emph{with default}):
Flip the coordinate system.}

\item{...}{Further parameters passed to \link{plot}.
Custom parameters include:
\itemize{
\item \code{verbose} (\link{logical}): show or hide console output
\item \code{line_col}: Colour of the fitted line
\item \code{line_lty}: Type of the fitted line (see \code{lty} in \code{?par})
\item \code{line_lwd}: Line width of the fitted line (see \code{lwd} in \code{?par})
}}
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
\verb{$summary} \tab \code{data.frame} \tab summary of the fitting results \cr
\verb{$data} \tab \code{data.frame} \tab the original input data \cr
\verb{$fit} \tab \code{nls} \tab the fitting object produced by \link[minpack.lm:nlsLM]{minpack.lm::nlsLM} \cr
\verb{$args} \tab \code{character} \tab arguments of the call \cr
\verb{$call} \tab \code{call} \tab the original function call \cr
}

\strong{slot:} \strong{\verb{@info}}

Currently unused.

------------------------\cr
\verb{[ PLOT OUTPUT ]}\cr
------------------------\cr

A scatter plot of the provided depth-intensity OSL surface exposure data
with the fitted model.
}
\description{
This function determines the (weighted) least-squares estimates of the
parameters of either equation 1 in \emph{Sohbati et al. (2012a)} or equation 12 in
\emph{Sohbati et al. (2012b)} for a given OSL surface exposure data set (\strong{BETA}).
}
\details{
\strong{Weighted fitting}

If \code{weights = TRUE} the function will use the inverse square of the error (\eqn{1/\sigma^2})
as weights during fitting using \link[minpack.lm:nlsLM]{minpack.lm::nlsLM}. Naturally, for this to
take effect individual errors must be provided in the third column of the
\code{data.frame} for \code{data}. Weighted fitting is \strong{not} supported if \code{data}
is a list of multiple \code{data.frame}s, i.e., it is not available for global
fitting.

\strong{Dose rate}
If any of the arguments \code{Ddot} or \code{D0} is at its default value (\code{NULL}),
this function will fit equation 1 in Sohbati et al. (2012a) to the data. If
the effect of dose rate (i.e., signal saturation) needs to be considered,
numeric values for the dose rate (\code{Ddot}) (in Gy/ka) and the characteristic
saturation dose (\code{D0}) (in Gy) must be provided. The function will then fit
equation 12 in Sohbati et al. (2012b) to the data.

\strong{NOTE}: Currently, this function does \strong{not} consider the variability
of the dose rate with sample depth (\code{x})! In the original equation the dose
rate \code{D} is an arbitrary function of \code{x} (term \code{D(x)}), but here \code{D} is assumed
constant.

\strong{Global fitting}
If \code{data} is \link{list} of multiple \code{data.frame}s, each representing a separate
sample, the function automatically performs a global fit to the data. This
may be useful to better constrain the parameters \code{sigmaphi} or \code{mu} and
\strong{requires} that known ages for each sample is provided
(e.g., \code{age = c(100, 1000)} if \code{data} is a list with two samples).
}
\note{
\strong{This function has BETA status. If possible, results should be}
\strong{cross-checked.}
}
\section{Function version}{
 0.1.0
}

\examples{

## Load example data
data("ExampleData.SurfaceExposure")

## Example 1 - Single sample
# Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10
sample_1 <- ExampleData.SurfaceExposure$sample_1
head(sample_1)
results <- fit_SurfaceExposure(
 data = sample_1,
 mu = 0.9,
 sigmaphi = 5e-10)
get_RLum(results)


## Example 2 - Single sample and considering dose rate
# Known parameters: 10000 a, mu = 0.9, sigmaphi = 5e-10,
# dose rate = 2.5 Gy/ka, D0 = 40 Gy
sample_2 <- ExampleData.SurfaceExposure$sample_2
head(sample_2)
results <- fit_SurfaceExposure(
 data = sample_2,
 mu = 0.9,
 sigmaphi = 5e-10,
 Ddot = 2.5,
 D0 = 40)
get_RLum(results)

## Example 3 - Multiple samples (global fit) to better constrain 'mu'
# Known parameters: ages = 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10
set_1 <- ExampleData.SurfaceExposure$set_1
str(set_1, max.level = 2)
results <- fit_SurfaceExposure(
  data = set_1,
  age = c(1e3, 1e4, 1e5, 1e6),
  sigmaphi = 5e-10)
get_RLum(results)


## Example 4 - Multiple samples (global fit) and considering dose rate
# Known parameters: ages = 1e2, 1e3, 1e4, 1e5, 1e6 a, mu = 0.9, sigmaphi = 5e-10,
# dose rate = 1.0 Ga/ka, D0 = 40 Gy
set_2 <- ExampleData.SurfaceExposure$set_2
str(set_2, max.level = 2)
results <- fit_SurfaceExposure(
 data = set_2,
 age = c(1e2, 1e3, 1e4, 1e5, 1e6),
 sigmaphi = 5e-10,
 Ddot = 1,
 D0 = 40)
get_RLum(results)

} 

\section{How to cite}{
Burow, C., 2025. fit_SurfaceExposure(): Nonlinear Least Squares Fit for OSL surface exposure data. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Sohbati, R., Murray, A.S., Chapot, M.S., Jain, M., Pederson, J., 2012a.
Optically stimulated luminescence (OSL) as a chronometer for surface exposure
dating. Journal of Geophysical Research 117, B09202. doi:
\doi{10.1029/2012JB009383}

Sohbati, R., Jain, M., Murray, A.S., 2012b. Surface exposure dating of
non-terrestrial bodies using optically stimulated luminescence: A new method.
Icarus 221, 160-166.
}
\seealso{
\link{ExampleData.SurfaceExposure}, \link[minpack.lm:nlsLM]{minpack.lm::nlsLM}
}
\author{
Christoph Burow, University of Cologne (Germany)
, RLum Developer Team}
\keyword{datagen}

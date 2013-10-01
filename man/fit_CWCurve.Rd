\name{fit_CWCurve}
\alias{fit_CWCurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nonlinear Least Squares Fit for CW-OSL curves
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function determines the weighted least-squares estimates of the component parameters of a CW-OSL for a given maximum number of components and returns various component parameters.
The fitting procedure uses the \code{\link{nls}} function with the \code{port} algorithm.
}
\usage{
fit_CWCurve(values, 
            n.components.max, 
            fit.failure_threshold = 3, 
            fit.trace = FALSE, 
            fit.calcError = FALSE, 
            LED.power = 36, 
            LED.wavelength = 470, 
            log = "", 
            cex.global = 0.6, 
            main = "CW-OSL Curve Fit", 
            sample_code = "Default", 
            ylab, xlab, 
            output.path, 
            output.terminal = TRUE, 
            output.terminalAdvanced = TRUE, 
            output.plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{\code{\linkS4class{RLum.Data.Curve}} or \link{data.frame} (\bold{required}): x, y data of measured values (time and counts). See examples.}
  \item{n.components.max}{\link{vector} (optional): maximum number of components that are to be used for 
  fitting. The upper limit is 7.
}
  \item{fit.failure_threshold}{\link{vector} (with default): limits the failed fitting attempts.  
}
 \item{fit.trace}{\link{logical} (with default): trace the fitting process on the terminal.}
  \item{fit.calcError}{\link{logical} (with default): calculate 1-sigma error range of components using \link{confint}}
  
 \item{LED.power}{\link{numeric} (with default): LED power (max.) used for intensity ramping in mW/cm^2. \bold{Note:} The value is used 
    for the calculation of the absolute photoionisation cross section.}
    \item{LED.wavelength}{\link{numeric} (with default): LED wavelength used for stimulation in nm. \bold{Note:} The value is used 
    for the calculation of the absolute photoionisation cross section.}
    
  \item{log}{\link{character} (optional): option for log-scaled axis, works as in \link{plot}
}
  \item{cex.global}{\link{numeric} (with default): global scaling factor.}
  \item{main}{\link{character} (with default): header for plot output.}
  \item{sample_code}{\link{character} (optional): sample code used for the plot and the optional output table (mtext).}
  \item{ylab}{\link{character} (with default): alternative y-axis labelling 
}
  \item{xlab}{\link{character} (with default): alternative x-axis labelling 
}
 \item{output.path}{\link{character} (optional): output path for table output containing the results of the fit. The file name is set automatically. If
    the file already exists in the directory, the values are appended.}
  \item{output.terminal}{\link{logical} (with default): terminal ouput with fitting results.}
   \item{output.terminalAdvanced}{\link{logical} (with default): enhanced terminal output. Requires \code{output.terminal = TRUE}.
    If output.terminal = FALSE no advanced output is possible.}
   \item{output.plot}{\link{logical} (with default): returns a plot of the fitted curves.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~

\bold{Fitting function}

The function for the CW-OSL fitting has the general form:
\deqn{y = I0_{1}*\lambda_{1}*exp(-\lambda_1*x) + ,\ldots, 
      + I0_{i}*\lambda_{i}*exp(-\lambda_i*x)
}
      where \eqn{1>= i <= 7}\cr\cr
      and \eqn{\lambda} is the decay constant and \eqn{N0} the intial number of 
      trapped electrons.
 
(for the used equation e.g. cf. Boetter-Jensen et al., 2003 ) 

\bold{Start values}

Start values are estimated automatically by fitting a linear function on the log input data set. 
Currently, there is no option to manually provide start parameters. 

\bold{Goodness of fit}

The goodness of the fit is given by as pseudo-R^2 value (pseudo coefficient of determination). 
According to Lave (1970), the value is calculated as:

\deqn{pseudoR^2 = 1 - RSS/TSS}
where   \eqn{RSS = Residual~Sum~of~Squares} \cr
and \eqn{TSS = Total~Sum~of~Squares}

\bold{Error of fitted component parameters}

The 1-sigma error for the components is calculated using the function \link{confint}. Due to considerable calculation time, this option is deactived by default. In addition, the error for the components can be estimated by using internal R functions like \link{summary}.
See the \link{nls} help page for more information.

\emph{For details on the nonlinear regression in R, see Ritz & Streibig (2008).}
}
\value{
\item{plot}{(optional) the fitted CW-OSL curves are returned as plot.}
\item{table}{(optional) an output table (*.csv) with the fitted components is provided 
if the \code{output.path} is set.}


\item{\code{\linkS4class{RLum.Results}} object}{beside the plot and table output options a \code{\linkS4class{RLum.Results}} is returned.

\code{fit} : a \code{nls} object (\code{$fit}) for which generic R functions are provided
e.g. \link{summary}, \link{confint}, \link{profile}. For more details, see \link{nls}.


\code{output.table}:  a \link{data.frame} containing the summarized parameters including the 
error

\code{component.contribution.matrix}: \link{matrix} containing the values for the component to sum contribution plot 
(\code{$component.contribution.matrix}).
}
}
\references{
Boetter-Jensen, L., McKeever, S.W.S., Wintle, A.G., 2003. Optically Stimulated Luminescence Dosimetry. Elsevier Science B.V.

Lave, C.A.T., 1970. The Demand for Urban Mass Transportation. The Review of Economics and Statistics, 52 (3), pp. 320-323. 

Ritz, C. & Streibig, J.C., 2008. Nonlinear Regression with R. R. Gentleman, K. Hornik, G. Parmigiani, eds., Springer.
}
\author{Sebastian Kreutzer, JLU Giessen (Germany)}

\section{Version}{0.3 [2013-09-30]}

\note{
\bold{Beta version - This function has not been properly tested yet.}

%%  ~~further notes~~
The pseudo-R^2 may not be the best parameter to describe the goodness of the fit. The trade off between the \code{n.components} and the pseudo-R^2 value has not yet been considered. 

The function \bold{does not} ensure that the fitting procedure has reached a global minimum 
rather than a local minimum! In any case of doubt, the use of manual start values is highly 
recommended.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fit_LMCurve}}, \code{\link{plot}},\code{\link{nls}},
\code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Results}},
\code{\link{get_RLum.Results}}


%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##load data
data(ExampleData.CW_OSL_Curve, envir = environment())

##fit data
fit <- fit_CWCurve(values = ExampleData.CW_OSL_Curve,
                   main = "CW Curve Fit",
                   n.components.max = 4,
                   log = "x")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{models}

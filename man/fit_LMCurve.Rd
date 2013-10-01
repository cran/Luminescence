\name{fit_LMCurve}
\alias{fit_LMCurve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Nonlinear Least Squares Fit for LM-OSL Curves}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The function determines weighted nonlinear least-squares estimates of the component parameters of a LM-OSL curve (Bulur 1996) 
for a given number of components and returns various component parameters.
The fitting procedure uses the function \code{\link{nls}} with the \code{port} algorithm. 
}
\usage{
fit_LMCurve(values, 
            values.bg, 
            n.components = 3, 
            start_values,
            input.dataType = "LM",
            sample_code = "",
            sample_ID = "",
            LED.power = 36, LED.wavelength = 470, 
            cex.global = 0.8, 
            fit.trace = FALSE,
            fit.advanced = FALSE,
            fit.calcError = FALSE,
            bg.subtraction = "polynomial",            
            output.path,                   
            output.terminal = TRUE, output.terminaladvanced = TRUE, 
            output.plot = TRUE, output.plotBG = FALSE,
            ...)
            
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{values}{\code{\linkS4class{RLum.Data.Curve}} or \link{data.frame} (\bold{required}): x,y data of measured values (time and counts). See examples.}
    \item{values.bg}{\code{\linkS4class{RLum.Data.Curve}} or \link{data.frame} (optional): x,y data of measured values (time and counts) for background subtraction}
    \item{n.components}{\link{integer} (with default): fixed number of components that are to be recognised during fitting (min = 1, max = 7).}
    \item{start_values}{\link{data.frame} (optional): start parameters for lm and xm data for the fit. If no start values are given, an automatic start value estimation is attempted (see details).}
    \item{input.dataType}{\link{character} (with default): alter the plot output depending on the input data: "LM" or "pLM" (pseudo-LM). 
    See: \link{CW2pLM}}
    \item{sample_code}{\link{character} (optional): sample code used for the plot and the optional output table (mtext).}
    \item{sample_ID}{\link{character} (optional): additional identifier used as column for the table output.}
    \item{LED.power}{\link{numeric} (with default): LED power (max.) used for intensity ramping in mW/cm^2. \bold{Note:} This value is used 
    for the calculation of the absolute photoionisation cross section.}
    \item{LED.wavelength}{\link{numeric} (with default): LED wavelength used for stimulation in nm. \bold{Note:} This value is used 
    for the calculation of the absolute photoionisation cross section.}
    \item{cex.global}{\link{numeric} (with default): global scaling factor.}
    \item{fit.trace}{\link{logical} (with default): trace the fitting process on the terminal.}
    \item{fit.advanced}{\link{logical} (with default): enables advanced fitting attempt for automatic start parameter recognition. Works only if no start parameters are provided. \bold{Note:} It may take a while.}
    \item{fit.calcError}{\link{logical} (with default): calculate 1-sigma error range of components using \link{confint}}
    \item{bg.subtraction}{\link{character} (with default): specify method for background subraction (\code{polynomial},
                          \code{linear}, \code{channel}, see Details). \bold{Note:} requires input for \code{values.bg}.}
    \item{output.path}{\link{character} (optional): output path for table output containing the results of the fit. The file name is set automatically. If
    the file already exists in the directory, the values are appended.}
    \item{output.terminal}{\link{logical} (with default): terminal output with fitting results.}
    \item{output.terminaladvanced}{\link{logical} (with default): enhanced terminal output. Requires \code{output.terminal = TRUE}.
    If output.terminal = FALSE no advanced output is possible.}
    \item{output.plot}{\link{logical} (with default): returns a plot of the fitted curves.}
    \item{output.plotBG}{\link{logical} (with default): returns a plot of the background values with the fit used for the
    background subtraction.}
    \item{...}{Further arguments that may passed to the plot output, e.g. \code{xlab}, \code{xlab}, \code{main}, \code{log}}
    
 }
\details{

\bold{Fitting function}

The function for the fitting has the general form :
\deqn{y = exp(0.5)*Im_1*x/xm_1)*exp(-x^2/(2*xm_1^2)) + ,\ldots, 
      + exp(0.5)*Im_i*x/xm_i)*exp(-x^2/(2*xm_i^2))
}
      where \eqn{1>= i <= 7}
      
This function and the equations for the conversion to b 
(detrapping probability) and n0 (proportional to initially trapped charge) have been taken from Kitis et al. (2008):

\deqn{xm_i=\sqrt{max(t)/b_i}}
\deqn{Im_i=exp(-0.5)n0/xm_i}\cr

\bold{Background subtraction}

Three methods for background subtraction are provided for a given background signal (\code{values.bg}).

\code{polynomial}: default method. A polynomial function is fitted using \link{glm} and the resulting function 
is used for the background subtraction:

\deqn{y = a*x^4 + b*x^3 + c*x^2 + d*x + e}

\code{linear}: a linear function is fitted using \link{glm} and the resulting function 
is used for the background subtraction:

\deqn{y = a*x + b}

\code{channel}: the measured background signal is subtracted channelwise from the measured signal.\cr

\bold{Start values}

The choice of the initial parameters for the nls-fitting is a crucial point and the fitting procedure
may mainly fail due to ill chosen start parameters. Here, three options are provided:

\bold{(a)} If no start values (\code{start_values}) are provided by the user, a cheap guess is made by using 
the detrapping values found by Jain et al. (2003) for quartz for a maximum of 7 components. Based
on these values, the pseudo start parameters xm and Im are recalculated for the given data set. 
In all cases, the fitting starts with the ultra-fast component and (depending on \code{n.components})
steps through the following values. If no fit could be achieved, an error plot 
(for \code{output.plot = TRUE}) with the pseudo curve (based on the pseudo start parameters) is provided.
This may give the opportunity to identify appropriate start parameters visually.

\bold{(b)} If start values are provided, the function works like a simple \code{\link{nls}} fitting approach.

\bold{(c)} If no start parameters are provided and the option \code{fit.advanced = TRUE} is chosen, an 
advanced start paramter estimation is applied using a stochastical attempt. Therefore the recalculated start parameters \bold{(a)}
are used to construct a normal distribution. The start parameters are then sampled randomly from this distribution. 
A maximum of 100 attempts will be made. \bold{Note:} This process may be time consuming. \cr

\bold{Goodness of fit}

The goodness of the fit is given by a pseudo-R^2 value (pseudo coefficient of determination). 
According to Lave (1970), the value is calculated as:

\deqn{pseudoR^2 = 1 - RSS/TSS}
where   \eqn{RSS = Residual~Sum~of~Squares} \cr
and \eqn{TSS = Total~Sum~of~Squares}

\bold{Error of fitted component parameters}

The 1-sigma error for the components is calculated using the function \link{confint}. Due to considerable
calculation time, this option is deactived by default. In addition, the error for the components 
can be estimated by using internal R functions like \link{summary}.
See the \link{nls} help page for more information.


\emph{For more details on the nonlinear regression in R, see Ritz & Streibig (2008).}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{plot}{(optional) various types of plots are returned. For details see above.}
\item{table}{(optional) an output table (*.csv) with the fitted components is provided 
if the \code{output.path} is set.}
\item{\code{list} object}{beside the plot and table output options a \link{list} is returned. 
The list contains: \cr

(a) a \code{nls} object (\code{$fit}) for which generic R functions are prov,
e.g. \link{summary}, \link{confint}, \link{profile}. For more details, see \link{nls}. \cr

(b) a \link{data.frame} containing the summarised parameters including the 
error (\code{$output.table}).

(c) a \link{matrix} containing the values for the component to sum contribution plot 
(\code{$component.contribution.matrix}).
}

}
\references{
%% ~put references to the literature/web site here ~
Bulur, E., 1996. An Alternative Technique For Optically Stimulated Luminescence (OSL) Experiment. Radiation Measurements, 26 (5), 
pp. 701-709.

Jain, M., Murray, A.S., Boetter-Jensen, L., 2003. Characterisation of blue-light stimulated luminescence components in different quartz samples: implications for dose measurement. Radiation Measurements, 37 (4-5), pp. 441-449. 

Kitis, G. & Pagonis, V., 2008. Computerized curve deconvolution analysis for LM-OSL. Radiation Measurements, 43, pp. 737-741. 

Lave, C.A.T., 1970. The Demand for Urban Mass Transportation. The Review of Economics and Statistics, 52 (3), pp. 320-323. 

Ritz, C. & Streibig, J.C., 2008. Nonlinear Regression with R. R. Gentleman, K. Hornik, & G. Parmigiani, eds., Springer.

}
\author{Sebastian Kreutzer, JLU Giessen (Germany)}
\section{Version}{0.2.10 [2013-09-27]}

\note{
%%  ~~further notes~~
The pseudo-R^2 may not be the best parameter to describe the goodness of the fit. The trade off 
between the \code{n.components} and the pseudo-R^2 value has not yet been considered. 

The function \bold{does not} ensure that the fitting procedure has reached a global minimum 
rather than a local minimum! In any case of doubt, the use of manual start values is highly 
recommended.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{fit_CWCurve}}, \code{\link{plot}},\code{\link{nls}}
}
\examples{

##(1) fit LM data without background subtraction 
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve, n.components = 3, log = "x")

##(2) fit LM data with background subtraction and export as JPEG
## -alter file path for your prefered system
##jpeg(file = "~/Desktop/Fit_Output\%03d.jpg", quality = 100, 
    ## height = 3000, width = 3000, res = 300)
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve, values.bg = values.curveBG,
     n.components = 2, log = "x", output.plotBG = TRUE)
##dev.off()

##(3) fit LM data with manual start parameters
data(ExampleData.FittingLM, envir = environment())
fit_LMCurve(values = values.curve, values.bg = values.curveBG, n.components = 3, log = "x",
            start_values = data.frame(Im = c(170,25,400), xm = c(56,200,1500)))


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
\keyword{models}

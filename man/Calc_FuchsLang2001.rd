% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_FuchsLang2001.R
\name{calc_FuchsLang2001}
\alias{calc_FuchsLang2001}
\title{Apply the model after Fuchs & Lang (2001) to a given De distribution.}
\usage{
calc_FuchsLang2001(data, cvThreshold = 5, startDeValue = 1, plot = TRUE,
  ...)
}
\arguments{
\item{data}{\code{\linkS4class{RLum.Results}} or \link{data.frame}
(\bold{required}): for \code{data.frame}: two columns with De
\code{(data[,1])} and De error \code{(values[,2])}}

\item{cvThreshold}{\link{numeric} (with default): coefficient of variation
in percent, as threshold for the method, e.g. \code{cvThreshold = 3}. See
details.}

\item{startDeValue}{\link{numeric} (with default): number of the first
aliquot that is used for the calculations}

\item{plot}{\link{logical} (with default): plot output
\code{TRUE}/\code{FALSE}}

\item{\dots}{further arguments and graphical parameters passed to
\code{\link{plot}}}
}
\value{
Returns a plot (optional) and terminal output. In addition an
\code{\linkS4class{RLum.Results}} object is returned containing the
following elements:

\item{summary}{\link{data.frame} summary of all relevant model results.}
\item{data}{\link{data.frame} original input data} \item{args}{\link{list}
used arguments} \item{call}{\link{call} the function call}
\item{usedDeValues}{\link{data.frame} containing the used values for the
calculation}
}
\description{
This function applies the method according to Fuchs & Lang (2001) for
heterogeneously bleached samples with a given coefficient of variation
threshold.
}
\details{
\bold{Used values} \cr If the coefficient of variation (c[v]) of the first
two values is larger than the threshold c[v_threshold], the first value is
skipped.  Use the \code{startDeValue} argument to define a start value for
calculation (e.g. 2nd or 3rd value).\cr

\bold{Basic steps of the approach} \cr

(1) Estimate natural relative variation of the sample using a dose recovery
test\cr (2) Sort the input values ascendingly\cr (3) Calculate a running
mean, starting with the lowermost two values and add values iteratively.\cr
(4) Stop if the calculated c[v] exceeds the specified \code{cvThreshold}\cr
}
\note{
Please consider the requirements and the constraints of this method
(see Fuchs & Lang, 2001)
}
\section{Function version}{
 0.4.1 (2015-11-29 17:27:48)
}
\examples{


##load example data
data(ExampleData.DeValues, envir = environment())

##calculate De according to Fuchs & Lang (2001)
temp<- calc_FuchsLang2001(ExampleData.DeValues$BT998, cvThreshold = 5)

}
\author{
Sebastian Kreutzer, IRAMAT-CRP2A, Universite Bordeaux Montaigne
(France) Christoph Burow, University of Cologne (Germany)
\cr R Luminescence Package Team}
\references{
Fuchs, M. & Lang, A., 2001. OSL dating of coarse-grain fluvial
quartz using single-aliqout protocols on sediments from NE Peloponnese,
Greece. In: Quaternary Science Reviews 20, 783-787.

Fuchs, M. & Wagner, G.A., 2003. Recognition of insufficient bleaching by
small aliquots of quartz for reconstructing soil erosion in Greece.
Quaternary Science Reviews 22, 1161-1167.
}
\seealso{
\code{\link{plot}}, \code{\link{calc_MinDose}},
\code{\link{calc_FiniteMixture}}, \code{\link{calc_CentralDose}},
\code{\link{calc_CommonDose}}, \code{\linkS4class{RLum.Results}}
}
\keyword{dplot}


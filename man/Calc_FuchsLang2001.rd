\name{calc_FuchsLang2001}
\alias{calc_FuchsLang2001}
\title{
Apply the model after Fuchs & Lang (2001) to a given De distribution.
}
\description{
This function applies the method according to Fuchs & Lang (2001) for heterogeneously bleached samples with a 
given coefficient of variation threshold.
}
\usage{
calc_FuchsLang2001(sample, 
                   sample.mtext = "unknown sample", 
                   sample.id = sample.mtext, 
                   cvThreshold = 5, 
                   startDeValue = 1, 
                   output.plot = TRUE, output.terminal = TRUE, 
                   main = "Fuchs & Lang (2001)", 
                   xlab = expression(paste(D[e], " [Gy]")), 
                   cex.global = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{\link{data.frame} (\bold{required}): two column data frame, e.g. De and De error
}
  \item{sample.mtext}{\link{character} (optional): mtext for optional plot (top)
}
  \item{sample.id}{\link{character} (with default): sample id, with default the sample.mtext is used.
}
  \item{cvThreshold}{\link{numeric} (with default): coefficient of variation in percent, as threshold for the method, e.g. \code{cvThreshold = 3}. See details.
}
  \item{startDeValue}{\link{numeric} (with default): number of the first aliquot that is used for the calculations
}
  \item{output.plot}{\link{logical} (with default): plot output \code{TRUE}/\code{FALSE}
}
  \item{output.terminal}{\link{logical} (with default): terminal output \code{TRUE}/\code{FALSE}
}
  \item{main}{\link{character} (with default): title of the plot (works as in \link{plot})
}
  \item{xlab}{\link{character} (with default): xlab works as in \link{plot}
}
  \item{cex.global}{\link{numeric} (with default): global scaling factor
}
}
\details{

\bold{Used values} \cr

If the coefficient of variation (c[v]) of the first two values is larger than the threshold c[v_threshold], the first value is skipped. 
Use the \code{startDeValue} argument to define a start value for calculation (e.g. 2nd or 3rd value).\cr


\bold{Basic steps of the approach} \cr

	
(1) Estimate natural relative variation of the sample using a dose recovery test

(2) Sort the input values ascendingly

(3) Calculate a running mean, starting with the lowermost two values and add values iteratively. 

(4) Stop if the calculated c[v] exceeds the specified \code{cvThreshold}
}
\value{
A plot and terminal output is provided if desired. In addition, a list is returned containing two
elements:

\item{results}{\link{data.frame} with stastical parameters, e.g. mean, sd, ...}
\item{usedDeValues}{\link{data.frame} containing the used values for the calculation}

}
\references{
Fuchs, M. & Lang, A., 2001. OSL dating of coarse-grain fluvial quartz using single-aliqout
protocols on sediments from NE Peloponnese, Greece. In: Quaternary Science Reviews (20), pp. 783-787.

Fuchs, M. & Wagner, G.A., 2003. Recognition of insufficient bleaching by small aliquots of quartz 
for reconstructing soil erosion in Greece. Quaternary Science Reviews, 22, pp. 1161-1167. 

}
\author{
Sebastian Kreutzer, JLU Giessen, 2012
}
\note{
Please consider the requirements and the constraints of this method (see Fuchs & Lang, 2001)
}

\seealso{
\code{\link{plot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##load example data
data(ExampleData.DeValues)

##calculate De according to Fuchs & Lang (2001)
calc_FuchsLang2001(ExampleData.DeValues, cvThreshold = 5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

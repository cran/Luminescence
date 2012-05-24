\name{Calc_FuchsLang2001}
\alias{Calc_FuchsLang2001}
\title{
Calculate De/Age applying the method according to Fuchs/Lang 2001
}
\description{
Applying the Fuchs/Lang 2001 method for heterogeneously bleached samples for a 
given coefficient of variation threshold.
}
\usage{
Calc_FuchsLang2001(sample, 
                   sample.mtext = "unkown sample", 
                   sample.id = sample.mtext, 
                   cvThreshold = 5, 
                   startDeValue = 1, 
                   output.plot = TRUE, output.terminal = TRUE, 
                   main = "Fuchs/Lang 2001", 
                   xlab = expression(paste(D[e], " [Gy]")), 
                   cex.global = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sample}{\link{data.frame} (\bold{required}): two column data.frame, e.g. De and De Error
}
  \item{sample.mtext}{\link{character} (optional): mtext for optional plot
}
  \item{sample.id}{\link{character} (with default): sample id, with default the sample.mtext is used.
}
  \item{cvThreshold}{\link{numeric} (with default): coefficient of variation as threshold for the method 
  in percentage, e.g. \code{cvThreshold=3}. See details.
}
  \item{startDeValue}{\link{numeric} (with default): number of the first aliquot that is used for the calculations
}
  \item{output.plot}{\link{logical} (with default): plot output \code{TRUE} or \code{FALSE}
}
  \item{output.terminal}{\link{logical} (with default): terminal output \code{TRUE} or \code{FALSE}
}
  \item{main}{\link{character} (with default): title of the plot (works as in \link{plot})
}
  \item{xlab}{\link{character} (with default): xlab works as in \link{plot}.
}
  \item{cex.global}{\link{numeric} (with default): global scaling factor.
}
}
\details{

\bold{used values} \cr

If the cv of the first two values > cvThreshold, the frist value is skipped. If you want to use only values
above the 2nd or 3th values use the \code{startDeValue} parameter\cr


\bold{how the method works} \cr

	
(1) Estimate natural relative variation of your sample via a dose recovery test

(2) The input values are orderd in ascending order

(3) A running mean is calculated, starting with the lowermost two values, subsequent one value is added at each
step


(4) Stop if the calculated c[v] exceeds the specified \code{cvThreshold}
}
\value{
A plot and terminal output is provided if wanted. In addition a list is returned containing two
elements:

\item{results}{\link{data.frame} with stastical parameters, e.g. mean, sd,...}
\item{usedDeValues}{\link{data.frame} containing the used values for the calculation.}

}
\references{
Fuchs, M.,Lang, A. (2001): OSL dating of coarse-grain fluvial quartz using single-aliqout
protocols on sediments from NE Peloponnese, Greece. In: Quaternary Science Reviews (20), p. 783-787

Fuchs, M. & Wagner, G.A. (2003). Recognition of insufficient bleaching by small aliquots of quartz 
for reconstructing soil erosion in Greece. Quaternary Science Reviews, 22, 1161-1167. 

}
\author{
Sebastian Kreutzer, JLU Giessen, 2012
}
\note{
Before using: Please carefully consider the requirments and the constraints of this method!
}

\seealso{
\code{\link{plot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(ExampleData.DeValues)
Calc_FuchsLang2001(ExampleData.DeValues,cvThreshold=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{dplot}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

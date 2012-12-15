\name{Calc_FadingCorr}
\alias{Calc_FadingCorr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Apply a fading correction according to Huntley & Lamothe (2001) for a given g-value.
}
\description{
This function runs the itrerations that are needed to calculate the corrected age including the error 
for a given g-value according to Huntley & Lamothe (2001).
}
\usage{
Calc_FadingCorr(g_value, tc, age.faded, n.MCruns = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g_value}{\link{vector} (\bold{required}): g-value and error obtained from separate 
  fading measurements (see example)
}
  \item{tc}{\link{numeric} (\bold{required}): time in seconds (time between irradiation and
  the prompt measurement, cf. Huntely & Lamothe 2001).
}
  \item{age.faded}{\link{numeric} \link{vector} (\bold{required}): uncorrected age with error in ka (see example)
}

 \item{n.MCruns}{\link{integer} (with default): number of Monte Carlo simulation runs for error estimation
}
}
\details{
The error of the fading-corrected age is determined using a Monte Carlo simulation approach. 
Hugh values for n.MCruns will significantly increase the computation time.
}
\value{
A \link{data.frame} containing the fading-corrected age is returned.
}
\references{
Huntley, D.J. & Lamothe, M., 2001. Ubiquity of anomalous fading in K-feldspars and the measurement and correction for it in optical dating. Canadian Journal of Earth Sciences, 38, pp. 1093-1106.}
\author{
Sebastian Kreutzer, JLU Giessen, 2012
}
\note{
The upper age limit is set to 500 ka!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
##
}
\examples{
Calc_FadingCorr(g_value = c(3.3,0.03), tc = 752, age.faded = c(100,10))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

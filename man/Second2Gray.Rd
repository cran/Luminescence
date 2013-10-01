\name{Second2Gray}
\alias{Second2Gray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converting values from seconds (s) to gray (Gy)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Conversion of absorbed radiation dose in seconds (s) to the SI unit gray (Gy) 
including error propagation. Normally used for equivalent dose data.

}
\usage{
Second2Gray(values,
            dose_rate,
            method = "gaussian")}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
%%     ~~Describe \code{file} here~~
  \link{data.frame} (\bold{required}): measured data (\code{values[,1]}) and data error (\code{values [,2]})}
  \item{dose_rate}{\link{vector} (\bold{required}): dose rate in Gy/s and dose rate error in Gy/s}


\item{method}{\link{character} (with default): method used for error calculation \code{gaussian} or \code{absolute}, see details for further information}

}
\details{
Calculation of De values from seconds (s) to gray (Gy)
\deqn{De [Gy] = De [s] * Dose Rate [Gy/s])}


Provided calculation methods for error calculation:

\bold{gaussian} error propagation

\deqn{De.error.gray = \sqrt(dose.rate * De.error.seconds)^2 + (De.seconds * dose.rate.error)^2 ))}

\bold{absolute} error propagation 

\deqn{De.error.gray = abs(dose.rate * De.error.seconds) + abs(De.seconds * dose.rate.error)}

}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a \link{data.frame} with converted values.
}
\references{
%% ~put references to the literature/web site here ~
##
}
\author{Sebastian Kreutzer, JLU Giessen (Germany),
Michael Dietze, TU Dresden (Germany),
Margret C. Fuchs, TU Bergakadamie Freiberg (Germany)}
\note{
%%  ~~further notes~~
If no or a wrong method is given, the execution of the function is stopped. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\section{Version}{0.3 [2013-02-15]}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
##
}
\examples{
##(1) for dose rate = 0.14 +/- 0.001
data(ExampleData.DeValues, envir = environment())
Second2Gray(ExampleData.DeValues,c(0.14,0.001))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

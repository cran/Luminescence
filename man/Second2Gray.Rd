\name{Second2Gray}
\alias{Second2Gray}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converting values from seconds (s) to gray (Gy)}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Convertion of absorbed radiation dose in seconds (s) to the SI unit gray (Gy) 
using the Gaussian-Error Propagation. Normally used for equivalent dose data.

}
\usage{
Second2Gray(values,dose_rate)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
%%     ~~Describe \code{file} here~~
  \link{data.frame} (\bold{required}): measured data (\code{values[,1]}) and data error (\code{values [,2]})}
  \item{dose_rate}{\link{vector} (\bold{required}): dose rate in Gy/s and dose rate error in Gy/s}

}
\details{
%%  ~~ If necessary, more details than the description above ~~
The functions uses the Gaussian error propagation to convert values with errors from Seconds to Gray.

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
\author{Sebastian Kreutzer, JLU Giessen (Germany), 2012}
\note{
%%  ~~further notes~~
##
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
##
}
\examples{
##(1) for dose rate = 0.14 +/- 0.001
data(ExampleData.DeValues)
Second2Gray(ExampleData.DeValues,c(0.14,0.001))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}
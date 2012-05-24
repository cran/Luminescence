\name{ExampleData.LxTxOSLData}
\alias{ExampleData.LxTxOSLData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example Lx and Tx curve data from a artificially OSL measurement.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Lx and Tx curve.

}
\usage{
ExampleData.LxTxOSLData.RData
}
%- maybe also 'usage' for other objects documented here.
\format{
Two \code{data.frames} containing time and count values.
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
Artificial OSL measurement.

}

\references{
%% ~put references to the literature/web site here ~
##
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##load data
data(ExampleData.LxTxOSLData)


##plot data
plot(Lx.data)
plot(Tx.data)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
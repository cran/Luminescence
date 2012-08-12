\name{ExampleData.LxTxData}
\alias{ExampleData.LxTxData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example Lx/Tx data from SAR measurement}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
LxTx data from a measurement

}
\usage{
ExampleData.LxTxData
}
%- maybe also 'usage' for other objects documented here.
\format{
A \code{data.frame} with 4 columns (Dose,LxTx,LxTx.Error,TnTx).
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT607\cr
Location: \tab Ostrau (Saxony-Anhalt/Germany)\cr
Material: \tab middle grain quartz measured on a Risoe DA-15 reader.\cr
}
}

\references{
%% ~put references to the literature/web site here ~
##
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##plot Lx/Tx data vs dose [s]
data(ExampleData.LxTxData)
plot(LxTxData$Dose,LxTxData$LxTx)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
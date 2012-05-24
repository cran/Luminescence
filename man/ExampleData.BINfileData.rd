\name{ExampleData.BINfileData}
\alias{ExampleData.BINfileData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example data from a SAR luminescence measurement}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Example data from a SAR luminescence measurement diretly extracted from
a Risoe BIN file provided in an object of type \link{Risoe.BINfileData-class}
}
\usage{
ExampleData.BINfileData
}
%- maybe also 'usage' for other objects documented here.
\format{
Class object containing two slots: (a) \code{METADATA} is a \link{data.frame} with all metadata
stored in the BIN file of the measurements and (b) \code{DATA} contains a list of vectors of the measured data
(usually count values).
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT607\cr
Location: \tab Saxony/Germany\cr
Material: \tab middle grain quartz measured on aluminum cups on a Risoe DA-15 reader.\cr
}
}

\references{
%% ~put references to the literature/web site here ~
unpublished data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##show first 5 elements of the METADATA and DATA elelement in the terminal
data(ExampleData.BINfileData)
BINfileData@METADATA[1:5,]
BINfileData@DATA[1:5]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
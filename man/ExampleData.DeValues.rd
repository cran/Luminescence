\name{ExampleData.DeValues}
\alias{ExampleData.DeValues}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example De data for package Luminescence}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
25 equivalent dose (De) values measured for a fine grain quartz sample from a loess section 
in Rottewitz (Saxony/Germany). Internal lab code: BT998.

}
\usage{
ExampleData.DeValues
}
%- maybe also 'usage' for other objects documented here.
\format{
A data.frame with two columns (De and De.Error).
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT998\cr
Location: \tab Rottewitz (Saxony/Germany)\cr
Material: \tab fine grain quartz measured on aluminum discs on a Risoe DA-15 reader.\cr
Units:    \tab values are given in seconds. \cr
Dose Rate: \tab dose rate of the beta-source ca. 0.0438 +/- 0.0019 Gy/s
}
}

\references{
%% ~put references to the literature/web site here ~
unpublished data
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##plot histogram
data(ExampleData.DeValues)
hist(ExampleData.DeValues[,1])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
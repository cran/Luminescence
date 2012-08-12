\name{ExampleData.FittingLM}
\alias{ExampleData.FittingLM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Example data for fit_LMCurve() in package Luminescence }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Lineraly modulated (LM) measurement data from a quartz sample including background 
measurement.

}
\usage{
ExampleData.FittingLM
}
%- maybe also 'usage' for other objects documented here.
\format{
Two objects (data.frames) with two columns (time and counts).
} 

\source{
%%  ~~ If necessary, more details than the description above ~~
\tabular{ll}{
Lab: \tab Luminescence Laboratory Bayreuth\cr
Lab-Code: \tab BT900\cr
Location: \tab Norway\cr
Material: \tab beach deposit, coarse grain quartz measured on aluminum discs on a Risoe DA-15 reader.\cr
}
}


\references{
%% ~put references to the literature/web site here ~
Fuchs, M. et al., 2012. OSL and IRSL dating of raised beach sand deposits along the southeastern coast of Norway. Quaternary Geochronology, 10(0), pp.195-200.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##show LM data 
data(ExampleData.FittingLM)
plot(values.curve,log="x")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{IO}
\name{readBIN2R}
\alias{readBIN2R}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Import Risoe BIN-file into R}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Import a *.bin file produced by a Risoe DA15 and DA20 TL/OSL reader into R.
}
\usage{
readBIN2R(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
%%     ~~Describe \code{file} here~~
  \link{character} (\bold{required}): the bin-file name (including path), e.g. \cr
  [WIN]: \code{readBIN2R("C:/Desktop/test.bin")}, \cr  
  [MAC/LINUX]: \code{readBIN2R("/User/test/Desktop/test.bin")}
} 
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The binary data file is parsed byte per byte following the data structure published in the Appendices of 
the Analyst manual p. 42. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Return a S4 \link{Risoe.BINfileData-class} object containing two slots:

\item{METADATA}{A \link{data.frame} containing all variables stored in the bin-file.}
\item{DATA}{A \link{list} containing a numeric \link{vector} of the measured data. The ID corresponds to the record ID in METADATA}

}
\references{
%% ~put references to the literature/web site here ~
Duller, G. (2007). Analyst. 
}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
%%  ~~further notes~~
Function have been tested for BIN-file version 3 and 4. The version depends on used the Sequence Editor. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{readBin}}
}
\examples{
##
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
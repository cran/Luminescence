\name{writeR2BIN}
\alias{writeR2BIN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Export Risoe.BINfileData into Risoe BIN-file
}
\description{
Export a Risoe.BINfileData object in a *.bin or *.binx file that can be 
opened by the Analyst software or other Risoe software
}
\usage{
writeR2BIN(object, 
           file,
           version,
           txtProgressBar = TRUE)
}

\arguments{
  \item{object}{\code{\linkS4class{Risoe.BINfileData}} (\bold{required}): input object
  to be stored in a bin file.
%%     ~~Describe \code{object} here~~
}
  \item{file}{\code{\link{character}} (\bold{required}): file name and path of the 
  output file\cr
  [WIN]: \code{writeR2BIN(object, "C:/Desktop/test.bin")}, \cr  
  [MAC/LINUX]: \code{writeR2BIN("/User/test/Desktop/test.bin")}
  
%%     ~~Describe \code{file} here~~
}

\item{version}{\code{\link{raw}} (optional): version number for the output file.
 If no value is provided the highest version number from the \code{\linkS4class{Risoe.BINfileData}}
 is taken automatically.\cr
 Note: This argument can be used to convert BIN-file versions.

}
  



\item{txtProgressBar}{
  \link{logical} (with default): enables or disables \code{\link{txtProgressBar}}.} 


}
\details{

The structure of the exported binary data follows the data structure published in the Appendices of 
the Analyst manual p. 42.\cr

If  \code{LTYPE}, \code{DTYPE} and \code{LIGHTSOURCE} are not of type \code{\link{character}}
no transformation into numeric values takes places. 

}
\value{
Write a binary file.
}
\references{
Duller, G., 2007. Analyst. 
}
\author{Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany)
}

\note{
The function just check only roughly the data structures. The validity of the output
data depends on the user.\cr

The validity of the file path is not further checked. \cr

BIN-file conversions using the argument \code{version} may be a lossly conversion, 
depending on the chosen input and output data (e.g. conversion from version 06 to 04)
}

\section{Version}{0.1}


\seealso{

\code{\link{readBIN2R}}, \code{\linkS4class{Risoe.BINfileData}},
\code{\link{writeBin}}


}
\examples{
##uncomment for usage 

##data(ExampleData.BINfileData, envir = environment())
##writeR2BIN(CWOSL.SAR.Data, file="[your path]/output.bin")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}

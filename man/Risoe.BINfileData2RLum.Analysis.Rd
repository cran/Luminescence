\name{Risoe.BINfileData2RLum.Analysis}
\alias{Risoe.BINfileData2RLum.Analysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert Risoe.BINfileData object to an RLum.Analysis object
}
\description{
Convert values from one specific position of an Risoe.BINfileData S4-classe object
to an RLum.Analysis object
}
\usage{
Risoe.BINfileData2RLum.Analysis(object, 
                                pos, 
                                run, 
                                set, 
                                ltype = c("IRSL", "OSL", "TL", "RIR", "RBR","USER", "RL"),
                                protocol = "unknown")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{\linkS4class{Risoe.BINfileData}} (\bold{required}): Risoe.BINfileData object
}

  \item{pos}{\code{\link{integer}} (\bold{required}): position number of the Risoe.BINfileData
  object for which the curves should be stored in the RLum.Analysis object.
}
  \item{run}{\code{\link{vector}, \link{numeric}} (optional): run number from the measurement to limit
the converted data set (e.g. run = c(1:48)). 
}
  \item{set}{\code{\link{vector}, \link{numeric}} (optional): set number from the measurement to limit
the converted data set (e.g. set = c(1:48)). 
  
}
  \item{ltype}{\code{\link{vector}, \link{character}} (with default): curve type to limit 
  the converted data. Allowed values are: \code{IRSL}, \code{OSL}, \code{TL}, \code{RIR},  \code{RBR}
  and \code{USER}
}

\item{protocol}{\code{\link{character}} (optional): set protocol type for analysis 
object. Value may be used by subsequent analysis functions.  
}
}

\details{
The \code{\linkS4class{RLum.Analysis}} object requires a set of curves for specific 
further protocol analyses. However, the \code{\linkS4class{Risoe.BINfileData}} usually contains
a set of curves for different aliquots and different protocol types may mixed up.
Therefore, a conversion is needed. 

%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an \code{\linkS4class{RLum.Analysis}} object. 
}
\references{
#
}
\author{Sebastian Kreutzer, JLU Giessen/Freiberg Instruments (Germany)
}

\section{Version}{0.1 [2013-08-04]}
\note{
The \code{protocol} argument of the \code{\linkS4class{RLum.Analysis}} object is 
set to 'unknown' if not stated otherwise.  
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum.Analysis}}, 
\code{\link{readBIN2R}}

}
\examples{
##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

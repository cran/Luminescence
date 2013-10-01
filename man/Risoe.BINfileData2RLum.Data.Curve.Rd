\name{Risoe.BINfileData2RLum.Data.Curve}
\alias{Risoe.BINfileData2RLum.Data.Curve}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Converts an element from an Risoe.BINfileData object to an RLum.Data.Curve object
}
\description{
The function converts one specified single record from an Risoe.BINfileData object
to an RLum.Data.Curve object. 
}
\usage{
Risoe.BINfileData2RLum.Data.Curve(object, id, pos, run, set)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{\linkS4class{Risoe.BINfileData}} (\bold{required}): Risoe.BINfileData object
}

  \item{id}{\code{\link{integer}} (\bold{required}): record id in the \code{Risoe.BINfileData} object
  of the curve that should be stored in the \code{RLum.Data.Curve} object. If no value 
  for id is provided the record has to be specified by \code{pos}, \code{set} and \code{run}.
}

  \item{pos}{\code{\link{integer}} (optional): record position number in the \code{Risoe.BINfileData} object
  of the curve that should be stored in the \code{RLum.Data.Curve} object. If a value
  for \code{id} is provided this argument is ignored.
}
  \item{run}{\code{\link{integer}} (optional): record run number in the \code{Risoe.BINfileData} object
  of the curve that should be stored in the \code{RLum.Data.Curve} object. If a value
  for \code{id} is provided this argument is ignored.
}

  \item{set}{\code{\link{integer}} (optional): record set number in the \code{Risoe.BINfileData} object
  of the curve that should be stored in the \code{RLum.Data.Curve} object. If a value
  for \code{id} is provided this argument is ignored.
}
}
\details{
The function extracts all \code{METADATA} from the \code{Risoe.BINfileData} object and 
store them in the \code{RLum.Data.Curve} object.

}
\value{
Returns an \code{\linkS4class{RLum.Data.Curve}} object. 
}
\references{
#
}
\author{
Sebastian Kreutzer, JLU Giessen/Freiberg Instruments (Germany)
}

\section{Version}{0.1 [2013-03-27]}

\note{
The function is intended for experimental usage. Normally, the function 
\code{\link{Risoe.BINfileData2RLum.Analysis}} should be used for the conversion.
}


\seealso{
\code{\link{Risoe.BINfileData2RLum.Analysis}}, \code{\link{set_RLum.Data.Curve}},
\code{\linkS4class{RLum.Data.Curve}}, \code{\linkS4class{RLum.Analysis}}, 
\code{\linkS4class{Risoe.BINfileData}}, \code{\link{plot_RLum}}

}
\examples{
##get package example data
data(ExampleData.BINfileData, envir = environment())

##convert one record
Risoe.BINfileData2RLum.Data.Curve(CWOSL.SAR.Data, id=1)


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

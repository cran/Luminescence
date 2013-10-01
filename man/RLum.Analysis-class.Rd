\name{RLum.Analysis-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Analysis-class}
\alias{show,RLum.Analysis-method}
\alias{set_RLum.Analysis}
\alias{set_RLum.Analysis,RLum.Analysis-method}
\alias{set_RLum.Analysis,list-method}
\alias{get_RLum.Analysis}
\alias{get_RLum.Analysis-methods}
\alias{get_RLum.Analysis,RLum.Analysis-method}
\alias{get_structure.RLum.Analysis}
\alias{get_structure.RLum.Analysis,RLum.Analysis-method}


\title{Class \code{"RLum.Analysis"}}
\description{
Object class containing analysis data for protocol analysis.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Analysis", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{records}:}{Object of class \code{"list"} containing objects of class \code{\linkS4class{RLum.Data}}}
     \item{\code{protocol}:}{Object of class \code{"character"} describing the applied
     measurement protocol}
    
    
    \item{\code{.S3Class}:}{Object of class \code{"character"}}
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "RLum.Analysis")}: ... }
    \item{set_RLum.Analysis}{\code{signature(records = "list", protocol = "character")}: 
    
    Conctructor method for \code{RLum.Analysis} object. The slot \code{protocol} is optional 
    and predefined as \code{UNKNOWN} by default.    
    }
    
    \item{get_RLum.Analysis}{\code{signature(object = "RLum.Analysis", 
                                                       record.id = "numeric",
                                                       recordType = "character")}: 
    
    Accessor method for \code{RLum.Analysis} object. The slot \code{record.id} and \code{recordType} 
    are optional to allow for record limiting by their id (list index number) or their record type 
    (e.g. \code{OSL}).    
    }
    
    \item{get_structure.RLum.Analysis}{\code{signature(object = "RLum.Analysis")}: get meta structure
    of object as \code{\link{data.frame}}
   }
}}
   
\references{
#
}
\author{
Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany)
}

\section{Version}{0.1 [2013-03-27]}

\note{
The method \code{get_structure.RLum.Analysis} currently are just avaiblable for 
objects containing \code{\linkS4class{RLum.Data.Curve}} only.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Risoe.BINfileData2RLum.Analysis}}, 
\code{\linkS4class{Risoe.BINfileData}}, \code{\linkS4class{RLum}}
}

\examples{
showClass("RLum.Analysis")
}
\keyword{classes}

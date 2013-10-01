\name{RLum.Results-class}
\Rdversion{1.1}
\docType{class}
\alias{RLum.Results-class}
\alias{set_RLum.Results}
\alias{set_RLum.Results,RLum.Results-method}
\alias{set_RLum.Results,ANY,list-method}
\alias{get_RLum.Results}
\alias{get_RLum.Results,RLum.Results-method}
\alias{merge_RLum.Results}
\alias{merge_RLum.Results-methods}
\alias{merge_RLum.Results,list-method}
\alias{validObject,RLum.Results-method}


\title{Class \code{"RLum.Results"}}
\description{
Object class contains results data from functions.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLum.Results", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{originator}:}{Object of class \code{"character"} containing name of the producing function}
     \item{\code{data}:}{Object of class \code{"list"} containing output data}
    
    
    \item{\code{.S3Class}:}{Object of class \code{"character"}}
  }
}

\section{Methods}{
  \describe{
    \item{validObject}{\code{signature(object = "RLum.Results")}: validates object depending 
    on the \code{originator} argument}
    
    \item{set_RLum.Results}{\code{signature(originator = "character", data = "list")}: 
    
    Conctructor method for \code{RLum.Results} object. The slot \code{originator} is optional 
    and predefined as the function that calls the function \code{set_RLum.Results}.    
    }
    
     \item{get_RLum.Results}{\code{signature(object = "RLum.Results", data.object = "character")}: accessor method for \code{RLum.Results} object.
     The argument \code{data.object} allows to directly access objects delivered within the slot \code{data}. If no \code{data.object} is specified a preselected object is returned. The default return
     object depends on the object originator (e.g. \code{fit_LMCurve}). 
     }
     
      \item{merge_RLum.Results}{\code{signature(object.list = "list")}: merge method for \code{RLum.Results} objects.
     The argument \code{object.list} requires a list of \code{RLum.Results} objects.\cr
     Merging is doen by appending similar elements on the first object of the input list.
     }

   }
}


\references{
#
}

\author{Sebastian Kreutzer, 2013 (Freiberg Instruments/JLU Giessen, Germany)
}

\section{Version}{0.3 [2013-09-27]}


\note{
The class is intended to store results from functions to be used by other 
functions. The data in the object should always be accessed by the method
\code{get_RLum.Results}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{RLum}}
}

\examples{
showClass("RLum.Results")
}
\keyword{classes}
\keyword{methods}

\name{Risoe.BINfileData-class}
\Rdversion{1.1}
\docType{class}
\alias{Risoe.BINfileData-class}
\alias{show,Risoe.BINfileData-method}

\title{Class \code{"Risoe.BINfileData"}}
\description{
S4 class object for luminescence data in R. The object is produced by the 
function \link{readBIN2R}.

%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Risoe.BINfileData", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{METADATA}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{DATA}:}{Object of class \code{"list"} ~~ }
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "Risoe.BINfileData")}: ... }
	 }
}
\references{
%%  ~~put references to the literature/web site here~~
##
}
\author{
Sebastian Kreutzer, JLU Giessen/Germany
}
\note{
%%  ~~further notes~~
##
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
\code{\link{plot_BINfileData}}
}
\examples{
showClass("Risoe.BINfileData")
}
\keyword{classes}

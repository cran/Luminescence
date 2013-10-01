\name{plot_RLum.Analysis}
\alias{plot_RLum.Analysis}
 
\title{
Plot function for an RLum.Analysis S4 class object
}
\description{
The functions provides a standardized plot output for curve data of an RLum.Analysis S4 class object
}
\usage{
plot_RLum.Analysis(object, nrows = 3, ncols = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{\code{\linkS4class{RLum.Analysis}} (\bold{required}): S4 object of class \code{RLum.Analysis}
}

 \item{nrows}{\code{\link{integer}} (with default): set number of rows for plot output
}

  \item{ncols}{\code{\link{integer}} (with default): set number of columns for plot output
}

  \item{\dots}{further arguments and graphical parameters passed to the \code{plot} function
}

}
\details{
The function produces a multiple plot output. A file output is recommended (e.g. \code{\link{pdf}}). 

}
\value{
Returns multiple plots.
}

\references{
#
}

\author{Sebastian Kreutzer, Freiberg Instruments/JLU Giessen (Germany)
}

\section{Version}{0.1 [2013-01-27]}


\note{
Not all arguments available for \code{\link{plot}} will be passed!
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot}}, \code{\link{plot_RLum}}, \code{\link{plot_RLum.Data.Curve}}
}
\examples{
##load data
data(ExampleData.BINfileData, envir = environment())

##convert values for position 1
temp <- Risoe.BINfileData2RLum.Analysis(CWOSL.SAR.Data, pos=1)

##plot
plot_RLum.Analysis(temp)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}

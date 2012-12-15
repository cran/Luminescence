\name{Calc_OSLLxTxRatio}
\alias{Calc_OSLLxTxRatio}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Lx/Tx ratio for CW-OSL curves.}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Calculate Lx/Tx ratios from two given OSL curves. 

}
\usage{
Calc_OSLLxTxRatio(Lx.data,Tx.data,
                  signal.integral,
                  background.integral)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{Lx.data}{\link{data.frame} (\bold{required}): requires a CW-OSL shine down curve (x = time, y= counts)}
    \item{Tx.data}{\link{data.frame} (optional): requires a CW-OSL shine down curve (x = time, y = counts). If no input is given the Tx.data will 
    be treated as \code{NA} and no Lx/Tx ratio is calculated.}
    \item{signal.integral}{\link{vector} (\bold{required}): requires a vector with the limits for the signal integral.}
    \item{background.integral}{\link{vector} (\bold{required}): requires a vector with the bounds for the background integral.}
  
 }
\details{
%%  ~~ If necessary, more details than the description above ~~
The integrity of the chosen values for the signal and background integral is checked by the function; 
the signal integral limits have to be lower than the background integral limits. 
If a \link{vector} is given as input instead of a \link{data.frame}, an artificial \code{data.frame}
is produced. The error calculation is done according to Galbraith (2002).
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a \link{data.frame} with the following structure:\cr 

 $ LnLx  \cr        
 $ LnLx.BG   \cr     
 $ TnTx    \cr       
 $ TnTx.BG    \cr   
 $ Net_LnLx   \cr   
 $ Net_LnLx.Error\cr 
 $ Net_TnTx.Error

}
\references{
%% ~put references to the literature/web site here ~
Duller, G., 2007. Analyst.

Galbraith, R.F., 2002. A note on the variance of a background-corrected OSL count. Ancient TL, 20 (2), 49-51.
}
\author{Sebastian Kreutzer, JLU Giessen, Germany}
\note{
%%  ~~further notes~~
The results of this function have been cross-checked with the Analyst (vers. 3.24b).
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~

\code{\link{Analyse_SAR.OSLdata}}, \code{\link{plot_GrowthCurve}}
}
\examples{
##load data
data(ExampleData.LxTxOSLData)

##calculate Lx/Tx ratio
results<-Calc_OSLLxTxRatio(Lx.data,Tx.data, signal.integral = c(1:2), background.integral = c(85:100))
print(results)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}

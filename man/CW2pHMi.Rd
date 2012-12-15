\name{CW2pHMi}
\alias{CW2pHMi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transform a CW-OSL curve into a pHM-OSL curve via interpolation under hyperbolic modulation conditions
}
\description{
This function transforms a conventionally measured continuous-wave (CW) OSL-curve to a pseudo hyperbolic modulated (pHM)
curve under hyperbolic modulation conditions using the interpolation procedure described by Bos & Wallinga (2012).
}
\usage{
CW2pHMi(values, delta)
}
\arguments{
  \item{values}{\code{\link{data.frame}} (\bold{required}): measured curve data of type stimulation time (t)
  (\code{values[,1]}) and measured counts (cts) (\code{values[,2]})}

  \item{delta}{\code{\link{vector}} (optional): stimulation rate parameter, if no value is given, the optimal value is estimated automatically (see details). Smaller values of delta produce more points in the rising tail of the curve.}
}
\details{
The complete procedure of the transformation is described in Bos & Wallinga (2012). 
The input \code{data.frame} consists of two columns: time (t) and count values (CW(t))\cr

\bold{Internal transformation steps}

(1) log(CW-OSL) values

(2) Calculate t' which is the transformed time:

\deqn{t' = t-(1/delta)*log(1+delta*t)}

(3) Interpolate CW(t'), i.e. use the log(CW(t)) to obtain the count values for the transformed time (t').
Values beyond \code{min(t)} and \code{max(t)} produce \code{NA} values.

(4) Select all values for t' < \code{min(t)}, i.e. values beyond the time resolution of t. 
Select the first two values of the transformed data set which contain no \code{NA} values and use
these values for a linear fit using \code{\link{lm}}.

(5) Extrapolate values for t' < \code{min(t)} based on the previously obtained fit parameters.

(6) Transform values using 

\deqn{pHM(t) = (delta*t/(1+delta*t))*c*CW(t')}
\deqn{c = (1+delta*P)/delta*P}
\deqn{P = length(stimulation~period)}

(7) Combine all values and truncate all values for t' > \code{max(t)} \cr

\emph{The number of values for t' < \code{min(t)} depends on the stimulation rate parameter
\code{delta}. To avoid the production of too many artificial data at the raising tail of the determined pHM curve, it is recommended to use the automatic estimation routine for \code{delta}, i.e. provide no value for \code{delta}.}

}
\value{
Returns a \code{\link{data.frame}} with three columns:

 \tabular{rl}{

$x \tab:   time\cr
$y.t \tab:   transformed count values\cr
$x.t  \tab: transformed time values \cr
$method \tab: used method for the production of the new data points
}
}

\references{
Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL signal components. Radiation Measurements 47, 752-758.

\bold{Further}

Bulur, E., 1996. An Alternative Technique For Optically Stimulated Luminescence (OSL) Experiment. Radiation Measurements, 26, 701-709.

Bulur, E., 2000. A simple transformation for converting CW-OSL curves to LM-OSL curves. Radiation Measurements, 32, 141-145.

}
\author{Sebastian Kreutzer, JLU Giessen, Germany, 2012

Based on comments and suggestions from:\cr
Adrie J.J. Bos, Delft University of Technology, The Netherlands
}
\note{
According to Bos & Wallinga (2012), the number of extrapolated points should be limited to avoid 
artificial intensity data. If \code{delta} is provided manually and more than two points are extrapolated, 
a warning message is returned. 

The function \code{\link{approx}} may produce some \code{Inf} and \code{NaN} data. The function tries to 
manually interpolate these values by calculating the \code{mean} using the adjacent channels. If
two invalid values are succeeding, the values are removed and no further interpolation is attempted.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{CW2pLM}}, \code{\link{CW2pLMi}}, \code{\link{CW2pPMi}}, \code{\link{fit_LMCurve}}, \code{\link{lm}}
}
\examples{
##(1) - simple transformation

##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve)

##transform values
values.transformed<-CW2pHMi(ExampleData.CW_OSL_Curve)

##plot
plot(values.transformed$x, values.transformed$y.t, log = "x")

##(2) - load CW-OSL curve from BIN-file and plot transformed values

##load BINfile
#BINfileData<-readBIN2R("[path to BIN-file]")
data(ExampleData.BINfileData)

##grep first CW-OSL curve from ALQ 1

  curve.ID<-BINfileData@METADATA[BINfileData@METADATA[,"LTYPE"]=="OSL" &
                                 BINfileData@METADATA[,"POSITION"]==1
                                 ,"ID"]

  curve.HIGH<-BINfileData@METADATA[BINfileData@METADATA[,"ID"]==curve.ID[1] 
                                   ,"HIGH"]

  curve.NPOINTS<-BINfileData@METADATA[BINfileData@METADATA[,"ID"]==curve.ID[1] 
                                      ,"NPOINTS"]

##combine curve to data set

  curve<-data.frame(x = seq(curve.HIGH/curve.NPOINTS,curve.HIGH,
                            by = curve.HIGH/curve.NPOINTS),
                    y=unlist(BINfileData@DATA[curve.ID[1]])                    
                    )

##transform values 
  
  curve.transformed<-CW2pHMi(curve)

##plot curve
  plot(curve.transformed$x, curve.transformed$y.t, log="x")
  
  
##(3) - produce Fig. 4 from Bos & Wallinga (2012)

##load data
data(ExampleData.CW_OSL_Curve)
values<-CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim=c(0.001,10),
     ylim=c(0,8000),
     ylab="pseudo OSL (cts/0.01 s)",
     xlab="t [s]",
     log="x",
     main="Fig. 4 - Bos & Wallinga (2012)", 
     )

values.t<-CW2pLMi(values, P=1/20)
lines(values[1:length(values.t[,1]),1],CW2pLMi(values, P=1/20)[,2], col="red" ,lwd=1.3)
text(0.03,4500,"LM", col="red" ,cex=.8)

values.t<-CW2pHMi(values, delta=40)
lines(values[1:length(values.t[,1]),1],CW2pHMi(values, delta=40)[,2], col="black", lwd=1.3)
text(0.005,3000,"HM", cex=.8)
 
values.t<-CW2pPMi(values, P=1/10)
lines(values[1:length(values.t[,1]),1],CW2pPMi(values, P=1/10)[,2], col="blue", lwd=1.3)
text(0.5,6500,"PM", col="blue" ,cex=.8)
  
  

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{manip}

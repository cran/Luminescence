% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_CW2pHMi.R
\name{convert_CW2pHMi}
\alias{convert_CW2pHMi}
\alias{CW2pHMi}
\title{Transform a CW-OSL curve into a pHM-OSL curve via interpolation under
hyperbolic modulation conditions}
\usage{
convert_CW2pHMi(values, delta)

CW2pHMi(values, delta)
}
\arguments{
\item{values}{\linkS4class{RLum.Data.Curve} or \link{data.frame} (\strong{required}):
\linkS4class{RLum.Data.Curve} or \link{data.frame} with measured curve data of type
stimulation time (t) (\code{values[,1]}) and measured counts (cts) (\code{values[,2]}).}

\item{delta}{\link{vector} (\emph{optional}):
stimulation rate parameter, if no value is given, the optimal value is
estimated automatically (see details). Smaller values of delta produce more
points in the rising tail of
the curve.}
}
\value{
The function returns the same data type as the input data type with
the transformed curve values.

\strong{\code{RLum.Data.Curve}}

\tabular{ll}{
\verb{$CW2pHMi.x.t} \tab: transformed time values \cr
\verb{$CW2pHMi.method} \tab: used method for the production of the new data points
}

\strong{\code{data.frame}}

\tabular{ll}{
\verb{$x} \tab: time\cr
\verb{$y.t} \tab: transformed count values\cr
\verb{$x.t} \tab: transformed time values \cr
\verb{$method} \tab: used method for the production of the new data points
}
}
\description{
This function transforms a conventionally measured continuous-wave (CW)
OSL-curve to a pseudo hyperbolic modulated (pHM) curve under hyperbolic
modulation conditions using the interpolation procedure described by Bos &
Wallinga (2012).
}
\details{
The complete procedure of the transformation is described in Bos & Wallinga
(2012). The input \code{data.frame} consists of two columns: time (t) and
count values (CW(t))

\strong{Internal transformation steps}

(1) log(CW-OSL) values

(2)
Calculate t' which is the transformed time:
\deqn{t' = t-(1/\delta)*log(1+\delta*t)}

(3)
Interpolate CW(t'), i.e. use the log(CW(t)) to obtain the count values
for the transformed time (t'). Values beyond \code{min(t)} and \code{max(t)}
produce \code{NA} values.

(4)
Select all values for t' < \code{min(t)}, i.e. values beyond the time
resolution of t. Select the first two values of the transformed data set
which contain no \code{NA} values and use these values for a linear fit
using \link{lm}.

(5)
Extrapolate values for t' < \code{min(t)} based on the previously
obtained fit parameters.

(6)
Transform values using
\deqn{pHM(t) = (\delta*t/(1+\delta*t))*c*CW(t')}
\deqn{c = (1+\delta*P)/\delta*P}
\deqn{P = length(stimulation~period)}

(7) Combine all values and truncate all values for t' > \code{max(t)}

\strong{NOTE:}
The number of values for t' < \code{min(t)} depends on the stimulation rate
parameter \code{delta}. To avoid the production of too many artificial data
at the raising tail of the determined pHM curve, it is recommended to use
the automatic estimation routine for \code{delta}, i.e. provide no value for
\code{delta}.
}
\note{
According to Bos & Wallinga (2012), the number of extrapolated points
should be limited to avoid artificial intensity data. If \code{delta} is
provided manually and more than two points are extrapolated, a warning
message is returned.

The function \link{approx} may produce some \code{Inf} and \code{NaN} data.
The function tries to manually interpolate these values by calculating
the \code{mean} using the adjacent channels. If two invalid values are succeeding,
the values are removed and no further interpolation is attempted.
In every case a warning message is shown.
}
\section{Function version}{
 0.2.3
}

\examples{

##(1) - simple transformation

##load CW-OSL curve data
data(ExampleData.CW_OSL_Curve, envir = environment())

##transform values
values.transformed <- convert_CW2pHMi(ExampleData.CW_OSL_Curve)

##plot
plot(values.transformed$x, values.transformed$y.t, log = "x")

##(2) - load CW-OSL curve from BIN-file and plot transformed values

##load BINfile
#BINfileData<-readBIN2R("[path to BIN-file]")
data(ExampleData.BINfileData, envir = environment())

##grep first CW-OSL curve from ALQ 1
curve.ID<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"LTYPE"]=="OSL" &
                                    CWOSL.SAR.Data@METADATA[,"POSITION"]==1
                                  ,"ID"]

curve.HIGH<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"ID"]==curve.ID[1]
                                    ,"HIGH"]

curve.NPOINTS<-CWOSL.SAR.Data@METADATA[CWOSL.SAR.Data@METADATA[,"ID"]==curve.ID[1]
                                       ,"NPOINTS"]

##combine curve to data set

curve<-data.frame(x = seq(curve.HIGH/curve.NPOINTS,curve.HIGH,
                          by = curve.HIGH/curve.NPOINTS),
                  y=unlist(CWOSL.SAR.Data@DATA[curve.ID[1]]))


##transform values

curve.transformed <- convert_CW2pHMi(curve)

##plot curve
plot(curve.transformed$x, curve.transformed$y.t, log = "x")


##(3) - produce Fig. 4 from Bos & Wallinga (2012)

##load data
data(ExampleData.CW_OSL_Curve, envir = environment())
values <- CW_Curve.BosWallinga2012

##open plot area
plot(NA, NA,
     xlim=c(0.001,10),
     ylim=c(0,8000),
     ylab="pseudo OSL (cts/0.01 s)",
     xlab="t [s]",
     log="x",
     main="Fig. 4 - Bos & Wallinga (2012)")

values.t <- convert_CW2pLMi(values, P = 1/20)
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col="red" ,lwd=1.3)
text(0.03,4500,"LM", col="red" ,cex=.8)

values.t <- convert_CW2pHMi(values, delta = 40)
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col="black", lwd=1.3)
text(0.005,3000,"HM", cex=.8)

values.t <- convert_CW2pPMi(values, P = 1/10)
lines(values[1:length(values.t[, 1]), 1], values.t[, 2],
      col="blue", lwd=1.3)
text(0.5,6500,"PM", col="blue" ,cex=.8)

} 

\section{How to cite}{
Kreutzer, S., 2025. convert_CW2pHMi(): Transform a CW-OSL curve into a pHM-OSL curve via interpolation under hyperbolic modulation conditions. Function version 0.2.3. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Bos, A.J.J. & Wallinga, J., 2012. How to visualize quartz OSL
signal components. Radiation Measurements, 47, 752-758.\cr

\strong{Further Reading}

Bulur, E., 1996. An Alternative Technique For
Optically Stimulated Luminescence (OSL) Experiment. Radiation Measurements,
26, 701-709.

Bulur, E., 2000. A simple transformation for converting CW-OSL curves to
LM-OSL curves. Radiation Measurements, 32, 141-145.
}
\seealso{
\link{convert_CW2pLM}, \link{convert_CW2pLMi}, \link{convert_CW2pPMi},
\link{fit_LMCurve}, \link{lm}, \linkS4class{RLum.Data.Curve}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Based on comments and suggestions from:\cr
Adrie J.J. Bos, Delft University of Technology, The Netherlands
, RLum Developer Team}
\keyword{manip}

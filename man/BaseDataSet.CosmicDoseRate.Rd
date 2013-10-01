\name{BaseDataSet.CosmicDoseRate}
\alias{BaseDataSet.CosmicDoseRate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Base data set for cosmic dose rate calculation}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Collection of data from various sources needed for cosmic dose rate calculation 
}
\usage{
BaseDataSet.CosmicDoseRate
}
%- maybe also 'usage' for other objects documented here.
\format{


\code{values.cosmic.Softcomp}: \cr 
data frame containing cosmic dose rates for 
shallow depths (0-1 m) obtained using the "AGE" program by Rainer Gruen (Gruen 2009). 
These data essentially reproduce the graph shown in Fig. 1 of Prescott & Hutton (1988). \cr

\code{values.factor.Altitude}: \cr 
data frame containing altitude factors for adjusting geomagnetic field-change factors. 
Values were read from Fig. 1 in Prescott & Hutton (1994). \cr

\code{values.par.FJH}: \cr 
data frame containing values for parameters F, J and H 
(read off from Fig. 2 in Prescott & Hutton 1994) used in the expression 


\deqn{D_{c} = D_{0}*(F+J*exp((altitude/1000)/H))}
} 

\source{
The following data were carefully read from figures in mentioned sources and used for fitting procedures. The derived expressions are used in the function \code{\link{calc_CosmicDoseRate}}.

\bold{values.cosmic.Softcomp}

\tabular{ll}{

Program: \tab "AGE"\cr
Reference: \tab Gruen (2009) \cr
Fit: \tab 6-degree polynomial in the form of

}

\deqn{y = 0.6926*x^6 - 2.6189*x^5 + 4.1617*x^4 - 3.6374*x^3 + 1.9109*x^2 - 0.6258*x + 0.2999}


\bold{values.factor.Altitude}

\tabular{ll}{

Reference: \tab Prescott & Hutton (1994) \cr
Page: \tab 499 \cr
Figure: \tab 1 \cr
Fit: \tab 2-degree polynomial in the form of

}

\deqn{y = -0.026*x^2 + 0.6628*x + 1.0435}


\bold{values.par.FJH}

\tabular{ll}{

Reference: \tab Prescott & Hutton (1994) \cr
Page: \tab 500  \cr
Figure: \tab 2 \cr
Fits: \tab 6-degree polynomials and linear fits

}

\deqn{F (non-linear-part, lambda < 36.5 deg.)}

\deqn{y = 0.6926*x^6 - 2.6189*x^5 + 4.1617*x^4 - 
3.6374*x^3 + 1.9109*x^2 - 0.6258*x + 0.2999}

\deqn{F (linear-part, lambda > 36.5 deg.)}

\deqn{y = -0.0001*x + 0.2347}

\deqn{J (non-linear-part, lambda < 34 deg.)}

\deqn{y = -2*10^-10*x^6 + 1*10^-8*x^5 + 4*10^-7*x^4 -
3*10^-5*x^3 + 0.0006*x^2 - 0.0013*x + 0.52}

\deqn{J (linear-part, lambda > 34 deg.)}

\deqn{y = 0.0005*x + 0.7388}

\deqn{H (non-linear-part, lambda < 36 deg.)}

\deqn{y = 1*10^-9*x^6 - 8*10^-8*x^5 + 2*10^-6*x^4 - 
3*10^-6*x^3 - 0.0002*x^2 - 0.0025*x + 4.399}

\deqn{H (linear-part, lambda > 36 deg.)}

\deqn{y = 0.0002*x + 4.0914}




}

\references{
%% ~put references to the literature/web site here ~
Gruen, R., 2009. The "AGE" program for the calculation of luminescence age estimates. Ancient TL, 27, pp. 45-46.

Prescott, J.R., Hutton, J.T., 1988. Cosmic ray and gamma ray dosimetry for TL and ESR. Nuclear Tracks and Radiation Measurements, 14, pp. 223-227.

Prescott, J.R., Hutton, J.T., 1994. Cosmic ray contributions to dose rates for luminescence and ESR dating: large depths and long-term time variations. Radiation Measurements, 23, pp. 497-500.
}
\section{Version}{0.1}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
##load data
data(BaseDataSet.CosmicDoseRate, envir = environment())
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datasets}

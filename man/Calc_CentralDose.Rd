\name{Calc_CentralDose}
\alias{Calc_CentralDose}
\title{
Apply the central age model (CAM) after Galbraith et al. (1999) to a given De distribution
}
\description{
This function calculates the central dose and dispersion of the De distribution, their standard errors and the profile log likelihood function for sigma.
}
\usage{
Calc_CentralDose(input.data,
                 sigmab = 0,
                 sample.id = "unknown sample",
                 print.iterations = FALSE,
                 output.plot = TRUE)
}
\arguments{
  \item{input.data}{\link{data.frame} (\bold{required}): two column data frame with De values and corresponding De errors
}
  \item{sigmab}{\link{numeric} (with default): spread in De values given as a fraction (e.g. 0.2). This value represents the expected overdispersion in the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).
}
  \item{sample.id}{\link{character} (with default): sample id
}
  \item{print.iterations}{\link{logical} (with default): terminal output of calculation iterations

}
  \item{output.plot}{\link{logical} (with default): plot output

}
}
\details{
This function uses the equations of Galbraith et al. (1999, pp. 358-359). The parameter \code{sigma} is estimated using the maximum likelihood approach. A detailed explanation on maximum likelihood estimation can be found in the appendix of Galbraith & Laslett (1993, pp. 468-470)
}
\value{
Returns a plot and terminal output. In addition a list is returned containing the following element:

\item{results}{data frame with statistical parameters.}
}
\references{
Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470.

Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 1999. Optical dating of single grains of quartz from Jinmium rock shelter, northern Australia. Part I: experimental design and statistical models. Archaeometry, 41, pp. 339-364. 

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology, 11, pp. 1-27.

\bold{Further reading}

Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain equivalent dose (De) distributions: Implications for OSL dating of sediment mixtures. Quaternary Geochronology, 4, pp. 204-230.

Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain quartz De distributions and an assessment of procedures for estimating burial dose. Quaternary Science Reviews, 25, pp. 2475-2502.

Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial archives using robust OSL chronologies. Quaternary Geochronology, 12, pp. 98-106.

Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the reproducibility and accuracy of optical dating of fluvial deposits. Quaternary Geochronology, 1, pp. 109-120. 

Rodnight, H., 2008. How many equivalent dose values are needed to obtain a reproducible distribution?. Ancient TL, 26, pp. 3-10.
}
\author{
Original S script: Rex Galbraith, University College London (UK), 2010

Revised R script: Christoph Burow, University of Cologne (Germany), 2012
}
\seealso{
  \link{plot},
  \link{Calc_CommonDose}, 
  \link{Calc_FiniteMixture},
  \link{Calc_FuchsLang2001},
  \link{Calc_MinDose3},
  \link{Calc_MinDose4}
}
\examples{
## load example data
data(ExampleData.DeValues)

## apply the central dose model
Calc_CentralDose(ExampleData.DeValues)
}
